/*
================================================================
fast_speedtest.js — Quantumult X 用 fast.com 测当前节点下载 + 上传
@Author: Tuesday
@Date: 2026-11-01 (v10: 逻辑同 v4，界面对齐排版：表格两栏，标签列等宽，数值列对齐，行距舒适)
@Description:
fast.com 网页版的算法（从 fast.com/app-*.js 实测）：
duration min 5s / max 30s，连接数随速度自适应到 8，
25 MB 级 range 请求连续下载，稳定移动平均后取最大
速度；上传是向同一个 /speedtest/range/0-<size> 地址
POST application/octet-stream 数据，同样连续多轮。
网页版在用户截图里下了 670 MB 才报 260 Mbps。
QX 的 $task.fetch 只支持文本 body、单请求默认 10 秒，
不能像浏览器那样在同一连接上流式复用，所以 v4 用
等效做法：大包 + 多轮 + 扣连接建立时间，流量不限。
- 下载：5 MiB 热身估速 → 按估速把每轮调到约 3 秒
（每连接 5~15 MiB，4 并发）→ 3 轮主测，取最快
一轮；每轮失败自动减半重试一次。
- 上传：向同一 OCA 地址 POST 同样大小的数据，
1 MiB 热身 → 2 轮主测（每连接 1~5 MiB），取最快
一轮；上传失败不影响下载结果，显示为 —。
- 延迟：1 字节 range 连打 5 次取最小值。
界面按截图样式：左对齐纯文本排版、粗体标签、空行分组、结论区；逻辑与 v4 完全一致，不追速度。
细节（各轮、策略、出口 IP）只写 console.log。


- 一次约 100~300 MB 下载 + 10~40 MB 上传，流量不限
才用，别挂 cron。
- 出口 IP 只写日志，不进弹窗（IPv6 太长，界面难看）。
- 和网页版仍不会完全相等：QX 文本 body 解码开销计入
时间，且同一时刻拥塞不同；看相对变化更靠谱。

手动点测（换成新 commit 固定地址可绕过
QX 24 小时远程缓存）：
[task_local]
event-interaction https://cdn.jsdelivr.net/gh/Youglas/anyouglas@master/fast_speedtest.js, tag=Fast测速, img-url=bolt.square.fill.system, enabled=true
================================================================
*/

const NAME = 'Fast.com 测速';
const UA = 'Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Mobile/15E148 Safari/604.1';
const FALLBACK_TOKEN = 'YXNkZmFzZGxmbnNkYWZoYXNkZmhrYWxm';
const CONCURRENCY = 4;
const WARMUP_BYTES = 5 * 1048576;
const MIN_BYTES = 5 * 1048576;
const MAX_BYTES = 15 * 1048576;
const ROUND_TARGET_SEC = 3.0;
const UP_TARGET_SEC = 2.0;
const MAIN_ROUNDS = 3;
const UP_MIN_BYTES = 1048576;
const UP_MAX_BYTES = 5 * 1048576;
const UP_ROUNDS = 2;
const REQ_TIMEOUT_MS = 20000;
const API_TIMEOUT_MS = 8000;

function log(msg) {
if (typeof console!== 'undefined' && console.log) console.log('[' + NAME + '] ' + msg);
}

function selectedPolicy() {
try {
if (typeof $environment!== 'undefined' && $environment && typeof $environment.params === 'string' && $environment.params) {
return $environment.params;
}
} catch (e) { /* ignore */}
return null;
}

function normalizeHeaders(headers) {
const out = {};
if (!headers) return out;
Object.keys(headers).forEach(function (k) { out[String(k).toLowerCase()] = headers[k];});
return out;
}

function countedBytes(headers, body) {
const n = parseInt(headers['content-length'], 10);
if (!isNaN(n) && n > 0) return n;
if (body) return body.length;
return 0;
}

// 统一请求：GET 用 $task.fetch / $httpClient；POST 多传 method + body
function httpRequest(url, timeoutMs, opts) {
opts = opts || {};
const method = opts.method || 'GET';
const body = opts.body || null;
const start = Date.now();
return new Promise(function (resolve) {
let settled = false;
const timer = setTimeout(function () {
if (!settled) {
settled = true;
resolve({ ok: false, error: 'timeout ' + timeoutMs + 'ms', status: 0, headers: {}, body: '', ms: Date.now() - start});
}
}, timeoutMs);
function finish(res) {
if (settled) return;
settled = true;
clearTimeout(timer);
res.ms = Date.now() - start;
resolve(res);
}
try {
if (typeof $task!== 'undefined' && $task.fetch) {
const req = { url: url, method: method, headers: { 'User-Agent': UA, 'Accept': '*/*'}, timeout: timeoutMs};
if (method === 'POST') req.headers['Content-Type'] = 'application/octet-stream';
if (body!== null) req.body = body;
const policy = selectedPolicy();
if (policy) req.opts = { policy: policy};
$task.fetch(req).then(function (resp) {
const headers = normalizeHeaders(resp.headers || {});
finish({ ok: resp.statusCode >= 200 && resp.statusCode < 300, status: resp.statusCode, headers: headers, body: resp.body || ''});
}).catch(function (e) {
finish({ ok: false, error: String((e && (e.error || e.message)) || e), status: 0, headers: {}, body: ''});
});
} else if (typeof $httpClient!== 'undefined' && $httpClient.get) {
const reqObj = { url: url, headers: { 'User-Agent': UA, 'Accept': '*/*'}};
if (method === 'POST') reqObj.headers['Content-Type'] = 'application/octet-stream';
if (body!== null) reqObj.body = body;
const fn = method === 'POST'? $httpClient.post: $httpClient.get;
fn.call($httpClient, reqObj, function (err, resp, respBody) {
if (err) {
finish({ ok: false, error: String(err), status: 0, headers: {}, body: ''});
} else {
const headers = normalizeHeaders((resp && resp.headers) || {});
const status = (resp && (resp.status || resp.statusCode)) || 0;
finish({ ok: status >= 200 && status < 300, status: status, headers: headers, body: respBody || ''});
}
});
} else {
finish({ ok: false, error: 'no http client ($task/$httpClient)', status: 0, headers: {}, body: ''});
}
} catch (e) {
finish({ ok: false, error: String((e && e.message) || e), status: 0, headers: {}, body: ''});
}
});
}

function httpGet(url, timeoutMs) {
return httpRequest(url, timeoutMs, { method: 'GET'});
}

async function fetchToken() {
const home = await httpGet('https://fast.com/', API_TIMEOUT_MS);
if (home.ok && home.body) {
const m = home.body.match(/app-[A-Za-z0-9_-]+\.js/);
if (m) {
const js = await httpGet('https://fast.com/' + m[0], API_TIMEOUT_MS);
if (js.ok && js.body) {
const t = js.body.match(/token\s*:\s*"([A-Za-z0-9+\/=]+)"/);
if (t && t[1]) return t[1];
}
}
}
return FALLBACK_TOKEN;
}

async function fetchTargets(token) {
const url = 'https://api.fast.com/netflix/speedtest/v2?https=true&token=' + encodeURIComponent(token) + '&urlCount=' + CONCURRENCY;
const res = await httpGet(url, API_TIMEOUT_MS);
if (!res.ok) throw new Error('fast.com API 请求失败，HTTP ' + res.status + ' / ' + (res.error || ''));
let data;
try { data = JSON.parse(res.body);} catch (e) { throw new Error('fast.com API 返回解析失败');}
if (data && data.message) throw new Error('fast.com API：' + data.message);
const targets = (data && data.targets || []).map(function (t) { return t.url;}).filter(Boolean);
if (!targets.length) throw new Error('fast.com 没有返回测速节点');
return { client: data.client || {}, targets: targets};
}

function rangeUrl(baseUrl, sizeBytes) {
const withRange = baseUrl.replace('speedtest', 'speedtest/range/');
return withRange.replace('/range/', '/range/0-' + (sizeBytes - 1));
}

function mbps(bytes, elapsedMs) {
if (!elapsedMs || elapsedMs <= 0) return 0;
return (bytes * 8) / (elapsedMs / 1000) / 1e6;
}

function correctedMbps(bytes, elapsedMs, setupMs) {
const corrected = Math.max(elapsedMs - (setupMs || 0), 50);
return mbps(bytes, corrected);
}

async function downloadRound(urls, sizeBytes, timeoutMs) {
const start = Date.now();
const results = await Promise.all(urls.map(function (u) { return httpGet(rangeUrl(u, sizeBytes), timeoutMs);}));
const elapsedMs = Date.now() - start;
let totalBytes = 0;
let okCount = 0;
const errors = [];
results.forEach(function (r) {
if (r.ok) { okCount++; totalBytes += countedBytes(r.headers, r.body);}
else if (r.error || r.status) errors.push('HTTP ' + r.status + ' ' + (r.error || ''));
});
return { totalBytes: totalBytes, elapsedMs: elapsedMs, okCount: okCount, total: results.length, errors: errors};
}

// 一轮失败就减半重试一次（大包在慢节点/内存吃紧时可能超时）
async function downloadRoundWithFallback(urls, sizeBytes, timeoutMs) {
let size = sizeBytes;
let round = await downloadRound(urls, size, timeoutMs);
if ((!round.totalBytes ||!round.okCount) && size > MIN_BYTES) {
size = Math.max(MIN_BYTES, Math.floor(size / 2));
log('download round failed, retry at ' + (size / 1048576).toFixed(1) + ' MiB');
round = await downloadRound(urls, size, timeoutMs);
}
round.sizeBytes = size;
return round;
}

let uploadBodyCache = { size: 0, body: ''};
function uploadBody(sizeBytes) {
if (uploadBodyCache.size!== sizeBytes) {
uploadBodyCache = { size: sizeBytes, body: 'a'.repeat(sizeBytes)};
}
return uploadBodyCache.body;
}

async function uploadRound(urls, sizeBytes, timeoutMs) {
const body = uploadBody(sizeBytes);
const start = Date.now();
const results = await Promise.all(urls.map(function (u) {
return httpRequest(rangeUrl(u, sizeBytes), timeoutMs, { method: 'POST', body: body});
}));
const elapsedMs = Date.now() - start;
let okCount = 0;
const errors = [];
results.forEach(function (r) {
if (r.ok) okCount++;
else if (r.error || r.status) errors.push('HTTP ' + r.status + ' ' + (r.error || ''));
});
// 上传字节按发出的数据计，响应体本身是 0 字节
return { totalBytes: sizeBytes * okCount, elapsedMs: elapsedMs, okCount: okCount, total: results.length, errors: errors};
}

async function uploadRoundWithFallback(urls, sizeBytes, timeoutMs) {
let size = sizeBytes;
let round = await uploadRound(urls, size, timeoutMs);
if ((!round.totalBytes ||!round.okCount) && size > UP_MIN_BYTES) {
size = Math.max(UP_MIN_BYTES, Math.floor(size / 2));
log('upload round failed, retry at ' + (size / 1048576).toFixed(1) + ' MiB');
round = await uploadRound(urls, size, timeoutMs);
}
round.sizeBytes = size;
return round;
}

function buildPanel(download, upload, latency, locationText, footerText, policy) {
var verdict = download >= 200 ? '✅ 速度良好' : (download >= 50 ? '✅ 速度一般' : '⚠️ 速度偏慢');
var nodeLine = policy ? policy : locationText;
var fparts = footerText.split(' · ');
var plain = '下载: ' + download.toFixed(0) + ' Mbps\n'
+ '上传: ' + (upload!== null? upload.toFixed(0) + ' Mbps': '—') + '\n'
+ '延迟: ' + (latency!== null? latency + ' ms': '—') + '\n'
+ '出口: ' + locationText + '\n'
+ '用时: ' + (fparts[0] ? fparts[0].replace('用时 ', '') : '') + '\n'
+ '流量: ' + (fparts[1] ? fparts[1].replace('流量 ', '') : '') + '\n\n'
+ '📋 测速结论\n\n'
+ verdict + '\n\n'
+ '节点: ' + nodeLine;
function row(label, value, boldValue) {
return '<tr><td style="width:68px; padding:7px 0; vertical-align:top"><b>' + label + ':</b></td><td style="padding:7px 0; vertical-align:top">' + (boldValue ? '<b>' + value + '</b>' : value) + '</td></tr>';
}
var html = '<table style="width:100%; border-collapse:collapse; font-family:-apple-system; font-size:16px; line-height:1.5">'
+ row('下载', download.toFixed(0) + ' Mbps', true)
+ row('上传', upload!== null? upload.toFixed(0) + ' Mbps': '—', true)
+ row('延迟', latency!== null? latency + ' ms': '—', false)
+ row('出口', locationText, false)
+ row('用时', fparts[0] ? fparts[0].replace('用时 ', '') : '', false)
+ row('流量', fparts[1] ? fparts[1].replace('流量 ', '') : '', false)
+ '<tr><td colspan="2" style="padding:16px 0 4px">📋 <b>测速结论</b></td></tr>'
+ '<tr><td colspan="2" style="padding:4px 0">' + verdict + '</td></tr>'
+ '<tr><td style="width:68px; padding:10px 0 2px; vertical-align:top"><b>节点:</b></td><td style="padding:10px 0 2px; vertical-align:top"><font color="#0A84FF">' + nodeLine + '</font></td></tr>'
+ '</table>';
return { plain: plain, html: html };
}
function showResult(title, plain, html) {
log(title + ' | ' + plain);
if (typeof $done!== 'undefined') {
try { $done({ title: title, message: plain, htmlMessage: html }); }
catch (e) { try { $done(); } catch (e2) { /* ignore */ } }
}
}

async function main() {
const startAll = Date.now();
try {
let token = await fetchToken();
let fetched;
try {
fetched = await fetchTargets(token);
} catch (e) {
if (String(e.message).indexOf('Unknown') > -1 && token!== FALLBACK_TOKEN) {
fetched = await fetchTargets(FALLBACK_TOKEN);
} else {
throw e;
}
}
const targets = fetched.targets.slice(0, CONCURRENCY);
const client = fetched.client || {};
const exitIp = client.ip || '';
const locationText = client.location? [client.location.city, client.location.country].filter(Boolean).join(', '): (exitIp || '未知出口');
const policy = selectedPolicy();
log('exit=' + exitIp + ' loc=' + locationText + ' policy=' + (policy || '-'));

// 延迟：1 字节连打 5 次取最小
const latencies = [];
for (let i = 0; i < 5; i++) {
const r = await httpGet(rangeUrl(targets[0], 1), 5000);
if (r.ok) latencies.push(r.ms);
}
const latency = latencies.length? Math.min.apply(null, latencies): null;
log('latency probes=' + JSON.stringify(latencies));

// 下载热身
const warmup = await downloadRoundWithFallback(targets, WARMUP_BYTES, REQ_TIMEOUT_MS);
if (!warmup.totalBytes ||!warmup.okCount) {
throw new Error('下载热身全部失败（' + warmup.okCount + '/' + warmup.total + (warmup.errors.length? '，' + warmup.errors[0]: '') + '）');
}
const warmupMbps = correctedMbps(warmup.totalBytes, warmup.elapsedMs, latency);
log('warmup=' + warmupMbps.toFixed(1) + ' Mbps size=' + (warmup.sizeBytes / 1048576).toFixed(1) + 'MiB elapsed=' + warmup.elapsedMs + 'ms');

// 按热身估速定主测包大小，目标每轮约 3 秒
let perConn = Math.round((warmupMbps * 1e6 / 8 * ROUND_TARGET_SEC / CONCURRENCY) / 1048576) * 1048576;
perConn = Math.max(MIN_BYTES, Math.min(MAX_BYTES, perConn));

const rounds = [];
let downBytes = warmup.totalBytes;
for (let i = 0; i < MAIN_ROUNDS; i++) {
const round = await downloadRoundWithFallback(targets, perConn, REQ_TIMEOUT_MS);
downBytes += round.totalBytes || 0;
if (round.totalBytes && round.okCount) {
rounds.push(correctedMbps(round.totalBytes, round.elapsedMs, latency));
log('download round ' + (i + 1) + '=' + rounds[rounds.length - 1].toFixed(1) + ' Mbps size=' + (round.sizeBytes / 1048576).toFixed(1) + 'MiB elapsed=' + round.elapsedMs + 'ms');
} else {
log('download round ' + (i + 1) + ' failed: ' + JSON.stringify(round.errors));
break;
}
}
if (!rounds.length) throw new Error('下载主测全部失败');
const downloadMbps = Math.max.apply(null, rounds);

// 上传
let uploadMbps = null;
let upBytes = 0;
try {
const upWarmup = await uploadRoundWithFallback(targets, UP_MIN_BYTES, REQ_TIMEOUT_MS);
upBytes += upWarmup.totalBytes || 0;
if (upWarmup.totalBytes && upWarmup.okCount) {
const upWarmupMbps = correctedMbps(upWarmup.totalBytes, upWarmup.elapsedMs, latency);
log('upload warmup=' + upWarmupMbps.toFixed(1) + ' Mbps elapsed=' + upWarmup.elapsedMs + 'ms');
let upPerConn = Math.round((upWarmupMbps * 1e6 / 8 * UP_TARGET_SEC / CONCURRENCY) / 1048576) * 1048576;
upPerConn = Math.max(UP_MIN_BYTES, Math.min(UP_MAX_BYTES, upPerConn));
const upRounds = [];
for (let i = 0; i < UP_ROUNDS; i++) {
const round = await uploadRoundWithFallback(targets, upPerConn, REQ_TIMEOUT_MS);
upBytes += round.totalBytes || 0;
if (round.totalBytes && round.okCount) {
upRounds.push(correctedMbps(round.totalBytes, round.elapsedMs, latency));
log('upload round ' + (i + 1) + '=' + upRounds[upRounds.length - 1].toFixed(1) + ' Mbps');
} else {
log('upload round ' + (i + 1) + ' failed: ' + JSON.stringify(round.errors));
break;
}
}
if (upRounds.length) uploadMbps = Math.max.apply(null, upRounds);
else uploadMbps = upWarmupMbps;
} else {
log('upload warmup failed: ' + JSON.stringify(upWarmup.errors));
}
} catch (e) {
log('upload failed: ' + e);
}

const totalSec = ((Date.now() - startAll) / 1000).toFixed(0);
const totalMB = ((downBytes + upBytes) / 1048576).toFixed(0);
const footer = '用时 ' + totalSec + 's · 流量 ' + totalMB + ' MB';
const plainLines = [
'下载 ' + downloadMbps.toFixed(0) + ' Mbps',
uploadMbps!== null? '上传 ' + uploadMbps.toFixed(0) + ' Mbps': '上传 —',
'延迟 ' + (latency!== null? latency + ' ms': '—') + ' · ' + locationText,
footer
];
const panel = buildPanel(downloadMbps, uploadMbps, latency, locationText, footer, policy);
showResult('⚡ Fast.com 测速', panel.plain, panel.html);
} catch (e) {
const failPlain = '测速失败\n\n' + String((e && e.message) || e);
const failHtml = '<p style="text-align: left; font-family: -apple-system; font-size: large"><b>测速失败</b></br></br>' + String((e && e.message) || e) + '</p>';
showResult('⚡ Fast.com 测速', failPlain, failHtml);
}
}

main();
