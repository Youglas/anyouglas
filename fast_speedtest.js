/*
================================================================
fast_speedtest.js — Quantumult X 用 fast.com 测当前节点下载速度
@Author: Tuesday
@Date: 2026-11-01 (v3: 更接近 fast.com 网页版的多轮稳态测速)
@Description:
1. 动态从 fast.com 抓 token（token 会轮换，不写死）,
失败时回落到 2026-11-01 实测有效的 token；
2. 调 api.fast.com/netflix/speedtest/v2 拿 Netflix OCA
测速节点（targets），按 fast.com 网页版的做法把
URL 改成 /speedtest/range/0-<字节数> 形式下载；
3. fast.com 网页版用 3 个连接、25 MB 级请求、连续
下载并算最大速度，且会丢掉开头的慢启动阶段。
QX 的 $task.fetch 只支持文本 body、单请求默认 10 秒，
25 MB x 3 并发在手机上既占内存又易超时，所以 v3
用等效做法：1 MiB 探针估速后，做 3 轮主测（每轮
约 2 秒、每连接 2~10 MiB），扣掉 1 字节 probe 测到
的连接建立时间，取最快一轮作为 headline，同时给
出 3 轮合计的稳态值。v2 只有单轮小包，握手/慢启动
占了全程很大比例，是偏低的主要原因；
4. 结果以 event-interaction 弹窗 + 通知两种方式给出，
并带上 fast.com 看到的出口 IP，用来核对流量到底
走没走节点。


- event-interaction 弹窗来自 $done({title, htmlMessage})，
空 $done() 会显示 No Results，v2 起成功/失败都返回弹窗。
- $task.fetch 显式传 timeout（毫秒）和
opts.policy = $environment.params（点任务时选中的
策略/节点），走 geo_location.js 同款写法。


- 精准要花流量：v3 一次约 30~90 MB（视速度而定），
别挂 cron 高频跑，手动点测一次即可。想省流量就把
下面的 MAIN_ROUNDS 改成 1，数值会回落到接近 v2。
- 如果弹窗里的出口 IP 是你的真实 IP，说明这部分流量
走了直连，测的是直连速度，不是节点速度。
- 测速值按 Content-Length 计字节，不按 body.length 计，
因为 OCA 返回的是二进制，JS 字符串长度不等于字节数。
- 和网页版仍不会完全相等：浏览器用 ArrayBuffer/流式
计字节，QX 只能用文本 body，解码开销会计入时间；
而且同一时刻节点/OCA 的拥塞也不同。看趋势和相对
变化比盯单次绝对值更靠谱。

手动点测（换成新 commit 固定地址可绕过
QX 24 小时远程缓存，见回复里的地址）：
[event-interaction]
https://cdn.jsdelivr.net/gh/Youglas/anyouglas@master/fast_speedtest.js, tag=Fast测速, img-url=bolt.square.fill.system, enabled=true
================================================================
*/

const NAME = 'Fast.com 测速';
const UA = 'Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Mobile/15E148 Safari/604.1';
const FALLBACK_TOKEN = 'YXNkZmFzZGxmbnNkYWZoYXNkZmhrYWxm';
const CONCURRENCY = 3;
const PROBE_BYTES = 1048576;
const MAIN_ROUNDS = 3;
const ROUND_TARGET_SEC = 2.0;
const MIN_BYTES = 2 * 1048576;
const MAX_BYTES = 10 * 1048576;
const REQ_TIMEOUT_MS = 10000;
const API_TIMEOUT_MS = 8000;

function log(msg) {
if (typeof console!== 'undefined' && console.log) console.log('[' + NAME + '] ' + msg);
}

function showResult(title, subtitle, plainMessage, htmlMessage) {
log(title + ' | ' + subtitle + ' | ' + plainMessage);
if (typeof $notify!== 'undefined') {
try { $notify(title, subtitle, plainMessage);} catch (e) { log('notify failed: ' + e);}
} else if (typeof $notification!== 'undefined' && $notification.post) {
try { $notification.post(title, subtitle, plainMessage);} catch (e) { log('notify failed: ' + e);}
}
if (typeof $done!== 'undefined') {
try {
$done({ title: title, message: plainMessage, htmlMessage: htmlMessage || plainMessage});
} catch (e) {
try { $done();} catch (e2) { /* ignore */}
}
}
}

function escapeHtml(s) {
return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

function toHtml(lines) {
const body = lines.map(function (l) { return escapeHtml(l);}).join('</br>');
return '<p style="text-align: center; font-family: -apple-system; font-size: large; font-weight: thin">' + body + '</p>';
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

function httpGet(url, timeoutMs) {
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
const req = { url: url, method: 'GET', headers: { 'User-Agent': UA, 'Accept': '*/*'}, timeout: timeoutMs};
const policy = selectedPolicy();
if (policy) req.opts = { policy: policy};
$task.fetch(req).then(function (resp) {
const headers = normalizeHeaders(resp.headers || {});
finish({ ok: resp.statusCode >= 200 && resp.statusCode < 300, status: resp.statusCode, headers: headers, body: resp.body || ''});
}).catch(function (e) {
finish({ ok: false, error: String((e && (e.error || e.message)) || e), status: 0, headers: {}, body: ''});
});
} else if (typeof $httpClient!== 'undefined' && $httpClient.get) {
$httpClient.get({ url: url, headers: { 'User-Agent': UA, 'Accept': '*/*'}}, function (err, resp, body) {
if (err) {
finish({ ok: false, error: String(err), status: 0, headers: {}, body: ''});
} else {
const headers = normalizeHeaders((resp && resp.headers) || {});
const status = (resp && (resp.status || resp.statusCode)) || 0;
finish({ ok: status >= 200 && status < 300, status: status, headers: headers, body: body || ''});
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

function mbps(bytes, elapsedMs) {
if (!elapsedMs || elapsedMs <= 0) return 0;
return (bytes * 8) / (elapsedMs / 1000) / 1e6;
}

// 扣掉连接建立时间后的速度：1 字节 probe 的耗时近似等于
// TCP+TLS+首字节开销，它不属于传输本身。保底 50ms，避免
// 极短一轮算出离谱值。
function correctedMbps(bytes, elapsedMs, setupMs) {
const corrected = Math.max(elapsedMs - (setupMs || 0), 50);
return mbps(bytes, corrected);
}

async function main() {
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
const exitIp = client.ip || '未知';
const loc = client.location? [client.location.city, client.location.country].filter(Boolean).join(', '): '';
const asn = client.asn? ('AS' + client.asn): '';
const policy = selectedPolicy();

const latencies = [];
for (let i = 0; i < 3; i++) {
const r = await httpGet(rangeUrl(targets[0], 1), 5000);
if (r.ok) latencies.push(r.ms);
}
const latency = latencies.length? Math.min.apply(null, latencies): null;

const probe = await downloadRound(targets, PROBE_BYTES, REQ_TIMEOUT_MS);
if (!probe.totalBytes ||!probe.okCount) {
throw new Error('下载探针全部失败（' + probe.okCount + '/' + probe.total + ' 成功' + (probe.errors.length? '，' + probe.errors[0]: '') + '），请检查节点是否能访问 Netflix CDN，或出口是否支持 IPv6');
}
const probeMbps = mbps(probe.totalBytes, probe.elapsedMs);

// 每轮目标约 2 秒；慢节点用 1 MiB 保底，避免单轮撞 10 秒超时
const minBytes = probeMbps < 8? 1048576: MIN_BYTES;
let perConn = Math.round((probeMbps * 1e6 / 8 * ROUND_TARGET_SEC / CONCURRENCY) / 1048576) * 1048576;
perConn = Math.max(minBytes, Math.min(MAX_BYTES, perConn));

const rounds = [];
let totalBytes = probe.totalBytes;
for (let i = 0; i < MAIN_ROUNDS; i++) {
const round = await downloadRound(targets, perConn, REQ_TIMEOUT_MS);
totalBytes += round.totalBytes || 0;
if (round.totalBytes && round.okCount) {
rounds.push({
raw: mbps(round.totalBytes, round.elapsedMs),
corrected: correctedMbps(round.totalBytes, round.elapsedMs, latency),
bytes: round.totalBytes,
elapsedMs: round.elapsedMs
});
}
if (!round.totalBytes) break;
}
if (!rounds.length) {
throw new Error('主测 3 轮全部失败，请检查节点是否能访问 Netflix CDN');
}

const best = rounds.reduce(function (a, b) { return b.corrected > a.corrected? b: a;}, rounds[0]);
const aggBytes = rounds.reduce(function (s, r) { return s + r.bytes;}, 0);
const aggElapsed = rounds.reduce(function (s, r) { return s + r.elapsedMs;}, 0);
const aggCorrected = correctedMbps(aggBytes, aggElapsed, latency);
const finalMbps = best.corrected;
const totalMB = totalBytes / 1048576;

const subtitle = '下载 ' + finalMbps.toFixed(1) + ' Mbps' + (latency!== null? ' · 延迟 ' + latency + ' ms': '');
const lines = [
'下载：' + finalMbps.toFixed(1) + ' Mbps（' + (finalMbps / 8).toFixed(1) + ' MB/s）',
'稳态合计：' + aggCorrected.toFixed(1) + ' Mbps（' + rounds.length + ' 轮合计，已扣连接时间）',
'各轮：' + rounds.map(function (r) { return r.corrected.toFixed(0);}).join(' / ') + ' Mbps',
'探针估速：' + probeMbps.toFixed(1) + ' Mbps'
];
if (latency!== null) lines.push('延迟：' + latency + ' ms（1 字节 range 最小值）');
lines.push('出口：' + exitIp + (loc? '（' + loc + '）': '') + (asn? ' ' + asn: ''));
if (policy) lines.push('策略：' + policy);
lines.push('本次流量：约 ' + totalMB.toFixed(1) + ' MB（' + CONCURRENCY + ' 并发 x ' + rounds.length + ' 轮，每连接 ' + (perConn / 1048576).toFixed(0) + ' MiB）');
lines.push('注：QX 文本 body 解码开销会计入时间，与网页版不会完全相等');
const plain = lines.join('\n');
showResult(NAME, subtitle, plain, toHtml(lines));
} catch (e) {
const lines = [
'测速失败',
String((e && e.message) || e),
'请确认 QX 已开启、节点可用，且 fast.com / nflxvideo.net 走代理'
];
showResult(NAME, '测速失败', lines.join('\n'), toHtml(lines));
}
}

main();
