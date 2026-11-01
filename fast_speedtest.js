/*
================================================================
fast_speedtest.js — Quantumult X 用 fast.com 测当前节点下载速度
@Author: Tuesday
@Date: 2026-11-01 (v2: 修复 event-interaction 显示 No Results)
@Description:
1. 动态从 fast.com 抓 token（token 会轮换，不写死）,
失败时回落到 2026-11-01 实测有效的 token；
2. 调 api.fast.com/netflix/speedtest/v2 拿 Netflix OCA
测速节点（targets），按 fast.com 网页版的做法把
URL 改成 /speedtest/range/0-<字节数> 形式下载；
3. 先 3 并发各下 1 MiB 探针估速，再按估速自适应
主测包大小（每连接 2~4 MiB，3 并发），算下载速度；
4. 结果以 event-interaction 弹窗 + 通知两种方式给出，
并带上 fast.com 看到的出口 IP，用来核对流量到底
走没走节点。


- v1 只调 $notify + 空 $done()。QX 点 event-interaction
时，弹窗内容来自 $done({title, htmlMessage})，空 $done()
就会显示 No Results，通知在 QX 前台时也可能看不到。
v2 改为 $done({title, message, htmlMessage})，失败时也
一样返回弹窗，不再出现 No Results。
- v2 给 $task.fetch 显式传 timeout（毫秒）和
opts.policy = $environment.params（点任务时选中的
策略/节点），走 geo_location.js / streaming-ui-check.js
同款写法，让请求真正走当前选中的节点，而不是只看
Finals 的默认路由。$environment.params 为空时不传。
- QX 的 $task.fetch 默认超时 10 秒，单次下载包改小
（主测每连接最多 4 MiB），避免 8 MiB 在慢节点上
撞上默认超时。


- 这是下载测速，不是 QX 自带的延迟测试。一次运行约用
6~15 MB 流量，别挂 cron 高频跑，建议 event-interaction
手动点一次测一次。
- 如果弹窗里的出口 IP 是你的真实 IP，说明这部分流量
走了直连，测的是直连速度，不是节点速度。
- 测速值按 Content-Length 计字节，不按 body.length 计，
因为 OCA 返回的是二进制，JS 字符串长度不等于字节数。

手动点测：
[event-interaction]
https://cdn.jsdelivr.net/gh/Youglas/anyouglas@master/fast_speedtest.js, tag=Fast测速, img-url=bolt.square.fill.system, enabled=true
================================================================
*/

const NAME = 'Fast.com 测速';
const UA = 'Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Mobile/15E148 Safari/604.1';
// 2026-11-01 从 https://fast.com/app-*.js 实测提取，仅作动态获取失败时的回落
const FALLBACK_TOKEN = 'YXNkZmFzZGxmbnNkYWZoYXNkZmhrYWxm';
const CONCURRENCY = 3; // 并发连接数
const PROBE_BYTES = 1048576; // 探针：每连接 1 MiB
const MIN_BYTES = 2 * 1048576; // 主测：每连接最少 2 MiB
const MAX_BYTES = 4 * 1048576; // 主测：每连接最多 4 MiB（QX 单请求默认 10 秒超时，包太大易超时）
const REQ_TIMEOUT_MS = 10000; // 单次下载超时，毫秒，和 $task.fetch 的 timeout 字段一致
const API_TIMEOUT_MS = 8000;

function log(msg) {
if (typeof console!== 'undefined' && console.log) console.log('[' + NAME + '] ' + msg);
}

// 点 event-interaction 时 QX 弹窗只认 $done({title, htmlMessage})，
// 所以成功和失败都走这里；同时再发一条 $notify，cron 模式也能看到。
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

function toHtml(title, lines) {
const body = lines.map(function (l) { return escapeHtml(l);}).join('</br>');
return '<p style="text-align: center; font-family: -apple-system; font-size: large; font-weight: thin">' + body + '</p>';
}

// 点任务时选中的策略/节点名，QX 放在 $environment.params 里；
// 没有时返回 null，请求就按 QX 规则走，不强行指定。
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

let perConn = Math.round((probeMbps * 1e6 / 8 * 3.5 / CONCURRENCY) / 1048576) * 1048576;
perConn = Math.max(MIN_BYTES, Math.min(MAX_BYTES, perConn));
const mainTest = await downloadRound(targets, perConn, REQ_TIMEOUT_MS);
let finalMbps = probeMbps;
if (mainTest.totalBytes && mainTest.okCount) {
finalMbps = mbps(mainTest.totalBytes, mainTest.elapsedMs);
}

const totalMB = (probe.totalBytes + (mainTest.totalBytes || 0)) / 1048576;
const subtitle = '下载 ' + finalMbps.toFixed(1) + ' Mbps' + (latency!== null? ' · 延迟 ' + latency + ' ms': '');
const lines = [
'下载：' + finalMbps.toFixed(1) + ' Mbps（' + (finalMbps / 8).toFixed(1) + ' MB/s）',
'探针估速：' + probeMbps.toFixed(1) + ' Mbps'
];
if (latency!== null) lines.push('延迟：' + latency + ' ms（1 字节 range 最小值）');
lines.push('出口：' + exitIp + (loc? '（' + loc + '）': '') + (asn? ' ' + asn: ''));
if (policy) lines.push('策略：' + policy);
lines.push('本次流量：约 ' + totalMB.toFixed(1) + ' MB（' + CONCURRENCY + ' 并发，主测每连接 ' + (perConn / 1048576).toFixed(0) + ' MiB）');
lines.push('注：单轮取样，略低于 fast.com 网页版');
const plain = lines.join('\n');
showResult(NAME, subtitle, plain, toHtml(NAME, lines));
} catch (e) {
const lines = [
'测速失败',
String((e && e.message) || e),
'请确认 QX 已开启、节点可用，且 fast.com / nflxvideo.net 走代理'
];
showResult(NAME, '测速失败', lines.join('\n'), toHtml(NAME, lines));
}
}

main();
