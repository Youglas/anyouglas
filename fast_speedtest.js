/*
================================================================
fast_speedtest.js — Quantumult X 用 fast.com 测当前节点下载速度
@Author: Tuesday
@Date: 2026-11-01
@Description:
1. 动态从 fast.com 抓 token（token 会轮换，不写死）,
失败时回落到 2026-11-01 实测有效的 token；
2. 调 api.fast.com/netflix/speedtest/v2 拿 Netflix OCA
测速节点（targets），按 fast.com 网页版的做法把
URL 改成 /speedtest/range/0-<字节数> 形式下载；
3. 先 3 并发各下 1 MiB 探针估速，再按估速自适应
主测包大小（每连接 2~8 MiB，3 并发），算下载速度；
4. 结果用通知发出来，并带上 fast.com 看到的出口 IP，
用来核对流量到底走没走节点。


- 这是下载测速，不是 QX 自带的延迟测试（server_check_url
那个只测 HEAD 延迟）。一次运行约用 9~27 MB 流量，
别挂 cron 高频跑，建议用 event-interaction 手动点一次测一次。
- 脚本的请求走不走节点，取决于 QX 给 fast.com /
nflxvideo.net 分的策略。如果通知里的出口 IP 是你的
真实 IP，说明这部分流量走了直连，测的是直连速度，
不是节点速度；请确认 Finals/相关规则走代理后再测。
- 测速值按 Content-Length 计字节，不按 body.length 计，
因为 OCA 返回的是二进制，JS 字符串长度不等于字节数。

推荐手动点测（event-interaction）：
[event-interaction]
https://raw.githubusercontent.com/Youglas/anyouglas/master/fast_speedtest.js, tag=Fast测速, img-url=bolt.square.fill.system, enabled=true
（把上面 URL 换成你实际托管的脚本地址；本地文件也可直接
在 QX 里新建任务时选本地脚本）

如需定时测（不推荐，费流量）：
[task_local]
https://raw.githubusercontent.com/Youglas/anyouglas/master/fast_speedtest.js, tag=Fast测速, enabled=true
并在 QX 任务里给它设一个 cron，例如每天 1 次。
================================================================
*/

const NAME = 'Fast.com 测速';
const UA = 'Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Mobile/15E148 Safari/604.1';
// 2026-11-01 从 https://fast.com/app-*.js 实测提取，仅作动态获取失败时的回落
const FALLBACK_TOKEN = 'YXNkZmFzZGxmbnNkYWZoYXNkZmhrYWxm';
const CONCURRENCY = 3; // 并发连接数（fast.com 网页版默认 3~5 个目标）
const PROBE_BYTES = 1048576; // 探针：每连接 1 MiB
const MIN_BYTES = 2 * 1048576; // 主测：每连接最少 2 MiB
const MAX_BYTES = 8 * 1048576; // 主测：每连接最多 8 MiB
const REQ_TIMEOUT_MS = 20000; // 单次请求超时（脚本自计时，不依赖各平台 timeout 单位）
const API_TIMEOUT_MS = 10000;

function log(msg) {
if (typeof console!== 'undefined' && console.log) console.log('[' + NAME + '] ' + msg);
}

function notify(title, subtitle, message) {
if (typeof $notify!== 'undefined') {
$notify(title, subtitle, message);
} else if (typeof $notification!== 'undefined' && $notification.post) {
$notification.post(title, subtitle, message);
}
log(title + ' | ' + subtitle + ' | ' + message);
}

function done() {
if (typeof $done!== 'undefined') $done();
}

// headers 统一成小写 key，QX / Surge / Loon 的大小写不一致
function normalizeHeaders(headers) {
const out = {};
if (!headers) return out;
Object.keys(headers).forEach(function (k) { out[String(k).toLowerCase()] = headers[k];});
return out;
}

// 优先用 Content-Length 计字节；没有时才回落到 body 长度
function countedBytes(headers, body) {
const n = parseInt(headers['content-length'], 10);
if (!isNaN(n) && n > 0) return n;
if (body) return body.length;
return 0;
}

// 统一的 GET：优先 $task.fetch（QX），其次 $httpClient（Surge/Loon）
// 超时用脚本自带的 Promise.race 计时，不依赖各平台 timeout 参数的单位
function httpGet(url, timeoutMs) {
const start = Date.now();
return new Promise(function (resolve) {
let settled = false;
const timer = setTimeout(function () {
if (!settled) {
settled = true;
resolve({ ok: false, error: 'timeout', status: 0, headers: {}, body: '', ms: Date.now() - start});
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
$task.fetch({ url: url, method: 'GET', headers: { 'User-Agent': UA, 'Accept': '*/*'}}).then(function (resp) {
const headers = normalizeHeaders(resp.headers || {});
finish({ ok: resp.statusCode >= 200 && resp.statusCode < 300, status: resp.statusCode, headers: headers, body: resp.body || ''});
}).catch(function (e) {
finish({ ok: false, error: String((e && e.message) || e), status: 0, headers: {}, body: ''});
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

// 从 fast.com 动态抓 token：首页 -> app-*.js -> token:"..."
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
if (!res.ok) throw new Error('fast.com API 请求失败，HTTP ' + res.status);
let data;
try { data = JSON.parse(res.body);} catch (e) { throw new Error('fast.com API 返回解析失败');}
if (data && data.message) throw new Error('fast.com API：' + data.message);
const targets = (data && data.targets || []).map(function (t) { return t.url;}).filter(Boolean);
if (!targets.length) throw new Error('fast.com 没有返回测速节点');
return { client: data.client || {}, targets: targets};
}

// fast.com 网页版做法：speedtest -> speedtest/range/，再拼 0-<字节数>
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
results.forEach(function (r) {
if (r.ok) { okCount++; totalBytes += countedBytes(r.headers, r.body);}
});
return { totalBytes: totalBytes, elapsedMs: elapsedMs, okCount: okCount, total: results.length};
}

function mbps(bytes, elapsedMs) {
if (!elapsedMs || elapsedMs <= 0) return 0;
return (bytes * 8) / (elapsedMs / 1000) / 1e6;
}

async function main() {
try {
// 1. token（动态优先，API 报 Unknown token 时用回落 token 再试一次）
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

// 2. 延迟：对第一个节点打 3 次 1 字节 range，取最小值
const latencies = [];
for (let i = 0; i < 3; i++) {
const r = await httpGet(rangeUrl(targets[0], 1), 5000);
if (r.ok) latencies.push(r.ms);
}
const latency = latencies.length? Math.min.apply(null, latencies): null;

// 3. 探针：3 并发各 1 MiB
const probe = await downloadRound(targets, PROBE_BYTES, REQ_TIMEOUT_MS);
if (!probe.totalBytes ||!probe.okCount) {
throw new Error('下载探针全部失败（' + probe.okCount + '/' + probe.total + ' 成功），请检查节点是否能访问 Netflix CDN，或出口是否支持 IPv6');
}
const probeMbps = mbps(probe.totalBytes, probe.elapsedMs);

// 4. 主测：按探针估速，目标约 3.5 秒，每连接 2~8 MiB
let perConn = Math.round((probeMbps * 1e6 / 8 * 3.5 / CONCURRENCY) / 1048576) * 1048576;
perConn = Math.max(MIN_BYTES, Math.min(MAX_BYTES, perConn));
const mainTest = await downloadRound(targets, perConn, REQ_TIMEOUT_MS);
let finalMbps = probeMbps;
let usedBytes = probe.totalBytes;
let usedMs = probe.elapsedMs;
if (mainTest.totalBytes && mainTest.okCount) {
finalMbps = mbps(mainTest.totalBytes, mainTest.elapsedMs);
usedBytes = mainTest.totalBytes;
usedMs = mainTest.elapsedMs;
}

const totalMB = (probe.totalBytes + (mainTest.totalBytes || 0)) / 1048576;
const subtitle = '下载 ' + finalMbps.toFixed(1) + ' Mbps' + (latency!== null? ' · 延迟 ' + latency + ' ms': '');
const message =
'下载：' + finalMbps.toFixed(1) + ' Mbps（' + (finalMbps / 8).toFixed(1) + ' MB/s）\n' +
'探针估速：' + probeMbps.toFixed(1) + ' Mbps\n' +
(latency!== null? '延迟：' + latency + ' ms（1 字节 range 最小值）\n': '') +
'出口：' + exitIp + (loc? '（' + loc + '）': '') + (asn? ' ' + asn: '') + '\n' +
'本次流量：约 ' + totalMB.toFixed(1) + ' MB（' + CONCURRENCY + ' 并发，主测每连接 ' + (perConn / 1048576).toFixed(0) + ' MiB）\n' +
'注：结果为参考值，单轮取样会略低于 fast.com 网页版';
notify(NAME, subtitle, message);
} catch (e) {
notify(NAME, '测速失败', String((e && e.message) || e) + '\n请确认 QX 已开启、节点可用，且 fast.com / nflxvideo.net 走代理');
}
done();
}

main();
