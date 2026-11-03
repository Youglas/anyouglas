/*
================================================================
video_unlock_check.js — Quantumult X 检测全球主流视频网站解锁
@Author: Tuesday
@Date: 2026-11-03 (v1)
@Description:
检测当前出口 IP（所选策略/节点）能否打开全球主流视频网站。
14 个服务并行检测，7 秒超时，总时长约 8 秒。
只测“出口 IP 能否打开服务”，不代表账号能登录/订阅
（账号地区、支付方式、手机号另算）；Netflix 的“仅自制剧”
指该 IP 只能看 Netflix 自制剧，看不了第三方版权内容。

检测方法（2026-11-03 沙箱实测，美国出口 + 社区脚本方法）：
- Netflix：两个标题页。80018499（自制剧测试标题）+
  70143836（第三方版权《绝命毒师》）。任一返回 403 = IP
  被封禁；自制剧页不可看 = 未解锁；自制剧可看、版权剧
  页含 page-404/NSEZ-403/Oh no! = 仅自制剧；两页都正常
  = 全解锁。
- Disney+：首页。403/403 ERROR 或正文含地区不可用短语
  = 未解锁。
- Max：首页。地区不可用短语 = 未解锁；200 且含 Max/HBO
  标记 = 已解锁。
- Hulu：首页。403 或地区不可用短语 = 未解锁（美国以外
  基本都锁）。
- Prime Video：首页。含 isServiceRestricted = 服务受限；
  "currentTerritory":"XX" = 已解锁并给出地区。
- Paramount+：首页。canonical 指向 /intl/ 或地区不可用短语
  = 未解锁。
- Peacock：首页。地区不可用短语 = 未解锁（美国以外基本
  都锁）。
- Apple TV+：首页 tv.apple.com，200 且含 Apple TV = 已解锁。
- YouTube Premium：/premium 页（桌面 UA）。含 "Premium is
  not available in your country" = 未解锁；
  INNERTUBE_CONTEXT_GL 给出地区且含 ad-free = 已解锁。
- Discovery+：三步 API：bootstrapInfo 取 realm（dplusapac
  = 亚太未提供）→ token 接口取 token → cms 路由页，含
  relationships = 已解锁，含 unavailable/not yet available
  = 未解锁。
- Crunchyroll：首页。地区不可用短语 = 未解锁。
- BBC iPlayer：mediaselector 接口。含 geolocation = 仅限
  英国（未解锁）；含 vs-hls-push-uk = 已解锁（英国出口）。
- ESPN+：首页。地区不可用短语 = 未解锁（美国以外基本都锁）。
- DAZN：Startup API（POST）。Region.isAllowed +
  GeolocatedCountry 给出地区；Security policy has been
  breached = IP 被封禁。
- 出口国家：cloudflare trace 的 loc=，只作展示。
注意：Disney+/Max/Hulu/Paramount+/Peacock/ESPN+/Crunchyroll/
Apple TV+ 的“未解锁”短语取自社区脚本与页面文案，沙箱只
验到了美国出口的“已解锁”形态，未解锁形态未在沙箱复现，
以真机结果为准。

手动点测（固定 commit 地址，绕过 QX 24 小时远程缓存）：
[task_local]
event-interaction https://cdn.jsdelivr.net/gh/Youglas/anyouglas@master/video_unlock_check.js, tag=视频解锁检测, img-url=film.fill, enabled=true
================================================================
*/

const NAME = '视频解锁检测';
const UA = 'Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Mobile/15E148 Safari/604.1';
const UA_DESKTOP = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36';
const TIMEOUT_MS = 7000;

function log(msg) {
if (typeof console !== 'undefined' && console.log) console.log('[' + NAME + '] ' + msg);
}

function selectedPolicy() {
try {
if (typeof $environment !== 'undefined' && $environment && typeof $environment.params === 'string' && $environment.params) return $environment.params;
} catch (e) { /* ignore */ }
return null;
}

function normalizeHeaders(headers) {
const out = {};
if (!headers) return out;
Object.keys(headers).forEach(function (k) { out[String(k).toLowerCase()] = headers[k]; });
return out;
}

function httpRequest(url, options) {
options = options || {};
const timeoutMs = options.timeoutMs || TIMEOUT_MS;
const start = Date.now();
return new Promise(function (resolve) {
let settled = false;
const timer = setTimeout(function () {
if (!settled) { settled = true; resolve({ ok: false, error: 'timeout', status: 0, headers: {}, body: '', ms: Date.now() - start }); }
}, timeoutMs);
function finish(res) {
if (settled) return;
settled = true;
clearTimeout(timer);
res.ms = Date.now() - start;
resolve(res);
}
try {
if (typeof $task !== 'undefined' && $task.fetch) {
const req = { url: url, method: options.method || 'GET', headers: { 'User-Agent': options.ua || UA, 'Accept': '*/*' }, timeout: timeoutMs };
if (options.headers) {
Object.keys(options.headers).forEach(function (k) { req.headers[k] = options.headers[k]; });
}
if (options.body) req.body = options.body;
const policy = selectedPolicy();
if (policy) req.opts = { policy: policy };
$task.fetch(req).then(function (resp) {
const headers = normalizeHeaders(resp.headers || {});
const status = resp.statusCode || 0;
finish({ ok: status >= 200 && status < 300, status: status, headers: headers, body: resp.body || '' });
}).catch(function (e) {
finish({ ok: false, error: String((e && (e.error || e.message)) || e), status: 0, headers: {}, body: '' });
});
} else if (typeof $httpClient !== 'undefined' && $httpClient.get) {
const cb = function (err, resp, respBody) {
if (err) finish({ ok: false, error: String(err), status: 0, headers: {}, body: '' });
else {
const headers = normalizeHeaders((resp && resp.headers) || {});
const status = (resp && (resp.status || resp.statusCode)) || 0;
finish({ ok: status >= 200 && status < 300, status: status, headers: headers, body: respBody || '' });
}
};
const reqObj = { url: url, headers: { 'User-Agent': options.ua || UA, 'Accept': '*/*' } };
if (options.headers) {
Object.keys(options.headers).forEach(function (k) { reqObj.headers[k] = options.headers[k]; });
}
if ((options.method || 'GET') === 'POST' && $httpClient.post) {
reqObj.body = options.body || '';
$httpClient.post(reqObj, cb);
} else {
$httpClient.get(reqObj, cb);
}
} else {
finish({ ok: false, error: 'no http client', status: 0, headers: {}, body: '' });
}
} catch (e) {
finish({ ok: false, error: String((e && e.message) || e), status: 0, headers: {}, body: '' });
}
});
}

function httpGet(url, timeoutMs, ua) {
return httpRequest(url, { timeoutMs: timeoutMs, ua: ua });
}

function parseTrace(body) {
const m = body ? body.match(/loc=([A-Z]{2})/) : null;
return m ? m[1] : null;
}

// 地区不可用短语（整句匹配，不用 "unavailable" 这类单个词：
// 正常页面 JS/FAQ 里也有 unavailable，会误判）
function isBlockedText(body) {
const b = (body || '').toLowerCase();
return b.indexOf('not available in your country') > -1
|| b.indexOf('not available in your region') > -1
|| b.indexOf("isn't available in your country") > -1
|| b.indexOf("isn't available in your region") > -1
|| b.indexOf('is not available in your country') > -1
|| b.indexOf('is not available in your region') > -1
|| b.indexOf('not yet available in your region') > -1
|| b.indexOf('unavailable in your region') > -1
|| b.indexOf('unavailable in your country') > -1
|| b.indexOf('only available in the u.s.') > -1
|| b.indexOf('only available in the united states') > -1
|| b.indexOf('service is not available in your region') > -1
|| b.indexOf('is not yet available in your region') > -1
|| b.indexOf('premium is not available in your country') > -1;
}

function hasMarker(body, marker) {
return (body || '').toLowerCase().indexOf(marker.toLowerCase()) > -1;
}

// 每个检测返回 {status:'ok'|'blocked'|'timeout'|'unknown'|'partial', detail:'...'}
async function checkNetflix() {
const results = await Promise.all([
httpGet('https://www.netflix.com/title/80018499', TIMEOUT_MS),
httpGet('https://www.netflix.com/title/70143836', TIMEOUT_MS)
]);
const t1 = results[0];
const t2 = results[1];
if (t1.error === 'timeout' && t2.error === 'timeout') return { status: 'timeout', detail: '超时' };
if (t1.status === 403 || t2.status === 403) return { status: 'blocked', detail: 'IP 被封禁（HTTP 403）' };
function unavailable(r) {
const b = (r.body || '').toLowerCase();
return b.indexOf('page-404') > -1 || b.indexOf('nsez-403') > -1 || b.indexOf('oh no!') > -1;
}
if (!t1.body || unavailable(t1)) return { status: 'blocked', detail: '地区不支持' };
if (unavailable(t2)) return { status: 'partial', detail: '仅自制剧' };
if (t1.status === 200 && t2.status === 200) return { status: 'ok', detail: '全解锁' };
if (t1.status || t2.status) return { status: 'unknown', detail: 'HTTP ' + (t1.status || t2.status) };
return { status: 'unknown', detail: t1.error || t2.error || '无响应' };
}

async function checkHomepage(name, url, mustContain) {
const r = await httpGet(url, TIMEOUT_MS);
if (r.error === 'timeout') return { status: 'timeout', detail: '超时' };
if (r.status === 403) return { status: 'blocked', detail: 'HTTP 403' };
if (isBlockedText(r.body)) return { status: 'blocked', detail: '地区受限' };
if (hasMarker(r.body, '403 error')) return { status: 'blocked', detail: 'IP 被封禁' };
if (r.status === 200 && r.body && (!mustContain || hasMarker(r.body, mustContain))) return { status: 'ok', detail: 'HTTP 200' };
if (r.status >= 300 && r.status < 400) return { status: 'ok', detail: 'HTTP ' + r.status + '（跳转）' };
if (r.status) return { status: 'unknown', detail: 'HTTP ' + r.status };
return { status: 'unknown', detail: r.error || '无响应' };
}

async function checkDisneyPlus() {
return checkHomepage('Disney+', 'https://www.disneyplus.com/', 'disney');
}

async function checkMax() {
return checkHomepage('Max', 'https://www.max.com/', 'max');
}

async function checkHulu() {
return checkHomepage('Hulu', 'https://www.hulu.com/', 'hulu');
}

async function checkPrimeVideo() {
const r = await httpGet('https://www.primevideo.com/', TIMEOUT_MS);
if (r.error === 'timeout') return { status: 'timeout', detail: '超时' };
if (hasMarker(r.body, 'isServiceRestricted')) return { status: 'blocked', detail: '服务受限' };
const m = (r.body || '').match(/"currentTerritory":"([A-Z]+)"/);
if (m) return { status: 'ok', detail: '地区 ' + m[1] };
if (r.status === 200 && r.body) return { status: 'unknown', detail: '无地区信息' };
if (r.status) return { status: 'unknown', detail: 'HTTP ' + r.status };
return { status: 'unknown', detail: r.error || '无响应' };
}

async function checkParamountPlus() {
const r = await httpGet('https://www.paramountplus.com/', TIMEOUT_MS);
if (r.error === 'timeout') return { status: 'timeout', detail: '超时' };
const canonicalIntl = /<link rel="canonical"[^>]*\/intl\//i.test(r.body || '');
if (canonicalIntl || isBlockedText(r.body)) return { status: 'blocked', detail: '地区受限' };
if (r.status === 200 && hasMarker(r.body, 'paramount')) return { status: 'ok', detail: 'HTTP 200' };
if (r.status) return { status: 'unknown', detail: 'HTTP ' + r.status };
return { status: 'unknown', detail: r.error || '无响应' };
}

async function checkPeacock() {
return checkHomepage('Peacock', 'https://www.peacocktv.com/', 'peacock');
}

async function checkAppleTV() {
return checkHomepage('Apple TV+', 'https://tv.apple.com/', 'Apple TV');
}

async function checkYouTubePremium() {
const r = await httpGet('https://www.youtube.com/premium', TIMEOUT_MS, UA_DESKTOP);
if (r.error === 'timeout') return { status: 'timeout', detail: '超时' };
if (isBlockedText(r.body)) return { status: 'blocked', detail: '地区不支持' };
const m = (r.body || '').match(/"INNERTUBE_CONTEXT_GL":"([A-Z]{2})"/);
if (m) return { status: 'ok', detail: '地区 ' + m[1] };
if (hasMarker(r.body, 'ad-free')) return { status: 'ok', detail: 'HTTP 200' };
if (r.status) return { status: 'unknown', detail: 'HTTP ' + r.status };
return { status: 'unknown', detail: r.error || '无响应' };
}

function randomHex(n) {
const c = '0123456789abcdef';
let s = '';
for (let i = 0; i < n; i++) s += c.charAt(Math.floor(Math.random() * 16));
return s;
}

function parseJsonSafe(text) {
try { return JSON.parse(text); } catch (e) { return null; }
}

async function checkDiscoveryPlus() {
const boot = await httpRequest('https://global-prod.disco-api.com/bootstrapInfo', {
headers: {
'Accept': '*/*',
'Origin': 'https://www.discoveryplus.com',
'Referer': 'https://www.discoveryplus.com/',
'X-Disco-Client': 'WEB:UNKNOWN:dplus_us:2.46.0',
'X-Disco-Params': 'bid=dplus,hn=www.discoveryplus.com'
}
});
if (boot.error === 'timeout') return { status: 'timeout', detail: '超时' };
const bootJson = parseJsonSafe(boot.body);
const attrs = (bootJson && bootJson.data && bootJson.data.attributes) || bootJson || {};
const realm = attrs.realm;
const baseApiUrl = attrs.baseApiUrl;
if (!realm || !baseApiUrl) {
if (boot.status) return { status: 'unknown', detail: 'bootstrap HTTP ' + boot.status };
return { status: 'unknown', detail: boot.error || '无响应' };
}
if (realm === 'dplusapac') return { status: 'blocked', detail: '亚太暂未提供' };
const deviceId = randomHex(32);
const tokenRes = await httpRequest(baseApiUrl + '/token?deviceId=' + deviceId + '&realm=' + realm + '&shortlived=true', {
headers: {
'Accept': '*/*',
'Origin': 'https://www.discoveryplus.com',
'Referer': 'https://www.discoveryplus.com/',
'X-Device-Info': 'dplus_us/2.46.0 (desktop/desktop; Windows/NT 10.0; ' + deviceId + ')',
'X-Disco-Client': 'WEB:UNKNOWN:dplus_us:2.46.0',
'X-Disco-Params': 'realm=' + realm + ',bid=dplus,hn=www.discoveryplus.com,hth=,features=ar'
}
});
if (tokenRes.error === 'timeout') return { status: 'timeout', detail: '超时' };
const tokenMatch = (tokenRes.body || '').match(/"token"\s*:\s*"([^"]+)"/);
if (!tokenMatch) {
if (tokenRes.status) return { status: 'unknown', detail: 'token HTTP ' + tokenRes.status };
return { status: 'unknown', detail: tokenRes.error || '无响应' };
}
const cms = await httpRequest(baseApiUrl + '/cms/routes/tabbed-home?include=default&decorators=viewingHistory,isFavorite,playbackAllowed,contentAction', {
headers: {
'Accept': '*/*',
'Origin': 'https://www.discoveryplus.com',
'Referer': 'https://www.discoveryplus.com/',
'X-Disco-Client': 'WEB:UNKNOWN:dplus_us:2.46.0',
'X-Disco-Params': 'realm=' + realm + ',bid=dplus,hn=www.discoveryplus.com,hth=,features=ar',
'Cookie': 'st=' + tokenMatch[1]
}
});
if (cms.error === 'timeout') return { status: 'timeout', detail: '超时' };
const cb = (cms.body || '').toLowerCase();
if (cb.indexOf('is unavailable in your') > -1 || cb.indexOf('not yet available') > -1) return { status: 'blocked', detail: '地区不支持' };
if (cb.indexOf('relationships') > -1) return { status: 'ok', detail: 'HTTP 200' };
if (cms.status) return { status: 'unknown', detail: 'HTTP ' + cms.status };
return { status: 'unknown', detail: cms.error || '无响应' };
}

async function checkCrunchyroll() {
return checkHomepage('Crunchyroll', 'https://www.crunchyroll.com/', 'crunchyroll');
}

async function checkBBCIPlayer() {
const r = await httpGet('https://open.live.bbc.co.uk/mediaselector/6/select/version/2.0/mediaset/pc/vpid/bbc_one_london/format/json/jsfunc/JS_callbacks0', TIMEOUT_MS);
if (r.error === 'timeout') return { status: 'timeout', detail: '超时' };
const b = (r.body || '').toLowerCase();
if (b.indexOf('vs-hls-push-uk') > -1) return { status: 'ok', detail: '英国' };
if (b.indexOf('geolocation') > -1) return { status: 'blocked', detail: '仅限英国' };
if (r.status) return { status: 'unknown', detail: 'HTTP ' + r.status };
return { status: 'unknown', detail: r.error || '无响应' };
}

async function checkESPNPlus() {
return checkHomepage('ESPN+', 'https://plus.espn.com/', 'espn');
}

async function checkDAZN() {
const sessionId = randomHex(8) + '-' + randomHex(4) + '-' + randomHex(4) + '-' + randomHex(4) + '-' + randomHex(12);
const r = await httpRequest('https://startup.core.indazn.com/misl/v5/Startup', {
method: 'POST',
headers: {
'Accept': '*/*',
'Content-Type': 'application/json',
'Origin': 'https://www.dazn.com',
'Referer': 'https://www.dazn.com/',
'X-Session-Id': sessionId
},
body: JSON.stringify({ Version: '2', LandingPageKey: 'generic', Languages: 'en-US', Platform: 'web', Manufacturer: '', PromoCode: '', PlatformAttributes: {} })
});
if (r.error === 'timeout') return { status: 'timeout', detail: '超时' };
if (hasMarker(r.body, 'Security policy has been breached')) return { status: 'blocked', detail: 'IP 被封禁' };
const d = parseJsonSafe(r.body);
const region = d && d.Region;
if (region) {
if (region.isAllowed === true) {
const country = (region.GeolocatedCountry || '').toUpperCase();
return { status: 'ok', detail: country ? '地区 ' + country : 'HTTP 200' };
}
if (region.isAllowed === false) return { status: 'blocked', detail: '地区不支持' };
}
if (r.status) return { status: 'unknown', detail: 'HTTP ' + r.status };
return { status: 'unknown', detail: r.error || '无响应' };
}

const FLAGS = { US: '🇺🇸', JP: '🇯🇵', SG: '🇸🇬', HK: '🇭🇰', TW: '🇨🇳', GB: '🇬🇧', DE: '🇩🇪', FR: '🇫🇷', NL: '🇳🇱', KR: '🇰🇷', AU: '🇦🇺', CA: '🇨🇦', CH: '🇨🇭', IT: '🇮🇹', ES: '🇪🇸', IN: '🇮🇳', BR: '🇧🇷', MX: '🇲🇽', ID: '🇮🇩', MY: '🇲🇾', TH: '🇹🇭', VN: '🇻🇳', PH: '🇵🇭', NZ: '🇳🇿', SE: '🇸🇪', NO: '🇳🇴', DK: '🇩🇰', FI: '🇫🇮', IE: '🇮🇪', AT: '🇦🇹', BE: '🇧🇪', PL: '🇵🇱', TR: '🇹🇷', AE: '🇦🇪', ZA: '🇿🇦', AR: '🇦🇷', CL: '🇨🇱' };
function flagFor(code) {
return FLAGS[code] || '';
}

const CHECKS = [
{ name: 'Netflix', fn: checkNetflix },
{ name: 'Disney+', fn: checkDisneyPlus },
{ name: 'Max', fn: checkMax },
{ name: 'Hulu', fn: checkHulu },
{ name: 'Prime Video', fn: checkPrimeVideo },
{ name: 'Paramount+', fn: checkParamountPlus },
{ name: 'Peacock', fn: checkPeacock },
{ name: 'Apple TV+', fn: checkAppleTV },
{ name: 'YouTube Premium', fn: checkYouTubePremium },
{ name: 'Discovery+', fn: checkDiscoveryPlus },
{ name: 'Crunchyroll', fn: checkCrunchyroll },
{ name: 'BBC iPlayer', fn: checkBBCIPlayer },
{ name: 'ESPN+', fn: checkESPNPlus },
{ name: 'DAZN', fn: checkDAZN }
];

const ICONS = { ok: '✅', blocked: '❌', timeout: '⏱️', unknown: '❓', partial: '⚠️' };
const LABELS = { ok: '已解锁', blocked: '未解锁', timeout: '超时', unknown: '未知', partial: '仅自制剧' };

async function main() {
const startAll = Date.now();
const tracePromise = httpGet('https://www.cloudflare.com/cdn-cgi/trace', TIMEOUT_MS).then(function (r) { return parseTrace(r.body); }).catch(function () { return null; });
const results = await Promise.all(CHECKS.map(async function (c) {
try {
const r = await c.fn();
log(c.name + ' -> ' + r.status + ' (' + r.detail + ')');
return { name: c.name, status: r.status, detail: r.detail };
} catch (e) {
log(c.name + ' error: ' + e);
return { name: c.name, status: 'unknown', detail: String((e && e.message) || e) };
}
}));
let loc = null;
try { loc = await tracePromise; } catch (e) { /* ignore */ }
const okCount = results.filter(function (r) { return r.status === 'ok'; }).length;
const partialCount = results.filter(function (r) { return r.status === 'partial'; }).length;
const policy = selectedPolicy();
const secs = ((Date.now() - startAll) / 1000).toFixed(1);
const nodeLine = policy || '未知';
const exitLabel = loc ? '出口 ' + (flagFor(loc) ? flagFor(loc) + ' ' : '') + loc : '出口未知';
const summary = exitLabel + ' · ' + okCount + '/' + results.length + ' 已解锁' + (partialCount ? ' · ' + partialCount + ' 仅自制剧' : '') + ' · ' + secs + 's';
const verdict = okCount === results.length ? '✅ 全部已解锁' : (okCount * 2 >= results.length ? '⚠️ 部分已解锁' : '❌ 大多未解锁');
const rowsPlain = results.map(function (r) {
return r.name + ': ' + ICONS[r.status] + ' ' + LABELS[r.status] + ' (' + r.detail + ')';
}).join('\n');
const plain = rowsPlain + '\n\n📋 检测结论\n\n' + verdict + '\n' + summary + '\n\n节点: ' + nodeLine;
function videoLine(r) {
if (r.status === 'ok') return '<b>' + r.name + '：已解锁 🎉</b>';
if (r.status === 'partial') return '<b>' + r.name + '：仅自制剧 ⚠️（' + r.detail + '）</b>';
if (r.status === 'blocked') return '<b>' + r.name + '：未解锁 🚫（' + r.detail + '）</b>';
if (r.status === 'timeout') return '<b>' + r.name + '：检测超时 🚦</b>';
return '<b>' + r.name + '：检测失败 ❗（' + r.detail + '）</b>';
}
const lines = results.map(videoLine);
const html = '<p style="text-align: center; font-family: -apple-system; font-size: large; font-weight: thin">'
+ '------------------------------' + '</br>' + lines.join('</br></br>')
+ '</br>------------------------------</br>'
+ '<b>' + summary + '</b></br>'
+ '<font color=#CD5C5C><b>节点</b> ⇢ ' + nodeLine + '</font>'
+ '</p>';
log(NAME + ' | ' + plain);
if (typeof $done !== 'undefined') {
try { $done({ title: '🎬 视频解锁检测', htmlMessage: html }); }
catch (e) { try { $done(); } catch (e2) { /* ignore */ } }
}
}

main();
