/*
================================================================
ai_unlock_check.js — Quantumult X 检测主流 AI 是否已解锁
@Author: Tuesday
@Date: 2026-11-01 (v3: 界面美化，QX 原生标签渲染：大字汇总、彩色状态、分隔线；逻辑同 v1)
@Description:
检测当前出口 IP（所选策略/节点）能否打开主流 AI 服务。
每个服务并行检测，7 秒超时，总时长约 8 秒，不会
出现测速脚本那种长耗时导致的 no results。
检测方法（2026-11-01 沙箱实测 + 官方错误码文档）：
- ChatGPT 网页：GET https://chatgpt.com/ ，被封地区
返回 text/plain 的 "not available" 页；可用地区
返回 HTML。另取 chatgpt.com/cdn-cgi/trace 的
loc= 得到出口国家，显示在结果里。
- OpenAI API：GET https://api.openai.com/v1/models ，
无 key 时 401（Missing bearer <redacted>）= 服务可达、
已解锁；403 且 body 含 "not supported" = 地区
不支持（官方 403 Country not supported）。
- Claude 网页：claude.ai/cdn-cgi/trace 的 loc= 对照
Anthropic 不支持名单（CN/HK/MO/RU/BY/IR/SY/KP/
CU/VE 等）；首页 200 只作辅助。Claude 的地区
限制在登录后也会再查一次，本脚本只能判断出口。
- Anthropic API：GET https://api.anthropic.com/v1/models ，
401（x-api-key header is required）= 已解锁。
- Gemini 网页：GET https://gemini.google.com/ ，body 含
"isn't currently supported" = 未解锁；200 HTML = 已解锁。
网页版与 API 的国家名单不同（香港在网页名单、
不在 API 名单），所以 Gemini 同时测 API。
- Gemini API：GET https://generativelanguage.googleapis.com/
v1beta/models ，无 key 时 403 "unregistered callers"
= 可达；400 "User location is not supported" = 地区不支持。
- Grok：grok.com 首页 200 = 已解锁；xAI API
https://api.x.ai/v1/models 401 "No credentials" = 已解锁。
- Copilot：copilot.microsoft.com 首页 200 = 已解锁。
- Perplexity：perplexity.ai 首页 200 = 已解锁。
- Mistral Le Chat：chat.mistral.ai 首页 200 = 已解锁。
注意：只测“出口 IP 能否打开服务”，不代表账号
能注册/登录（手机号、支付、账号风控另算）。Meta AI
未列入：首页返回人机验证 403，脚本无法判断。

手动点测（固定 commit 地址，绕过 QX
24 小时远程缓存）：
[task_local]
event-interaction https://cdn.jsdelivr.net/gh/Youglas/anyouglas@master/ai_unlock_check.js, tag=AI解锁检测, img-url=checkmark.seal.system, enabled=true
================================================================
*/

const NAME = 'AI 解锁检测';
const UA = 'Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Mobile/15E148 Safari/604.1';
const TIMEOUT_MS = 7000;
const CLAUDE_BLOCKED = ['CN', 'HK', 'MO', 'RU', 'BY', 'IR', 'SY', 'KP', 'CU', 'VE', 'AF', 'MM'];

function log(msg) {
if (typeof console!== 'undefined' && console.log) console.log('[' + NAME + '] ' + msg);
}

function selectedPolicy() {
try {
if (typeof $environment!== 'undefined' && $environment && typeof $environment.params === 'string' && $environment.params) return $environment.params;
} catch (e) { /* ignore */}
return null;
}

function normalizeHeaders(headers) {
const out = {};
if (!headers) return out;
Object.keys(headers).forEach(function (k) { out[String(k).toLowerCase()] = headers[k];});
return out;
}

function httpGet(url, timeoutMs) {
const start = Date.now();
return new Promise(function (resolve) {
let settled = false;
const timer = setTimeout(function () {
if (!settled) { settled = true; resolve({ ok: false, error: 'timeout', status: 0, headers: {}, body: '', ms: Date.now() - start});}
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
const status = resp.statusCode || 0;
finish({ ok: status >= 200 && status < 300, status: status, headers: headers, body: resp.body || ''});
}).catch(function (e) {
finish({ ok: false, error: String((e && (e.error || e.message)) || e), status: 0, headers: {}, body: ''});
});
} else if (typeof $httpClient!== 'undefined' && $httpClient.get) {
$httpClient.get({ url: url, headers: { 'User-Agent': UA, 'Accept': '*/*'}}, function (err, resp, respBody) {
if (err) finish({ ok: false, error: String(err), status: 0, headers: {}, body: ''});
else {
const headers = normalizeHeaders((resp && resp.headers) || {});
const status = (resp && (resp.status || resp.statusCode)) || 0;
finish({ ok: status >= 200 && status < 300, status: status, headers: headers, body: respBody || ''});
}
});
} else {
finish({ ok: false, error: 'no http client', status: 0, headers: {}, body: ''});
}
} catch (e) {
finish({ ok: false, error: String((e && e.message) || e), status: 0, headers: {}, body: ''});
}
});
}

function parseTrace(body) {
const m = body? body.match(/loc=([A-Z]{2})/): null;
return m? m[1]: null;
}

function isBlockedText(body) {
const b = (body || '').toLowerCase();
return b.indexOf('not available in your country') > -1
|| b.indexOf('is not available in your country') > -1
|| b.indexOf("isn't currently supported") > -1
|| b.indexOf('country, region, or territory not supported') > -1
|| b.indexOf('unsupported country') > -1
|| b.indexOf('not available in your region') > -1;
}

// 每个检测返回 {status:'ok'|'blocked'|'timeout'|'unknown', detail:'US'|'HTTP 401'...}
async function checkChatGPT() {
const trace = await httpGet('https://chatgpt.com/cdn-cgi/trace', TIMEOUT_MS);
const loc = parseTrace(trace.body);
const home = await httpGet('https://chatgpt.com/', TIMEOUT_MS);
if (home.error === 'timeout' && trace.error === 'timeout') return { status: 'timeout', detail: '超时'};
if (isBlockedText(home.body) || (home.headers['content-type'] || '').indexOf('text/plain') > -1) return { status: 'blocked', detail: loc || '地区受限'};
if (home.status === 200 && home.body) return { status: 'ok', detail: loc || 'HTTP 200'};
if (home.status === 403 && isBlockedText(home.body)) return { status: 'blocked', detail: loc || 'HTTP 403'};
if (home.status) return { status: 'unknown', detail: 'HTTP ' + home.status};
return { status: 'unknown', detail: home.error || '无响应'};
}

async function checkOpenAIAPI() {
const r = await httpGet('https://api.openai.com/v1/models', TIMEOUT_MS);
if (r.error === 'timeout') return { status: 'timeout', detail: '超时'};
if (r.status === 401) return { status: 'ok', detail: 'HTTP 401（需 key）'};
if (r.status === 403 && isBlockedText(r.body)) return { status: 'blocked', detail: '地区不支持'};
if (r.status === 403) return { status: 'blocked', detail: 'HTTP 403'};
if (r.status === 200) return { status: 'ok', detail: 'HTTP 200'};
if (r.status) return { status: 'unknown', detail: 'HTTP ' + r.status};
return { status: 'unknown', detail: r.error || '无响应'};
}

async function checkClaude() {
const trace = await httpGet('https://claude.ai/cdn-cgi/trace', TIMEOUT_MS);
const loc = parseTrace(trace.body);
const home = await httpGet('https://claude.ai/', TIMEOUT_MS);
if (loc && CLAUDE_BLOCKED.indexOf(loc) > -1) return { status: 'blocked', detail: '出口 ' + loc};
if (isBlockedText(home.body)) return { status: 'blocked', detail: loc || '地区受限'};
if (loc) return { status: 'ok', detail: '出口 ' + loc};
if (home.status === 200) return { status: 'ok', detail: 'HTTP 200'};
if (home.error === 'timeout') return { status: 'timeout', detail: '超时'};
return { status: 'unknown', detail: home.error || ('HTTP ' + home.status)};
}

async function checkAnthropicAPI() {
const r = await httpGet('https://api.anthropic.com/v1/models', TIMEOUT_MS);
if (r.error === 'timeout') return { status: 'timeout', detail: '超时'};
if (r.status === 401) return { status: 'ok', detail: 'HTTP 401（需 key）'};
if (r.status === 403) return { status: 'blocked', detail: 'HTTP 403'};
if (r.status) return { status: 'unknown', detail: 'HTTP ' + r.status};
return { status: 'unknown', detail: r.error || '无响应'};
}

async function checkGemini() {
const results = await Promise.all([
httpGet('https://gemini.google.com/', TIMEOUT_MS),
httpGet('https://generativelanguage.googleapis.com/v1beta/models', TIMEOUT_MS)
]);
const home = results[0];
const api = results[1];
const apiBlocked = api.status === 400 && (api.body || '').indexOf('User location is not supported') > -1;
const homeBlocked = isBlockedText(home.body);
if (homeBlocked || apiBlocked) return { status: 'blocked', detail: '地区不支持'};
if (home.status === 200 && home.body) return { status: 'ok', detail: '网页 + API 可达'};
if (api.status === 403 && (api.body || '').indexOf('unregistered callers') > -1) return { status: 'ok', detail: 'API 可达（需 key）'};
if (home.error === 'timeout' && api.error === 'timeout') return { status: 'timeout', detail: '超时'};
if (api.status) return { status: 'unknown', detail: 'API HTTP ' + api.status};
return { status: 'unknown', detail: home.error || api.error || '无响应'};
}

async function checkHomepage(name, url) {
const r = await httpGet(url, TIMEOUT_MS);
if (r.error === 'timeout') return { status: 'timeout', detail: '超时'};
if (isBlockedText(r.body)) return { status: 'blocked', detail: '地区受限'};
if (r.status === 200 && r.body) return { status: 'ok', detail: 'HTTP 200'};
if (r.status === 403) return { status: 'blocked', detail: 'HTTP 403'};
if (r.status) return { status: 'unknown', detail: 'HTTP ' + r.status};
return { status: 'unknown', detail: r.error || '无响应'};
}

async function checkGrok() {
const results = await Promise.all([
httpGet('https://grok.com/', TIMEOUT_MS),
httpGet('https://api.x.ai/v1/models', TIMEOUT_MS)
]);
const home = results[0];
const api = results[1];
if (isBlockedText(home.body)) return { status: 'blocked', detail: '地区受限'};
if (api.status === 401) return { status: 'ok', detail: 'API HTTP 401（需 key）'};
if (home.status === 200 && home.body) return { status: 'ok', detail: 'HTTP 200'};
if (home.error === 'timeout' && api.error === 'timeout') return { status: 'timeout', detail: '超时'};
if (api.status) return { status: 'unknown', detail: 'API HTTP ' + api.status};
return { status: 'unknown', detail: home.error || '无响应'};
}

const CHECKS = [
{ name: 'ChatGPT', fn: checkChatGPT},
{ name: 'OpenAI API', fn: checkOpenAIAPI},
{ name: 'Claude', fn: checkClaude},
{ name: 'Anthropic API', fn: checkAnthropicAPI},
{ name: 'Gemini', fn: checkGemini},
{ name: 'Grok', fn: checkGrok},
{ name: 'Copilot', fn: function () { return checkHomepage('Copilot', 'https://copilot.microsoft.com/');}},
{ name: 'Perplexity', fn: function () { return checkHomepage('Perplexity', 'https://www.perplexity.ai/');}},
{ name: 'Mistral', fn: function () { return checkHomepage('Mistral', 'https://chat.mistral.ai/');}}
];

const ICONS = { ok: '✅', blocked: '❌', timeout: '⏱️', unknown: '❓'};
const LABELS = { ok: '已解锁', blocked: '未解锁', timeout: '超时', unknown: '未知'};

async function main() {
const startAll = Date.now();
const results = await Promise.all(CHECKS.map(async function (c) {
try {
const r = await c.fn();
log(c.name + ' -> ' + r.status + ' (' + r.detail + ')');
return { name: c.name, status: r.status, detail: r.detail};
} catch (e) {
log(c.name + ' error: ' + e);
return { name: c.name, status: 'unknown', detail: String((e && e.message) || e)};
}
}));
const okCount = results.filter(function (r) { return r.status === 'ok';}).length;
const policy = selectedPolicy();
const secs = ((Date.now() - startAll) / 1000).toFixed(1);
const plainLines = results.map(function (r) {
return ICONS[r.status] + ' ' + r.name + '：' + LABELS[r.status] + '（' + r.detail + '）';
});
const footer = okCount + '/' + results.length + ' 已解锁 · ' + secs + 's' + (policy? ' · ' + policy: '');
const COLORS = { ok: '#137333', blocked: '#A50E0E', timeout: '#B06000', unknown: '#666666' };
const rows = results.map(function (r) {
return '<b>' + r.name + '</b>　<font color="' + COLORS[r.status] + '"><b>' + ICONS[r.status] + ' ' + LABELS[r.status] + '</b></font></br><font color="#8E8E93" size="2">' + r.detail + '</font>';
}).join('</br></br>');
const html = '<p style="text-align: center; font-family: -apple-system; font-size: large">'
+ '<b><font color="#5856D6">🤖 AI 解锁检测</font></b> <font color="#AEAEB2" size="2">v3</font></br>'
+ '──────────────────────</br>'
+ '<font color="#5856D6"><span style="font-size:42px; font-weight:bold">' + okCount + ' / ' + results.length + '</span></font></br>'
+ '<font color="#8E8E93">项服务已解锁</font></br>'
+ '──────────────────────</br></p>'
+ '<p style="text-align: left; font-family: -apple-system; font-size: large">'
+ rows
+ '</p>'
+ '<p style="text-align: center; font-family: -apple-system; font-size: large">'
+ '──────────────────────</br>'
+ '<font color="#8E8E93">' + footer + '</font></br>'
+ '<font color="#AEAEB2" size="2">仅测当前出口 IP 能否打开服务，不代表账号能登录</font>'
+ '</p>';
const plain = plainLines.join('\n') + '\n' + footer;
log(NAME + ' | ' + plain);
if (typeof $done!== 'undefined') {
try { $done({ title: NAME, message: plain, htmlMessage: html});}
catch (e) { try { $done();} catch (e2) { /* ignore */}}
}
}

main();
