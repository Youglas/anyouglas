/*
脚本名称：雨晨签到

活动入口：ycios.top 每日签到

签到规则：每天签到一次，支持多账号

环境变量：yuchenios_data（BoxJS，格式：账号#密码，多账号用 & 分隔，如 13800000000#密码1&13900000000#密码2）

使用说明：BoxJS 里填好账号即可，不需要抓包、不需要 MITM、不需要重写规则；
         域名直接写死 ycios.top，不读任何自定义域名配置

更新时间：2026-11-26


================ Quantumult X 配置 ================

[task_local]

0 9 * * * https://cdn.jsdelivr.net/gh/Youglas/anyouglas@ab39b73a2839770fc1f116e43b1dfa1a09399fcc/yuchen_checkin.js, tag=雨晨签到, enabled=true

*/


// ---------------------- 一般不动变量区域 ----------------------

const $ = new Env('雨晨ios签到');

const BASE = 'https://ycios.top';

const CK_KEY = 'yuchenios_data';

const CHEERIO_KEY = 'Cheerio_code';

const CHEERIO_URL = 'https://fastly.jsdelivr.net/gh/Sliverkiss/QuantumultX@main/Utils/cheerio.js';

const Notify = 1;  // 0 为关闭通知, 1 为打开通知, 默认为 1

$.messages = [];  // 为通知准备的空数组


// ---------------------- 自定义变量区域 ----------------------

$.is_debug = ($.isNode() ? process.env.IS_DEBUG : $.getdata('is_debug')) || 'false';  // 调试模式

let accounts = [];  // { phone, password }

let succCount = 0;


// 主执行程序

!(async () => {

  // 本脚本只有任务模式，不需要重写规则
  if (typeof $request !== 'undefined') {
    $.log('本脚本只有任务模式，无需重写规则');
    return;
  }

  // 检查账号
  await checkEnv();

  // 加载 Cheerio（解析登录页和积分页）
  $.Cheerio = await loadCheerio();

  // 执行任务
  await main();

})()

  .catch((e) => { $.messages.push(e.message || e); console.log(e); })  // 捕获异常并把原因添加到通知

  .finally(async () => {
    await sendMsg($.messages.join('\n'));  // 推送通知
    $.done();
  });


// 脚本入口函数

async function main() {
  for (let i = 0; i < accounts.length; i++) {
    const acc = accounts[i];
    console.log(`账号[${i + 1}] ${maskPhone(acc.phone)} 开始执行`);

    let ok = false;
    try {
      // 1. 打开登录页拿 token
      const token = await getToken();

      // 2. 账号密码登录，拿 session
      const cookie = await login(acc, token);

      // 3. 签到前积分
      const before = await getUserInfo(cookie);

      // 4. 签到
      const signMsg = await signin(cookie);

      // 5. 签到后积分
      const after = await getUserInfo(cookie);

      const name = after.username || before.username || maskPhone(acc.phone);
      const pBefore = parseInt(before.points, 10) || 0;
      const pAfter = parseInt(after.points, 10) || 0;
      const diff = pAfter - pBefore;
      if (diff > 0) {
        $.messages.push(`用户: ${name} 积分:${pBefore}+${diff}`);
      } else {
        $.messages.push(`用户: ${name} 积分:${pAfter}（${signMsg}）`);
      }
      ok = true;
    } catch (e) {
      $.messages.push(`用户: ${maskPhone(acc.phone)} 签到失败：${e.message || e}`);
      console.log(e);
    }

    if (ok) succCount++;

    // 账号之间隔 2 秒
    if (i < accounts.length - 1) await $.wait(2000);
  }
}


// 打开登录页，取 token
async function getToken() {
  const resp = await http({ url: `${BASE}/login`, method: 'get' });
  const html = resp.body || '';
  if (typeof html !== 'string' || !html) throw new Error('打开登录页失败，返回为空');
  if (/Just a moment|cf-challenge|cf-browser-verification|attention required/i.test(html)) {
    throw new Error('CF 验证拦截：请先在浏览器里过一次验证，再跑任务');
  }
  const q = $.Cheerio.load(html);
  const token = q('input[name="token"]').attr('value');
  if (!token) throw new Error('获取 token 失败：登录页里找不到 token（页面结构变了）');
  return token;
}


// 账号密码登录，返回 session（Cookie）
async function login(acc, token) {
  const body = `user_login=${encodeURIComponent(acc.phone)}&password=${encodeURIComponent(acc.password)}&rememberme=1&redirect=${encodeURIComponent(BASE + '/')}&action=userlogin_form&token=${encodeURIComponent(token)}`;
  const resp = await http({
    url: `${BASE}/wp-admin/admin-ajax.php`,
    method: 'post',
    body,
    headers: { 'X-Requested-With': 'XMLHttpRequest' }
  });
  const headers = lowerKeys(resp.headers || {});
  let setCookie = headers['set-cookie'] || '';
  if (Array.isArray(setCookie)) setCookie = setCookie.join(' ');
  const found = (typeof setCookie === 'string' ? setCookie : '').match(/(wordpress_(sec|logged_in)_[a-f0-9]{32})=.+?;/g) || [];
  if (!found.length) {
    throw new Error('获取 session 失败：登录没通过（账号或密码不对，或 token 失效）');
  }
  return found.join(' ');
}


// 查用户信息（用户名 + 积分）
async function getUserInfo(cookie) {
  const resp = await http({
    url: `${BASE}/users?tab=credit`,
    method: 'get',
    headers: { 'Cookie': cookie }
  });
  const html = resp.body || '';
  if (typeof html !== 'string' || !html) throw new Error('读取用户信息失败，返回为空');
  const q = $.Cheerio.load(html);
  const username = q('.name_user').text().trim();
  const points = q('.user_setting_num p:nth-child(2) em').text().trim();
  if (!username && !points) throw new Error('读取用户信息失败：登录态失效或页面结构变了');
  return { username, points: points || '0' };
}


// 签到
async function signin(cookie) {
  const resp = await http({
    url: `${BASE}/wp-admin/admin-ajax.php`,
    method: 'post',
    body: 'action=daily_sign',
    headers: { 'Cookie': cookie, 'X-Requested-With': 'XMLHttpRequest' }
  });
  const obj = $.toObj(resp.body);
  const msg = (obj && (obj.msg || obj.message)) || (typeof resp.body === 'string' ? resp.body.slice(0, 80) : '');
  $.log(`签到返回：${msg}`);
  return msg || '签到请求已发送';
}


// 加载 Cheerio（有缓存直接用，没有就下载一次）
async function loadCheerio() {
  let code = $.getdata(CHEERIO_KEY) || '';
  if (code) {
    $.log(`缓存中存在 Cheerio 模块，跳过下载`);
    eval(code);
    return createCheerio();
  }
  $.log(`开始下载 Cheerio 模块`);
  let fn = await $.getScript(CHEERIO_URL);
  if (!fn) throw new Error('Cheerio 模块下载失败');
  $.setdata(fn, CHEERIO_KEY);
  eval(fn);
  $.log(`Cheerio 模块加载成功`);
  return createCheerio();
}


// 检查账号
async function checkEnv() {
  const raw = $.isNode() ? process.env[CK_KEY] : $.getdata(CK_KEY);
  accounts = parseAccounts(raw);
  if (!accounts.length) {
    throw new Error(`❌ 未找到账号：请在 BoxJS 里填写 ${CK_KEY}，格式：账号#密码，多账号用 & 分隔`);
  }
  console.log(`\n检测到 ${accounts.length} 个账号\n`);
  return accounts.length;
}


// 解析 BoxJS 里的账号：账号#密码，多账号用 & 分隔
function parseAccounts(raw) {
  if (!raw) return [];
  if (Array.isArray(raw)) return raw.filter(a => a && a.phone && a.password);
  if (typeof raw !== 'string') return [];
  return raw.split('&').map(s => s.trim()).filter(Boolean).map(item => {
    const i = item.indexOf('#');
    if (i < 0) return null;
    const phone = item.slice(0, i).trim();
    const password = item.slice(i + 1).trim();
    return (phone && password) ? { phone, password } : null;
  }).filter(Boolean);
}


// 手机号脱敏：138****0000
function maskPhone(phone) {
  if (/^\d{11}$/.test(phone)) return phone.replace(/(\d{3})\d{4}(\d{4})/, '$1****$2');
  return phone;
}


// 响应头 key 转小写
function lowerKeys(obj) {
  return Object.fromEntries(Object.entries(obj).map(([k, v]) => [k.toLowerCase(), v]));
}


// 请求封装：不跟随跳转（登录 302 的 set-cookie 要直接读）；报错说清楚原因
async function http(opts) {
  const req = {
    url: opts.url,
    headers: Object.assign(baseHeaders(), opts.headers || {}),
    opts: { redirection: false },
    timeout: 15000
  };
  if (opts.body) req.body = opts.body;
  try {
    // 注意：$.get/$.post 是回调式，返回 Promise 的是 $.http.get/$.http.post
    return opts.method === 'post' ? await $.http.post(req) : await $.http.get(req);
  } catch (e) {
    const msg = (e && e.message) || String(e || '');
    if (/9807|SSL/i.test(msg)) {
      throw new Error(`网络请求失败（SSL -9807）：请检查 QX 的 MITM 列表里是否还有 ycios.top，有就删掉，删完重启 QX 再试`);
    }
    throw new Error(`网络请求失败：${msg || '未知错误'}`);
  }
}


// 基础请求头
function baseHeaders() {
  return {
    'User-Agent': 'Mozilla/5.0 (iPhone; CPU iPhone OS 15_4_1 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.4 Mobile/15E148 Safari/604.1',
    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
    'Accept-Language': 'zh-CN,zh-Hans;q=0.9',
    'Accept-Encoding': 'gzip, deflate, br',
    'Connection': 'keep-alive',
    'Referer': `${BASE}/`,
    'Origin': BASE
  };
}


// 发送消息
async function sendMsg(message) {
  if (!message) return;

  message = message.replace(/\n+$/, '');  // 清除末尾换行

  const subtitle = `共${accounts.length}个账号，成功${succCount}个，失败${accounts.length - succCount}个`;

  if (Notify > 0) {
    if ($.isNode()) {
      try {
        var notify = require('./sendNotify');
      } catch (e) {
        var notify = require('./utils/sendNotify');
      }

      await notify.sendNotify($.name, subtitle + '\n' + message);
    } else {
      $.msg($.name, subtitle, message);
    }
  } else {
    console.log(message);
  }
}


// DEBUG
function debug(content, title = "debug") {
  let start = `\n----- ${title} -----\n`;
  let end = `\n----- ${$.time('HH:mm:ss')} -----\n`;

  if ($.is_debug === 'true') {
    if (typeof content == "string") {
      console.log(start + content + end);
    } else if (typeof content == "object") {
      console.log(start + $.toStr(content) + end);
    }
  }
}


// prettier-ignore
function Env(t, e) { class s { constructor(t) { this.env = t } send(t, e = "GET") { t = "string" == typeof t ? { url: t } : t; let s = this.get; return "POST" === e && (s = this.post), new Promise((e, i) => { s.call(this, t, (t, s, r) => { t ? i(t) : e(s) }) }) } get(t) { return this.send.call(this.env, t) } post(t) { return this.send.call(this.env, t, "POST") } } return new class { constructor(t, e) { this.name = t, this.http = new s(this), this.data = null, this.dataFile = "box.dat", this.logs = [], this.isMute = !1, this.isNeedRewrite = !1, this.logSeparator = "\n", this.encoding = "utf-8", this.startTime = (new Date).getTime(), Object.assign(this, e), this.log("", `\ud83d\udd14${this.name}, \u5f00\u59cb!`) } isNode() { return "undefined" != typeof module && !!module.exports } isQuanX() { return "undefined" != typeof $task } isSurge() { return "undefined" != typeof $httpClient && "undefined" == typeof $loon } isLoon() { return "undefined" != typeof $loon } isShadowrocket() { return "undefined" != typeof $rocket } isStash() { return "undefined" != typeof $environment && $environment["stash-version"] } toObj(t, e = null) { try { return JSON.parse(t) } catch { return e } } toStr(t, e = null) { try { return JSON.stringify(t) } catch { return e } } getjson(t, e) { let s = e; const i = this.getdata(t); if (i) try { s = JSON.parse(this.getdata(t)) } catch { } return s } setjson(t, e) { try { return this.setdata(JSON.stringify(t), e) } catch { return !1 } } getScript(t) { return new Promise(e => { this.get({ url: t }, (t, s, i) => e(i)) }) } runScript(t, e) { return new Promise(s => { let i = this.getdata("@chavy_boxjs_userCfgs.httpapi"); i = i ? i.replace(/\n/g, "").trim() : i; let r = this.getdata("@chavy_boxjs_userCfgs.httpapi_timeout"); r = r ? 1 * r : 20, r = e && e.timeout ? e.timeout : r; const [o, a] = i.split("@"), n = { url: `http://${a}/v1/scripting/evaluate`, body: { script_text: t, mock_type: "cron", timeout: r }, headers: { "X-Key": o, Accept: "*/*" } }; this.post(n, (t, e, i) => s(i)) }).catch(t => this.logErr(t)) } loaddata() { if (!this.isNode()) return {}; { this.fs = this.fs ? this.fs : require("fs"), this.path = this.path ? this.path : require("path"); const t = this.path.resolve(this.dataFile), e = this.path.resolve(process.cwd(), this.dataFile), s = this.fs.existsSync(t), i = !s && this.fs.existsSync(e); if (!s && !i) return {}; { const i = s ? t : e; try { return JSON.parse(this.fs.readFileSync(i)) } catch (t) { return {} } } } } writedata() { if (this.isNode()) { this.fs = this.fs ? this.fs : require("fs"), this.path = this.path ? this.path : require("path"); const t = this.path.resolve(this.dataFile), e = this.path.resolve(process.cwd(), this.dataFile), s = this.fs.existsSync(t), i = !s && this.fs.existsSync(e), r = JSON.stringify(this.data); s ? this.fs.writeFileSync(t, r) : i ? this.fs.writeFileSync(e, r) : this.fs.writeFileSync(t, r) } } lodash_get(t, e, s) { const i = e.replace(/\[(\d+)\]/g, ".$1").split("."); let r = t; for (const t of i) if (r = Object(r)[t], void 0 === r) return s; return r } lodash_set(t, e, s) { return Object(t) !== t ? t : (Array.isArray(e) || (e = e.toString().match(/[^.[\]]+/g) || []), e.slice(0, -1).reduce((t, s, i) => Object(t[s]) === t[s] ? t[s] : t[s] = Math.abs(e[i + 1]) >> 0 == +e[i + 1] ? [] : {}, t)[e[e.length - 1]] = s, t) } getdata(t) { let e = this.getval(t); if (/^@/.test(t)) { const [, s, i] = /^@(.*?)\.(.*?)$/.exec(t), r = s ? this.getval(s) : ""; if (r) try { const t = JSON.parse(r); e = t ? this.lodash_get(t, i, "") : e } catch (t) { e = "" } } return e } setdata(t, e) { let s = !1; if (/^@/.test(e)) { const [, i, r] = /^@(.*?)\.(.*?)$/.exec(e), o = this.getval(i), a = i ? "null" === o ? null : o || "{}" : "{}"; try { const e = JSON.parse(a); this.lodash_set(e, r, t), s = this.setval(JSON.stringify(e), i) } catch (e) { const o = {}; this.lodash_set(o, r, t), s = this.setval(JSON.stringify(o), i) } } else s = this.setval(t, e); return s } getval(t) { return this.isSurge() || this.isLoon() ? $persistentStore.read(t) : this.isQuanX() ? $prefs.valueForKey(t) : this.isNode() ? (this.data = this.loaddata(), this.data[t]) : this.data && this.data[t] || null } setval(t, e) { return this.isSurge() || this.isLoon() ? $persistentStore.write(t, e) : this.isQuanX() ? $prefs.setValueForKey(t, e) : this.isNode() ? (this.data = this.loaddata(), this.data[e] = t, this.writedata(), !0) : this.data && this.data[e] || null } initGotEnv(t) { this.got = this.got ? this.got : require("got"), this.cktough = this.cktough ? this.cktough : require("tough-cookie"), this.ckjar = this.ckjar ? this.ckjar : new this.cktough.CookieJar, t && (t.headers = t.headers ? t.headers : {}, void 0 === t.headers.Cookie && void 0 === t.cookieJar && (t.cookieJar = this.ckjar)) } get(t, e = (() => { })) { if (t.headers && (delete t.headers["Content-Type"], delete t.headers["Content-Length"]), this.isSurge() || this.isLoon()) this.isSurge() && this.isNeedRewrite && (t.headers = t.headers || {}, Object.assign(t.headers, { "X-Surge-Skip-Scripting": !1 })), $httpClient.get(t, (t, s, i) => { !t && s && (s.body = i, s.statusCode = s.status ? s.status : s.statusCode, s.status = s.statusCode), e(t, s, i) }); else if (this.isQuanX()) this.isNeedRewrite && (t.opts = t.opts || {}, Object.assign(t.opts, { hints: !1 })), $task.fetch(t).then(t => { const { statusCode: s, statusCode: i, headers: r, body: o } = t; e(null, { status: s, statusCode: i, headers: r, body: o }, o) }, t => e(t && t.error || "UndefinedError")); else if (this.isNode()) { let s = require("iconv-lite"); this.initGotEnv(t), this.got(t).on("redirect", (t, e) => { try { if (t.headers["set-cookie"]) { const s = t.headers["set-cookie"].map(this.cktough.Cookie.parse).toString(); s && this.ckjar.setCookieSync(s, null), e.cookieJar = this.ckjar } } catch (t) { this.logErr(t) } }).then(t => { const { statusCode: i, statusCode: r, headers: o, rawBody: a } = t, n = s.decode(a, this.encoding); e(null, { status: i, statusCode: r, headers: o, rawBody: a, body: n }, n) }, t => { const { message: i, response: r } = t; e(i, r, r && s.decode(r.rawBody, this.encoding)) }) } } post(t, e = (() => { })) { const s = t.method ? t.method.toLocaleLowerCase() : "post"; if (t.body && t.headers && !t.headers["Content-Type"] && (t.headers["Content-Type"] = "application/x-www-form-urlencoded"), t.headers && delete t.headers["Content-Length"], this.isSurge() || this.isLoon()) this.isSurge() && this.isNeedRewrite && (t.headers = t.headers || {}, Object.assign(t.headers, { "X-Surge-Skip-Scripting": !1 })), $httpClient[s](t, (t, s, i) => { !t && s && (s.body = i, s.statusCode = s.status ? s.status : s.statusCode, s.status = s.statusCode), e(t, s, i) }); else if (this.isQuanX()) t.method = s, this.isNeedRewrite && (t.opts = t.opts || {}, Object.assign(t.opts, { hints: !1 })), $task.fetch(t).then(t => { const { statusCode: s, statusCode: i, headers: r, body: o } = t; e(null, { status: s, statusCode: i, headers: r, body: o }, o) }, t => e(t && t.error || "UndefinedError")); else if (this.isNode()) { let i = require("iconv-lite"); this.initGotEnv(t); const { url: r, ...o } = t; this.got[s](r, o).then(t => { const { statusCode: s, statusCode: r, headers: o, rawBody: a } = t, n = i.decode(a, this.encoding); e(null, { status: s, statusCode: r, headers: o, rawBody: a, body: n }, n) }, t => { const { message: s, response: r } = t; e(s, r, r && i.decode(r.rawBody, this.encoding)) }) } } time(t, e = null) { const s = e ? new Date(e) : new Date; let i = { "M+": s.getMonth() + 1, "d+": s.getDate(), "H+": s.getHours(), "m+": s.getMinutes(), "s+": s.getSeconds(), "q+": Math.floor((s.getMonth() + 3) / 3), S: s.getMilliseconds() }; /(y+)/.test(t) && (t = t.replace(RegExp.$1, (s.getFullYear() + "").substr(4 - RegExp.$1.length))); for (let e in i) new RegExp("(" + e + ")").test(t) && (t = t.replace(RegExp.$1, 1 == RegExp.$1.length ? i[e] : ("00" + i[e]).substr(("" + i[e]).length))); return t } msg(e = t, s = "", i = "", r) { const o = t => { if (!t) return t; if ("string" == typeof t) return this.isLoon() ? t : this.isQuanX() ? { "open-url": t } : this.isSurge() ? { url: t } : void 0; if ("object" == typeof t) { if (this.isLoon()) { let e = t.openUrl || t.url || t["open-url"], s = t.mediaUrl || t["media-url"]; return { openUrl: e, mediaUrl: s } } if (this.isQuanX()) { let e = t["open-url"] || t.url || t.openUrl, s = t["media-url"] || t.mediaUrl, i = t["update-pasteboard"] || t.updatePasteboard; return { "open-url": e, "media-url": s, "update-pasteboard": i } } if (this.isSurge()) { let e = t.url || t.openUrl || t["open-url"]; return { url: e } } } }; if (this.isMute || (this.isSurge() || this.isLoon() ? $notification.post(e, s, i, o(r)) : this.isQuanX() && $notify(e, s, i, o(r))), !this.isMuteLog) { let t = ["", "==============\ud83d\udce3\u7cfb\u7edf\u901a\u77e5\ud83d\udce3=============="]; t.push(e), s && t.push(s), i && t.push(i), console.log(t.join("\n")), this.logs = this.logs.concat(t) } } log(...t) { t.length > 0 && (this.logs = [...this.logs, ...t]), console.log(t.join(this.logSeparator)) } logErr(t, e) { const s = !this.isSurge() && !this.isQuanX() && !this.isLoon(); s ? this.log("", `\u2757\ufe0f${this.name}, \u9519\u8bef!`, t.stack) : this.log("", `\u2757\ufe0f${this.name}, \u9519\u8bef!`, t) } wait(t) { return new Promise(e => setTimeout(e, t)) } done(t = {}) { const e = (new Date).getTime(), s = (e - this.startTime) / 1e3; this.log("", `\ud83d\udd14${this.name}, \u7ed3\u675f! \ud83d\udd5b ${s} \u79d2`), this.log(), this.isSurge() || this.isQuanX() || this.isLoon() ? $done(t) : this.isNode() && process.exit(1) } }(t, e) }
