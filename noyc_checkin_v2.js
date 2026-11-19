/*
脚本名称：草丛签到

活动入口：草丛网站 go.noyc.eu 签到

签到规则：每天签到一次，支持 4 个账号

活动奖励：签到流量奖励（以网站返回为准）

环境变量：noyc_cookie（QX 环境，多账号以@隔开）

使用说明：添加重写规则后，在 Safari 登录账号并手动点一次签到即可获取 Cookie 和 CSRF Token；任务会在签到前先试存的 Token，遇到 CSRF 报错自动刷新页面取新 Token 再试一次

更新时间：2026-11-20


================ Surge 配置 ================

[MITM]

hostname = %APPEND% go.noyc.eu


[Script]

获取草丛Cookie = type=http-request, pattern=^https:\/\/go\.noyc\.eu\/api\/checkin, requires-body=1, max-size=0, script-path=https://cdn.jsdelivr.net/gh/Youglas/anyouglas@d49e3db8fc30477810756cb48f330fe7699144a4/noyc_checkin_v2.js

草丛签到 = type=cron, cronexp=0 9 * * *, timeout=60, script-path=https://cdn.jsdelivr.net/gh/Youglas/anyouglas@d49e3db8fc30477810756cb48f330fe7699144a4/noyc_checkin_v2.js, script-update-interval=0


============ Quantumult X 配置 =============

[MITM]

hostname = %APPEND% go.noyc.eu


[rewrite_local]

^https:\/\/go\.noyc\.eu\/api\/checkin url script-request-body https://cdn.jsdelivr.net/gh/Youglas/anyouglas@d49e3db8fc30477810756cb48f330fe7699144a4/noyc_checkin_v2.js


[task_local]

0 9 * * * https://cdn.jsdelivr.net/gh/Youglas/anyouglas@d49e3db8fc30477810756cb48f330fe7699144a4/noyc_checkin_v2.js, tag=草丛签到, enabled=true


================ Loon 配置 ================

[MITM]

hostname = go.noyc.eu

cron "0 9 * * *" script-path=https://cdn.jsdelivr.net/gh/Youglas/anyouglas@d49e3db8fc30477810756cb48f330fe7699144a4/noyc_checkin_v2.js, tag=草丛签到

http-request ^https:\/\/go\.noyc\.eu\/api\/checkin script-path=https://cdn.jsdelivr.net/gh/Youglas/anyouglas@d49e3db8fc30477810756cb48f330fe7699144a4/noyc_checkin_v2.js, requires-body=true, timeout=10, enabled=false, tag=获取草丛Cookie

*/


// ---------------------- 一般不动变量区域 ----------------------

const $ = new Env('草丛签到');

const origin = 'https://go.noyc.eu';

const noyc_cookie_key = 'noyc_cookie';

const noyc_csrf_key = 'noyc_csrf';  // 每账号的 CSRF 信息（请求头 + 请求体 + URL 参数），与 noyc_cookie 按下标对应

const Notify = 1;  // 0 为关闭通知, 1 为打开通知, 默认为 1

$.messages = [];  // 为通知准备的空数组


// ---------------------- 自定义变量区域 ----------------------

$.is_debug = ($.isNode() ? process.env.IS_DEBUG : $.getdata('is_debug')) || 'false';  // 调试模式

let cookie = ($.isNode() ? process.env.noyc_cookie : $.getdata(noyc_cookie_key)) || '', cookieArr = [];

let csrf = ($.isNode() ? process.env.noyc_csrf : $.getdata(noyc_csrf_key)) || '', csrfArr = [];


// 统一管理 api 接口

const Api = {

  // 签到
  "checkin": {
    "url": "/api/checkin",
  }

}


// 主执行程序

!(async () => {

  // 检查变量
  await checkEnv();

  // 获取 Cookie
  if (isGetCookie = typeof $request !== 'undefined') {
    GetCookie();
    return;
  }

  // 未检测到账号变量, 退出
  if (!cookieArr[0]) {
    throw new Error(`❌ 未获取到 Cookie, 请先登录并手动点一次签到`);
  } else {
    // 执行任务
    await main();
  }

})()

  .catch((e) => $.messages.push(e.message || e) && console.log(e))  // 捕获登录函数等抛出的异常, 并把原因添加到全局变量(通知)
  .finally(async () => {
    await sendMsg($.messages.join('\n'));  // 推送通知
    $.done();
  })


// 脚本入口函数

async function main() {
  for (let i = 0; i < cookieArr.length; i++) {
    console.log(`账号[${i + 1}]开始执行`);

    // 变量初始化
    $.message = '';
    $.result = '';
    $.cookie = cookieArr[i];
    $.idx = i;

    // 签到
    await checkin();

    // 打印结果
    console.log($.result);

    // 等待 1 秒
    await $.wait(1000 * 1);

    // 拼接通知消息
    $.messages.push(`账号[${i + 1}] ${$.result.replace(/\n$/, '')}`);

    // 每个账号间隔 3 秒
    await $.wait(1000 * 3);
  }
}


// 获取数据
function GetCookie() {
  if ($request && $request.headers) {
    let headers = $request.headers;
    let ck = headers['Cookie'] || headers['cookie'] || '';
    if (ck) {
      // 连 CSRF Token 一起抓：请求头里含 csrf/xsrf/token 的字段 + 请求体 + URL 参数
      let csrfHeaders = {};
      for (let k in headers) {
        if (/csrf|xsrf|token/i.test(k) && k.toLowerCase() !== 'cookie') {
          csrfHeaders[k] = headers[k];
        }
      }
      let body = (typeof $request.body === 'string') ? $request.body : '';
      let q = '';
      if ($request.url) {
        let qi = $request.url.indexOf('?');
        if (qi !== -1) q = $request.url.slice(qi);
      }
      let info = encodeURIComponent(JSON.stringify({ h: csrfHeaders, b: body, q: q }));
      let arr = cookie ? cookie.split('@') : [];
      let cArr = csrf ? csrf.split('@') : [];
      while (cArr.length < arr.length) cArr.push('');
      let idx = arr.indexOf(ck);
      if (idx === -1) {
        cookie ? cookie += `@${ck}` : cookie += `${ck}`;
        cArr.push(info);
        csrf = cArr.join('@');
        $.setdata(cookie, noyc_cookie_key);
        $.setdata(csrf, noyc_csrf_key);
        console.log(`Cookie: ${hideSensitiveData(ck, 6, 6)} \n`);
        $.messages.push(`🎉 Cookie 写入成功（共 ${cookie.split('@').length} 个账号）`);
      } else {
        // 已存在的账号：更新它的 Token 信息
        cArr[idx] = info;
        csrf = cArr.join('@');
        $.setdata(csrf, noyc_csrf_key);
        console.log(`❌ 该账号 Cookie 已存在，已更新 Token\n`);
        $.messages.push(`🔄 账号[${idx + 1}] Token 已更新`);
      }
    }
  }
}


// 签到
async function checkin() {
  let info = parseCsrfInfo(csrfArr[$.idx] || '');
  let result = await doCheckin(info);
  debug(result, "checkin");

  // CSRF 报错：刷新页面取新 Token，再试一次
  if (isCsrfError(result)) {
    let fresh = await refreshToken();
    if (fresh) {
      let info2 = applyFreshToken(info, fresh);
      result = await doCheckin(info2);
      debug(result, "checkin-retry");
      // 重试过了 CSRF 校验：把新 Token 存下来
      if (!isCsrfError(result)) {
        csrfArr[$.idx] = encodeURIComponent(JSON.stringify(info2));
        csrf = csrfArr.join('@');
        $.setdata(csrf, noyc_csrf_key);
      }
    }
  }

  if (result && typeof result === 'object') {
    let m = result.message || result.msg || result.error || result.data || '';
    if (m && typeof m === 'string') {
      $.result += `${m}\n`;
    } else if (result.success === true || result.ret === 1 || result.code === 0) {
      $.result += `签到成功\n`;
    } else {
      // 无法识别的返回：把原始内容打出来，方便定位原因
      let raw = $.toStr(result) || '';
      $.result += `签到未成功：${raw.slice(0, 150)}\n`;
    }
  } else if (typeof result === 'string' && result) {
    if (result.indexOf('/auth/login') !== -1 || result.indexOf('<html') !== -1) {
      $.result += `Cookie 已过期，请重新抓取\n`;
    } else {
      $.result += `${result.slice(0, 40)}\n`;
    }
  } else {
    $.result += `请求失败或返回为空\n`;
  }
}


// 解析存的 CSRF 信息
function parseCsrfInfo(s) {
  let info = { h: {}, b: '', q: '' };
  if (s) {
    try {
      let o = JSON.parse(decodeURIComponent(s));
      if (o && typeof o === 'object') {
        info.h = o.h || {};
        info.b = o.b || '';
        info.q = o.q || '';
      }
    } catch (e) { }
  }
  return info;
}


// 发一次签到请求（带存的 Token）
async function doCheckin(info) {
  let url = Api.checkin.url + (info.q || '');
  let extra = Object.assign({}, info.h, { 'Cookie': $.cookie });
  let body = info.b || '';
  return await httpRequest(options(url, body, extra));
}


// 是否 CSRF 报错
function isCsrfError(result) {
  if (!result) return false;
  let s = (typeof result === 'string') ? result : $.toStr(result);
  return /csrf/i.test(s || '');
}


// 刷新页面取新 Token
async function refreshToken() {
  let html = await new Promise((resolve) => {
    $.get({ url: `${origin}/`, headers: { 'Cookie': $.cookie, 'Accept': 'text/html', 'User-Agent': 'Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Mobile/15E148 Safari/604.1' }, timeout: 10000 }, (err, resp, data) => {
      if (err) { console.log(`❌ 刷新页面失败`); $.logErr(err); resolve(''); }
      else resolve(data || '');
    });
  });
  if (!html || typeof html !== 'string') return '';
  let m = html.match(/<meta[^>]*name=["']csrf-token["'][^>]*content=["']([^"']+)["']/i)
    || html.match(/<input[^>]*name=["'](?:_token|csrf_token)["'][^>]*value=["']([^"']+)["']/i)
    || html.match(/<input[^>]*value=["']([^"']+)["'][^>]*name=["'](?:_token|csrf_token)["']/i)
    || html.match(/["']csrf[-_]?token["']\s*[:=]\s*["']([^"']+)["']/i)
    || html.match(/["']_token["']\s*[:=]\s*["']([^"']+)["']/i);
  return m ? m[1] : '';
}


// 把新 Token 填进原来放 Token 的位置
function applyFreshToken(info, fresh) {
  let out = { h: Object.assign({}, info.h), b: info.b || '', q: info.q || '' };
  let replaced = false;
  for (let k in out.h) {
    if (/csrf|xsrf|token/i.test(k)) { out.h[k] = fresh; replaced = true; }
  }
  if (out.b) {
    try {
      let obj = JSON.parse(out.b);
      if (obj && typeof obj === 'object') {
        for (let k in obj) {
          if (/csrf|xsrf|token/i.test(k)) { obj[k] = fresh; replaced = true; }
        }
        out.b = JSON.stringify(obj);
      }
    } catch (e) {
      let nb = out.b.replace(/((?:_token|csrf_token|csrf-token)=)[^&]*/gi, '$1' + encodeURIComponent(fresh));
      if (nb !== out.b) { out.b = nb; replaced = true; }
    }
  }
  if (out.q) {
    let nq = out.q.replace(/((?:_token|csrf_token|csrf-token)=)[^&]*/gi, '$1' + encodeURIComponent(fresh));
    if (nq !== out.q) { out.q = nq; replaced = true; }
  }
  if (!replaced) {
    // 原请求里没找到放 Token 的位置：按最常见的方式补一个请求头
    out.h['X-CSRF-TOKEN'] = fresh;
  }
  return out;
}


// 检查变量
async function checkEnv() {
  // 多账号分割
  cookieArr = cookie.split('@');
  csrfArr = csrf ? csrf.split('@') : [];
  while (csrfArr.length < cookieArr.length) csrfArr.push('');

  // 当下标0为空字符串也会占用长度，所以需判断是否为空字符串
  if (cookieArr[0]) {
    console.log(`\n检测到 ${cookieArr.length} 个账号变量\n`);
    return cookieArr.length;
  } else {
    console.log(`\n检测到 0 个账号变量\n`);
    return 0;
  }
}


// 发送消息
async function sendMsg(message) {
  if (!message) return;

  message = message.replace(/\n+$/, '');  // 清除末尾换行

  if (Notify > 0) {
    if ($.isNode()) {
      try {
        var notify = require('./sendNotify');
      } catch (e) {
        var notify = require('./utils/sendNotify');
      }

      await notify.sendNotify($.name, message);
    } else {
      $.msg($.name, '', message);
    }
  } else {
    console.log(message);
  }
}


// 封装请求参数
function options(url, body = '', extraHeaders = {}) {
  let opt = {
    url: `${origin}${url}`,
    headers: {
      "Host": "go.noyc.eu",
      "Accept": "application/json, text/plain, */*",
      "Accept-Encoding": "gzip, deflate, br",
      "Connection": "keep-alive",
      "Referer": `${origin}/`,
      "User-Agent": "Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Mobile/15E148 Safari/604.1",
      "Accept-Language": "zh-Hans-CN;q=1",
      ...extraHeaders
    },
    body,
    timeout: 10000
  }

  debug(opt);

  return opt;
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


// 数据脱敏
function hideSensitiveData(string, head_length = 2, foot_length = 2) {
  let star = '';
  for (var i = 0; i < string.length - head_length - foot_length; i++) {
    star += '*';
  }
  return string.substring(0, head_length) + star + string.substring(string.length - foot_length);
}


// 请求函数二次封装
function httpRequest(options, method = 'get') { if ('body' in options) { method = 'post' }; return new Promise((resolve) => { $[method](options, (err, resp, data) => { try { if (err) { console.log(`❌ ${options['url']} 请求失败`); $.logErr(err); } else { if (data) { try { typeof JSON.parse(data) == 'object' ? (data = JSON.parse(data)) : ''; } catch (e) { } } else { console.log(`服务器返回空数据`); } } } catch (e) { $.logErr(e, resp); } finally { resolve(data); } }) }) }

// prettier-ignore
function Env(t, e) { class s { constructor(t) { this.env = t } send(t, e = "GET") { t = "string" == typeof t ? { url: t } : t; let s = this.get; return "POST" === e && (s = this.post), new Promise((e, i) => { s.call(this, t, (t, s, r) => { t ? i(t) : e(s) }) }) } get(t) { return this.send.call(this.env, t) } post(t) { return this.send.call(this.env, t, "POST") } } return new class { constructor(t, e) { this.name = t, this.http = new s(this), this.data = null, this.dataFile = "box.dat", this.logs = [], this.isMute = !1, this.isNeedRewrite = !1, this.logSeparator = "\n", this.encoding = "utf-8", this.startTime = (new Date).getTime(), Object.assign(this, e), this.log("", `\ud83d\udd14${this.name}, \u5f00\u59cb!`) } isNode() { return "undefined" != typeof module && !!module.exports } isQuanX() { return "undefined" != typeof $task } isSurge() { return "undefined" != typeof $httpClient && "undefined" == typeof $loon } isLoon() { return "undefined" != typeof $loon } isShadowrocket() { return "undefined" != typeof $rocket } isStash() { return "undefined" != typeof $environment && $environment["stash-version"] } toObj(t, e = null) { try { return JSON.parse(t) } catch { return e } } toStr(t, e = null) { try { return JSON.stringify(t) } catch { return e } } getjson(t, e) { let s = e; const i = this.getdata(t); if (i) try { s = JSON.parse(this.getdata(t)) } catch { } return s } setjson(t, e) { try { return this.setdata(JSON.stringify(t), e) } catch { return !1 } } getScript(t) { return new Promise(e => { this.get({ url: t }, (t, s, i) => e(i)) }) } runScript(t, e) { return new Promise(s => { let i = this.getdata("@chavy_boxjs_userCfgs.httpapi"); i = i ? i.replace(/\n/g, "").trim() : i; let r = this.getdata("@chavy_boxjs_userCfgs.httpapi_timeout"); r = r ? 1 * r : 20, r = e && e.timeout ? e.timeout : r; const [o, a] = i.split("@"), n = { url: `http://${a}/v1/scripting/evaluate`, body: { script_text: t, mock_type: "cron", timeout: r }, headers: { "X-Key": o, Accept: "*/*" } }; this.post(n, (t, e, i) => s(i)) }).catch(t => this.logErr(t)) } loaddata() { if (!this.isNode()) return {}; { this.fs = this.fs ? this.fs : require("fs"), this.path = this.path ? this.path : require("path"); const t = this.path.resolve(this.dataFile), e = this.path.resolve(process.cwd(), this.dataFile), s = this.fs.existsSync(t), i = !s && this.fs.existsSync(e); if (!s && !i) return {}; { const i = s ? t : e; try { return JSON.parse(this.fs.readFileSync(i)) } catch (t) { return {} } } } } writedata() { if (this.isNode()) { this.fs = this.fs ? this.fs : require("fs"), this.path = this.path ? this.path : require("path"); const t = this.path.resolve(this.dataFile), e = this.path.resolve(process.cwd(), this.dataFile), s = this.fs.existsSync(t), i = !s && this.fs.existsSync(e), r = JSON.stringify(this.data); s ? this.fs.writeFileSync(t, r) : i ? this.fs.writeFileSync(e, r) : this.fs.writeFileSync(t, r) } } lodash_get(t, e, s) { const i = e.replace(/\[(\d+)\]/g, ".$1").split("."); let r = t; for (const t of i) if (r = Object(r)[t], void 0 === r) return s; return r } lodash_set(t, e, s) { return Object(t) !== t ? t : (Array.isArray(e) || (e = e.toString().match(/[^.[\]]+/g) || []), e.slice(0, -1).reduce((t, s, i) => Object(t[s]) === t[s] ? t[s] : t[s] = Math.abs(e[i + 1]) >> 0 == +e[i + 1] ? [] : {}, t)[e[e.length - 1]] = s, t) } getdata(t) { let e = this.getval(t); if (/^@/.test(t)) { const [, s, i] = /^@(.*?)\.(.*?)$/.exec(t), r = s ? this.getval(s) : ""; if (r) try { const t = JSON.parse(r); e = t ? this.lodash_get(t, i, "") : e } catch (t) { e = "" } } return e } setdata(t, e) { let s = !1; if (/^@/.test(e)) { const [, i, r] = /^@(.*?)\.(.*?)$/.exec(e), o = this.getval(i), a = i ? "null" === o ? null : o || "{}" : "{}"; try { const e = JSON.parse(a); this.lodash_set(e, r, t), s = this.setval(JSON.stringify(e), i) } catch (e) { const o = {}; this.lodash_set(o, r, t), s = this.setval(JSON.stringify(o), i) } } else s = this.setval(t, e); return s } getval(t) { return this.isSurge() || this.isLoon() ? $persistentStore.read(t) : this.isQuanX() ? $prefs.valueForKey(t) : this.isNode() ? (this.data = this.loaddata(), this.data[t]) : this.data && this.data[t] || null } setval(t, e) { return this.isSurge() || this.isLoon() ? $persistentStore.write(t, e) : this.isQuanX() ? $prefs.setValueForKey(t, e) : this.isNode() ? (this.data = this.loaddata(), this.data[e] = t, this.writedata(), !0) : this.data && this.data[e] || null } initGotEnv(t) { this.got = this.got ? this.got : require("got"), this.cktough = this.cktough ? this.cktough : require("tough-cookie"), this.ckjar = this.ckjar ? this.ckjar : new this.cktough.CookieJar, t && (t.headers = t.headers ? t.headers : {}, void 0 === t.headers.Cookie && void 0 === t.cookieJar && (t.cookieJar = this.ckjar)) } get(t, e = (() => { })) { if (t.headers && (delete t.headers["Content-Type"], delete t.headers["Content-Length"]), this.isSurge() || this.isLoon()) this.isSurge() && this.isNeedRewrite && (t.headers = t.headers || {}, Object.assign(t.headers, { "X-Surge-Skip-Scripting": !1 })), $httpClient.get(t, (t, s, i) => { !t && s && (s.body = i, s.statusCode = s.status ? s.status : s.statusCode, s.status = s.statusCode), e(t, s, i) }); else if (this.isQuanX()) this.isNeedRewrite && (t.opts = t.opts || {}, Object.assign(t.opts, { hints: !1 })), $task.fetch(t).then(t => { const { statusCode: s, statusCode: i, headers: r, body: o } = t; e(null, { status: s, statusCode: i, headers: r, body: o }, o) }, t => e(t && t.error || "UndefinedError")); else if (this.isNode()) { let s = require("iconv-lite"); this.initGotEnv(t), this.got(t).on("redirect", (t, e) => { try { if (t.headers["set-cookie"]) { const s = t.headers["set-cookie"].map(this.cktough.Cookie.parse).toString(); s && this.ckjar.setCookieSync(s, null), e.cookieJar = this.ckjar } } catch (t) { this.logErr(t) } }).then(t => { const { statusCode: i, statusCode: r, headers: o, rawBody: a } = t, n = s.decode(a, this.encoding); e(null, { status: i, statusCode: r, headers: o, rawBody: a, body: n }, n) }, t => { const { message: i, response: r } = t; e(i, r, r && s.decode(r.rawBody, this.encoding)) }) } } post(t, e = (() => { })) { const s = t.method ? t.method.toLocaleLowerCase() : "post"; if (t.body && t.headers && !t.headers["Content-Type"] && (t.headers["Content-Type"] = "application/x-www-form-urlencoded"), t.headers && delete t.headers["Content-Length"], this.isSurge() || this.isLoon()) this.isSurge() && this.isNeedRewrite && (t.headers = t.headers || {}, Object.assign(t.headers, { "X-Surge-Skip-Scripting": !1 })), $httpClient[s](t, (t, s, i) => { !t && s && (s.body = i, s.statusCode = s.status ? s.status : s.statusCode, s.status = s.statusCode), e(t, s, i) }); else if (this.isQuanX()) t.method = s, this.isNeedRewrite && (t.opts = t.opts || {}, Object.assign(t.opts, { hints: !1 })), $task.fetch(t).then(t => { const { statusCode: s, statusCode: i, headers: r, body: o } = t; e(null, { status: s, statusCode: i, headers: r, body: o }, o) }, t => e(t && t.error || "UndefinedError")); else if (this.isNode()) { let i = require("iconv-lite"); this.initGotEnv(t); const { url: r, ...o } = t; this.got[s](r, o).then(t => { const { statusCode: s, statusCode: r, headers: o, rawBody: a } = t, n = i.decode(a, this.encoding); e(null, { status: s, statusCode: r, headers: o, rawBody: a, body: n }, n) }, t => { const { message: s, response: r } = t; e(s, r, r && i.decode(r.rawBody, this.encoding)) }) } } time(t, e = null) { const s = e ? new Date(e) : new Date; let i = { "M+": s.getMonth() + 1, "d+": s.getDate(), "H+": s.getHours(), "m+": s.getMinutes(), "s+": s.getSeconds(), "q+": Math.floor((s.getMonth() + 3) / 3), S: s.getMilliseconds() }; /(y+)/.test(t) && (t = t.replace(RegExp.$1, (s.getFullYear() + "").substr(4 - RegExp.$1.length))); for (let e in i) new RegExp("(" + e + ")").test(t) && (t = t.replace(RegExp.$1, 1 == RegExp.$1.length ? i[e] : ("00" + i[e]).substr(("" + i[e]).length))); return t } msg(e = t, s = "", i = "", r) { const o = t => { if (!t) return t; if ("string" == typeof t) return this.isLoon() ? t : this.isQuanX() ? { "open-url": t } : this.isSurge() ? { url: t } : void 0; if ("object" == typeof t) { if (this.isLoon()) { let e = t.openUrl || t.url || t["open-url"], s = t.mediaUrl || t["media-url"]; return { openUrl: e, mediaUrl: s } } if (this.isQuanX()) { let e = t["open-url"] || t.url || t.openUrl, s = t["media-url"] || t.mediaUrl, i = t["update-pasteboard"] || t.updatePasteboard; return { "open-url": e, "media-url": s, "update-pasteboard": i } } if (this.isSurge()) { let e = t.url || t.openUrl || t["open-url"]; return { url: e } } } }; if (this.isMute || (this.isSurge() || this.isLoon() ? $notification.post(e, s, i, o(r)) : this.isQuanX() && $notify(e, s, i, o(r))), !this.isMuteLog) { let t = ["", "==============\ud83d\udce3\u7cfb\u7edf\u901a\u77e5\ud83d\udce3=============="]; t.push(e), s && t.push(s), i && t.push(i), console.log(t.join("\n")), this.logs = this.logs.concat(t) } } log(...t) { t.length > 0 && (this.logs = [...this.logs, ...t]), console.log(t.join(this.logSeparator)) } logErr(t, e) { const s = !this.isSurge() && !this.isQuanX() && !this.isLoon(); s ? this.log("", `\u2757\ufe0f${this.name}, \u9519\u8bef!`, t.stack) : this.log("", `\u2757\ufe0f${this.name}, \u9519\u8bef!`, t) } wait(t) { return new Promise(e => setTimeout(e, t)) } done(t = {}) { const e = (new Date).getTime(), s = (e - this.startTime) / 1e3; this.log("", `\ud83d\udd14${this.name}, \u7ed3\u675f! \ud83d\udd5b ${s} \u79d2`), this.log(), this.isSurge() || this.isQuanX() || this.isLoon() ? $done(t) : this.isNode() && process.exit(1) } }(t, e) }
