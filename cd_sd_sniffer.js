/****************************** 
脚本功能：cd.sd 农场接口嗅探
Version  : v1.0.0
更新时间：2026-11-20
作者：Tuesday
Platform : Quantumult X / Loon / Surge

使用说明：
cd.sd 的签到/农场接口在登录后才能看到，从外面探不到。
导入后登录 cd.sd，点一次签到、一次采菜、一次偷菜，脚本记录接口和 Cookie 并弹通知，把通知截图发我。
用完可删。

[rewrite_local]
^https://cd\.sd/.* url script-request-body https://cdn.jsdelivr.net/gh/Youglas/anyouglas@2253b2a562fec699346999252d934461f9fb2bdb/cd_sd_sniffer.js

[task_local]
0 9 * * * https://cdn.jsdelivr.net/gh/Youglas/anyouglas@2253b2a562fec699346999252d934461f9fb2bdb/cd_sd_sniffer.js, tag=cd 嗅探, enabled=true

[MITM]
hostname = %APPEND% cd.sd
*******************************/

// ========== 三端适配层 ==========
var isQX = typeof $task !== "undefined";
var isLoon = typeof $loon !== "undefined";
var isSurge = typeof $httpClient !== "undefined" && !isLoon;

var $store = {
  read: function (key) { return isQX ? $prefs.valueForKey(key) : $persistentStore.read(key); },
  write: function (val, key) { return isQX ? $prefs.setValueForKey(val, key) : $persistentStore.write(val, key); }
};

var notifyFn = isQX
  ? function (t, s, b) { $notify(t, s, b); }
  : function (t, s, b) { $notification.post(t, s, b); };

// ========== 嗅探逻辑 ==========
var COOKIE_KEY = "cd_cookie";
var PROBE_KEY = "cd_probe";
var isGetHeader = typeof $request !== "undefined";

if (isGetHeader) {
  var url = $request.url || "";
  var method = ($request.method || "GET").toUpperCase();
  var path = url.replace(/^https?:\/\/cd\.sd/i, "");
  var isApi = path.indexOf("/api/") !== -1;
  // 只抓 POST，和 /api/ 的 GET（列表类接口）；页面 HTML 的 GET 不抓
  if (method === "POST" || isApi) {
    // 存 Cookie（多账号 @ 拼接，去重）
    var cookie = $request.headers["Cookie"] || $request.headers["cookie"] || "";
    if (cookie) {
      var arr = $store.read(COOKIE_KEY) ? $store.read(COOKIE_KEY).split("@") : [];
      if (arr.indexOf(cookie) === -1) {
        arr.push(cookie);
        $store.write(arr.join("@"), COOKIE_KEY);
        notifyFn("🍑 cd 嗅探", "Cookie 已保存", "账号数：" + arr.length);
      }
    }
    // 记录接口
    var isLogin = /login/i.test(path);
    var body = "";
    if (!isLogin && $request.body) {
      body = typeof $request.body === "string" ? $request.body : JSON.stringify($request.body);
      body = body.slice(0, 120);
    }
    var record = method + " " + path + (body ? " | " + body : "");
    var list = $store.read(PROBE_KEY) ? $store.read(PROBE_KEY).split("\n") : [];
    if (list.indexOf(record) === -1) {
      list.push(record);
      $store.write(list.join("\n"), PROBE_KEY);
      notifyFn("🍑 cd 嗅探", "抓到 " + method + " " + path, body || "（无 body）");
    }
  }
  $done({});
} else {
  // 任务：汇报已记录的接口
  var stored = $store.read(PROBE_KEY) || "";
  var ck = $store.read(COOKIE_KEY) || "";
  var n = ck ? ck.split("@").length : 0;
  var m = stored ? stored.split("\n").length : 0;
  notifyFn("🍑 cd 嗅探", "已记录 " + m + " 个接口，" + n + " 个账号", stored || "还没抓到：登录后点一次签到/采菜/偷菜");
  $done({});
}
