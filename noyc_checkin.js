// 草丛网站每日签到 for Quantumult X（4 个账号）
// 站点：https://go.noyc.eu（维多利亚的秘密）
// 签到接口（实测存在，需登录态，未登录会 302 跳回 /auth/login）：
//   POST https://go.noyc.eu/api/checkin
//   Headers: Cookie（登录后的完整 Cookie）
// 抓取 + 定时签到同一个文件，靠 typeof $request 区分入口（Env 模板风格）。
// 抓取：Safari 登录账号后手动点一次签到按钮，即保存该账号 Cookie（最多 4 个）。
// 定时：每天 9:00 依次给 4 个账号签到。

function Env(name) {
  var isQX = typeof $task !== "undefined";
  return {
    name: name,
    getdata: function (key) {
      if (isQX) return $persistentStore.read(key);
      return null;
    },
    setdata: function (val, key) {
      if (isQX) $persistentStore.write(val, key);
    },
    msg: function (title, subt, desc) {
      if (isQX) $notify(title, subt, desc);
    },
    done: function (val) {
      if (isQX) $done(val || {});
    },
    post: function (opts, cb) {
      if (isQX) {
        $task.fetch({
          url: opts.url,
          method: "POST",
          headers: opts.headers,
          body: opts.body,
          timeout: 15000
        }).then(
          function (resp) {
            cb(null, { status: resp.statusCode, headers: resp.headers }, resp.body);
          },
          function (err) {
            cb(err, null, null);
          }
        );
      } else {
        cb(new Error("not in Quantumult X"), null, null);
      }
    }
  };
}

var $ = new Env("草丛签到");

var MAX_ACCOUNTS = 4;
var CHECKIN_URL = "https://go.noyc.eu/api/checkin";

function cookieKey(i) {
  return "noyc_cookie_" + i;
}

function getHeader(headers, name) {
  if (!headers) return "";
  var keys = Object.keys(headers);
  for (var i = 0; i < keys.length; i++) {
    if (keys[i].toLowerCase() === name.toLowerCase()) return headers[keys[i]] || "";
  }
  return "";
}

function getAccounts() {
  var list = [];
  for (var i = 1; i <= MAX_ACCOUNTS; i++) {
    var c = $.getdata(cookieKey(i));
    if (c) list.push({ slot: i, cookie: c });
  }
  return list;
}

function esc(s) {
  return String(s)
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;");
}

if (typeof $request !== "undefined") {
  // ---------- 抓包分支：保存账号 Cookie ----------
  // 触发条件：Safari 里手动点一次签到按钮（请求 POST /api/checkin 时）
  var headers = $request.headers || {};
  var cookie = getHeader(headers, "Cookie");
  if (!cookie) {
    $.msg("草丛签到", "未抓到 Cookie", "请先登录再点签到按钮");
  } else {
    var accounts = getAccounts();
    var slot = -1;
    for (var a = 0; a < accounts.length; a++) {
      if (accounts[a].cookie === cookie) slot = accounts[a].slot;
    }
    if (slot !== -1) {
      $.msg("草丛签到", "账号 " + slot + " 已保存过", "Cookie 未变，无需重复抓取");
    } else {
      for (var i = 1; i <= MAX_ACCOUNTS; i++) {
        if (!$.getdata(cookieKey(i))) {
          slot = i;
          break;
        }
      }
      if (slot === -1) {
        $.msg("草丛签到", "4 个位置已满", "如需更换账号请在 QX 设置里清理旧 Cookie");
      } else {
        $.setdata(cookie, cookieKey(slot));
        $.msg("草丛签到", "账号 " + slot + " Cookie 已保存", "每天 9:00 自动签到（共 " + (accounts.length + 1) + " 个账号）");
      }
    }
  }
  $.done({});
} else {
  // ---------- 定时分支：依次给每个账号签到 ----------
  var accounts = getAccounts();
  if (accounts.length === 0) {
    $.done({
      title: "草丛签到",
      htmlMessage:
        '<p style="text-align: center; font-family: -apple-system; font-size: large; font-weight: thin">未抓到 Cookie</br></br>请先登录并手动点一次签到</p>'
    });
  } else {
    var results = [];
    var idx = 0;

    function checkinOne(acc, cb) {
      $.post(
        {
          url: CHECKIN_URL,
          headers: {
            Cookie: acc.cookie,
            Referer: "https://go.noyc.eu/",
            Accept: "application/json, text/plain, */*",
            "User-Agent":
              "Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Mobile/15E148 Safari/604.1"
          },
          body: ""
        },
        function (err, resp, data) {
          if (err) return cb("请求失败");
          var status = resp ? resp.status : 0;
          if (status === 302 || status === 401 || status === 403) return cb("Cookie 已过期，请重新抓取");
          if (typeof data === "string" && (data.indexOf("/auth/login") !== -1 || data.indexOf("<html") !== -1)) {
            return cb("Cookie 已过期，请重新抓取");
          }
          var json = null;
          try {
            json = JSON.parse(data);
          } catch (e) {
            json = null;
          }
          if (json) {
            var m = json.message || json.msg || json.data || "";
            if (m) return cb(esc(String(m)).slice(0, 40));
            if (json.success === true || json.ret === 1 || json.code === 0) return cb("签到成功");
            return cb("返回异常 (HTTP " + status + ")");
          }
          return cb("返回异常 (HTTP " + status + ")");
        }
      );
    }

    function next() {
      if (idx >= accounts.length) {
        var html =
          '<p style="text-align: center; font-family: -apple-system; font-size: large; font-weight: thin">' +
          results.join(
            '</br><span style="color: #ccc;">- - - - - - - - - -</span></br>'
          ) +
          "</p>";
        $.done({ title: "草丛签到", htmlMessage: html });
        return;
      }
      var acc = accounts[idx++];
      checkinOne(acc, function (text) {
        results.push("<b>账号 " + acc.slot + "</b> ⇢ " + text);
        next();
      });
    }

    next();
  }
}
