/*
cd.sd 嗅探脚本（QX 专用，一次性探针）
用途：cd.sd 的签到/农场接口在登录后才能看到，从外面探不到。
这个脚本只做两件事：① 存登录 Cookie；② 把你点的签到/采菜/偷菜请求的
方法+路径+参数记下来并弹通知，你把通知截图发我，我就按真实接口写正式脚本。
登录请求不记 body（里面有密码）。
导入：把这个 .js 链接作为资源加进 QX（不要开 Resource Parser），下面的重写和 MITM 会一起导进来，用完可删。

[rewrite_local]
^https://cd\.sd/.* url script-request-body https://cdn.jsdelivr.net/gh/Youglas/anyouglas/cd_sd_sniffer.js, tag=cd嗅探, enabled=true

[mitm]
hostname = cd.sd
*/

const COOKIE_KEY = 'cd_cookie';
const PROBE_KEY = 'cd_probe';

// QX 的 API：存取用 $prefs，通知用 $notify
function read(k) { try { return $prefs.valueForKey(k) || ''; } catch (e) { return ''; } }
function write(v, k) { $prefs.setValueForKey(v, k); }
function notify(title, sub, body) { $notify(title, sub, body); }

try {
  if (typeof $request !== 'undefined') {
    // rewrite：捕获请求
    const url = $request.url || '';
    const method = ($request.method || 'GET').toUpperCase();
    const path = url.replace(/^https?:\/\/cd\.sd/i, '');
    const isApi = path.indexOf('/api/') !== -1;
    // 只抓 POST，和 /api/ 的 GET（列表类接口）；页面 HTML 的 GET 不抓
    if (method === 'POST' || isApi) {
      // 存 Cookie（多账号 @ 拼接，去重）
      const cookie = $request.headers['Cookie'] || $request.headers['cookie'] || '';
      if (cookie) {
        const arr = read(COOKIE_KEY) ? read(COOKIE_KEY).split('@') : [];
        if (arr.indexOf(cookie) === -1) {
          arr.push(cookie);
          write(arr.join('@'), COOKIE_KEY);
          notify('🍑 cd 嗅探', 'Cookie 已保存', `账号数：${arr.length}`);
        }
      }
      // 记录接口
      const isLogin = /login/i.test(path);
      let body = '';
      if (!isLogin && $request.body) {
        body = typeof $request.body === 'string' ? $request.body : JSON.stringify($request.body);
        body = body.slice(0, 120);
      }
      const record = `${method} ${path}${body ? ' | ' + body : ''}`;
      const list = read(PROBE_KEY) ? read(PROBE_KEY).split('\n') : [];
      if (list.indexOf(record) === -1) {
        list.push(record);
        write(list.join('\n'), PROBE_KEY);
        notify('🍑 cd 嗅探', `抓到 ${method} ${path}`, body || '（无 body）');
      }
    }
    $done({});
  } else {
    // task：汇报已记录的接口
    const stored = read(PROBE_KEY);
    const cookie = read(COOKIE_KEY);
    const n = cookie ? cookie.split('@').length : 0;
    const m = stored ? stored.split('\n').length : 0;
    notify('🍑 cd 嗅探', `已记录 ${m} 个接口，${n} 个账号`, stored || '还没抓到：登录后点一次签到/采菜/偷菜');
    $done({});
  }
} catch (e) {
  notify('🍑 cd 嗅探', '脚本出错', String((e && e.message) || e).slice(0, 120));
  $done({});
}
