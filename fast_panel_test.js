/* 最小弹窗测试：不联网，只返回 $done 弹窗，区分“弹窗机制不通”还是“测速逻辑失败”。 */
$done({
  title: '面板测试',
  message: '面板测试成功',
  htmlMessage: '<p style="text-align: center; font-family: -apple-system; font-size: large; font-weight: thin">面板测试成功</br>如果看到这句，说明 $done 弹窗是通的</p>'
});
