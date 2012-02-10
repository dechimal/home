// ==UserScript==
// @name           click
// @namespace      http://lovelang.heroku.com/
// @include        http://lovelang.heroku.com/
// ==/UserScript==
window.postcount = 0;
window.setInterval(
    function() {
        e = document.createEvent("MouseEvents");
        e.initMouseEvent("mouseup", true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
        document.getElementById("10").dispatchEvent(e);
        if (++window.postcount == 1200) {
            window.location.reload();
        }
    },
    50);
