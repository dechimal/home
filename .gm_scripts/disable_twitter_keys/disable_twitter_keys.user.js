// ==UserScript==
// @name           disable twitter keys
// @namespace      https://twitter.com/
// @include        http://twitter.com/*
// @include        https://twitter.com/*
// ==/UserScript==

window.setTimeout("$(document).unbind('keydown')", 1000);
