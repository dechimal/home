// ==UserScript==
// @name           disable twitter keys
// @namespace      https://twitter.com/
// @include        http://twitter.com/*
// @include        https://twitter.com/*
// ==/UserScript==

window.setInterval("$(document).off('keydown')", 1000);
