var main = require("../output/Main/index.js");
if( window.location.href.indexOf('#') === -1) {
  window.location.replace(
		`${location.protocol}//${location.host}/#${location.pathname}`
	);
}
main.main();