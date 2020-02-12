exports.js_document = function () { return document; };
exports.js_documentBody = function() { return document.body; };
exports.js_getElementById = function(x) { return function() { return document.getElementById(x); }; };
exports.js_querySelectorAll = function(x) { return function() { return document.querySelectorAll(x); }; };
exports.js_documentCreateNode = function(x) {return function() { return document.createElement(x); }; };
exports.js_documentCreateNodeNS = function(x) {return function(y) { return function() { return document.createElementNS(x,y); }; }; };
exports.js_createTextNode = function(x) {return function() { return document.createTextNode(x); };};

// Check if object is an HTML Element of current DOM.  Works with HTML
// elements and text nodes.
// http://stackoverflow.com/a/20476546/1749901
exports.js_isInCurrentDOM = function(x) { return function() {
    return (x !== null && !(!x.ownerDocument) && (window === (x.ownerDocument.defaultView || x.ownerDocument.parentWindow)));
}; };

exports.js_parentNode = function(x) { return function() { return x.parentNode; };};

exports.js_appendChild = function(x) { return function(y) { return function() { x.appendChild(y);}; }; };

exports.js_replaceChild = function(x) { return function(y) { return function(z) { return function() { x.replaceChild(y,z);};};};};

exports.js_removeChild = function(x) { return function(y) { return function() {x.removeChild(y);};};};

exports.js_clearChildren = function(x) { return function() {
  while (x.hasChildNodes()) x.removeChild(x.lastChild);
};};

exports.js_setAttribute = function(x) { return function(y) { return function(z) { return function() {x.setAttribute(y, z); }; };};};

exports.js_setInnerHtml = function(x) { return function(y) { return function() {x.innerHTML = y;}; };};

exports.js_addEventListener = function(x) { return function(y) { return function(z) { return function() { x.addEventListener(y,z);};};};};

exports.js_removeEventListener = function(x) { return function(y) { return function(z) { return function() { x.removeEventListener(y,z);};};};};
