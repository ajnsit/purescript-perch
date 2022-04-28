export function js_document() {
  return document;
}
export function js_documentBody() {
  return document.body;
}
export function js_getElementById(x) {
  return function () {
    return document.getElementById(x);
  };
}
export function js_querySelectorAll(x) {
  return function () {
    return document.querySelectorAll(x);
  };
}
export function js_documentCreateNode(x) {
  return function () {
    return document.createElement(x);
  };
}
export function js_documentCreateNodeNS(x) {
  return function (y) {
    return function () {
      return document.createElementNS(x, y);
    };
  };
}
export function js_createTextNode(x) {
  return function () {
    return document.createTextNode(x);
  };
}

// Check if object is an HTML Element of current DOM.  Works with HTML
// elements and text nodes.
// http://stackoverflow.com/a/20476546/1749901
export function js_isInCurrentDOM(x) {
  return function () {
    return x !== null && !!x.ownerDocument && window === (x.ownerDocument.defaultView || x.ownerDocument.parentWindow);
  };
}

export function js_parentNode(x) {
  return function () {
    return x.parentNode;
  };
}

export function js_appendChild(x) {
  return function (y) {
    return function () {
      x.appendChild(y);
    };
  };
}

export function js_replaceChild(x) {
  return function (y) {
    return function (z) {
      return function () {
        x.replaceChild(y, z);
      };
    };
  };
}

export function js_removeChild(x) {
  return function (y) {
    return function () {
      x.removeChild(y);
    };
  };
}

export function js_clearChildren(x) {
  return function () {
    while (x.hasChildNodes()) x.removeChild(x.lastChild);
  };
}

export function js_setAttribute(x) {
  return function (y) {
    return function (z) {
      return function () {
        x.setAttribute(y, z);
      };
    };
  };
}

export function js_setInnerHtml(x) {
  return function (y) {
    return function () {
      x.innerHTML = y;
    };
  };
}

export function js_addEventListener(x) {
  return function (y) {
    return function (z) {
      return function () {
        x.addEventListener(y, z);
      };
    };
  };
}

export function js_removeEventListener(x) {
  return function (y) {
    return function (z) {
      return function () {
        x.removeEventListener(y, z);
      };
    };
  };
}
