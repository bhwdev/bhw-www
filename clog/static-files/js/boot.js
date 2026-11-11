var ws=null;
var adr; var adrc;
var clog={};
var pingerid;
var retryid;
var s = document.location.search;
var tokens;
var r = /[?&]?([^=]+)=([^&]*)/g;

clog['body']=document.body;
clog['head']=document.head;
clog['documentElement']=document.documentElement;
clog['window']=window;
clog['navigator']=navigator;
clog['document']=window.document;
clog['location']=window.location;

if (typeof clog_debug == 'undefined') {
    clog_debug = false;
}

// What the scripts the server sends call, in place of jQuery and jQuery UI.

// Events.  A handler is called as jQuery calls it: with the element as
// THIS, the event, and the data of a CLOG_TRIGGER; returning false cancels
// the event and stops its propagation.  CLOG_OFF removes every handler
// CLOG_ON added for an event.
function clog_on(el, ev, fn) {
    var h = function (e) {
        var data = (e instanceof CustomEvent) ? e.detail : undefined;
        if (fn.call(el, e, data) === false) {
            e.preventDefault();
            e.stopPropagation();
        }
    };
    var hs = el.clog_handlers || (el.clog_handlers = {});
    (hs[ev] || (hs[ev] = [])).push(h);
    el.addEventListener(ev, h);
}

function clog_off(el, ev) {
    var hs = el.clog_handlers && el.clog_handlers[ev];
    if (hs) {
        hs.forEach(function (h) { el.removeEventListener(ev, h); });
        delete el.clog_handlers[ev];
    }
}

// Like jQuery's trigger, the event bubbles, up to the window.
function clog_trigger(el, ev, data) {
    el.dispatchEvent(new CustomEvent(ev, {bubbles: true, cancelable: true,
                                          detail: data}));
}

// The first element of HTML, not yet in the document.  As with innerHTML,
// its scripts do not run.
function clog_make(html) {
    var t = document.createElement('template');
    t.innerHTML = html;
    return t.content.firstElementChild;
}

// Styles: hyphenated names (and custom properties) or camelCase ones.
function clog_css(el, name) {
    var cs = getComputedStyle(el);
    return name.indexOf('-') >= 0 ? cs.getPropertyValue(name) : cs[name];
}

function clog_set_css(el, name, value) {
    if (typeof name == 'object') {
        for (var k in name) { clog_set_css(el, k, name[k]); }
    } else if (name.indexOf('-') >= 0) {
        el.style.setProperty(name, value);
    } else {
        el.style[name] = value;
    }
}

// Sizes in pixels, as jQuery measures them: DIM is 'Width' or 'Height',
// BOX 'content' (width()), 'padding' (innerWidth()), 'border'
// (outerWidth()) or 'margin' (outerWidth(true)).
function clog_box_extra(el, dim) {
    var cs = getComputedStyle(el);
    var a = dim == 'Width' ? ['Left', 'Right'] : ['Top', 'Bottom'];
    var sum = function (p, s) {
        return (parseFloat(cs[p + a[0] + s]) || 0) + (parseFloat(cs[p + a[1] + s]) || 0);
    };
    return {padding: sum('padding', ''), border: sum('border', 'Width'),
            margin: sum('margin', ''), borderBox: cs.boxSizing == 'border-box'};
}

function clog_box(el, dim, box) {
    if (el === window) {
        return (box == 'border' || box == 'margin') ? window['inner' + dim]
            : document.documentElement['client' + dim];
    }
    if (el.nodeType == 9) {
        var d = el.documentElement, b = el.body;
        return Math.max(b['scroll' + dim], d['scroll' + dim], b['offset' + dim],
                        d['offset' + dim], d['client' + dim]);
    }
    var x = clog_box_extra(el, dim), size = el['offset' + dim];
    if (box == 'margin') { return size + x.margin; }
    if (box == 'border') { return size; }
    if (box == 'padding') { return size - x.border; }
    return size - x.border - x.padding;
}

// Set the BOX size: a bare number, or one in px, is that box's size in
// pixels whatever the box-sizing; any other value is set as it is.
function clog_set_box(el, dim, box, value) {
    var s = String(value).trim(), m = s.match(/^(-?[\d.]+)(px)?$/);
    var name = dim.toLowerCase();
    if (!m) { el.style[name] = s; return; }
    var x = clog_box_extra(el, dim), px = parseFloat(m[1]);
    var content = box == 'content' ? px : box == 'padding' ? px - x.padding
        : px - x.padding - x.border;
    el.style[name] = Math.max(0, x.borderBox ? content + x.padding + x.border : content) + 'px';
}

// jQuery's position(): from the offset parent's padding edge to the margin edge.
function clog_position(el) {
    var cs = getComputedStyle(el);
    return {top: el.offsetTop - (parseFloat(cs.marginTop) || 0),
            left: el.offsetLeft - (parseFloat(cs.marginLeft) || 0)};
}

// jQuery's offset(): from the document's corner.
function clog_offset(el) {
    var r = el.getBoundingClientRect();
    return {top: r.top + window.pageYOffset, left: r.left + window.pageXOffset};
}

// OP is 'add', 'remove' or 'toggle'; CLASSES names one or more classes.
function clog_class(el, op, classes) {
    classes.split(/\s+/).forEach(function (c) { if (c) { el.classList[op](c); } });
}

// The text of an element's own text nodes, not its children's.
function clog_own_text(el) {
    var t = '';
    el.childNodes.forEach(function (n) {
        if (n.nodeType == 3 || n.nodeType == 4) { t += n.nodeValue; }
    });
    return t;
}

function clog_set_own_text(el, value) {
    for (var n = el.firstChild; n; n = n.nextSibling) {
        if (n.nodeType != 1) { n.nodeValue = value; return; }
    }
}

// The first element SELECTOR matches whose name attribute is NAME, with
// EXTRA (a pseudo-class, a descendant) after it; null if none.
function clog_field(selector, name, extra) {
    return document.querySelector(selector + '[name="' + CSS.escape(name) + '"]' + (extra || ''));
}

function clog_load_script(url, done) {
    var s = document.createElement('script');
    s.src = url;
    s.onload = done;
    document.head.appendChild(s);
}

// Moving and resizing a window in the browser, as CLOG-GUI asks with
// :CLIENT-MOVEMENT.  Pointer events, so touch works too.  dragstart and
// dragstop, resizestart and resizestop are triggered once the pointer has
// moved, and when it is released.
function clog_follow_pointer(e, onmove, start, stop) {
    var x0 = e.clientX, y0 = e.clientY, started = false;
    var move = function (e) {
        if (!started) { started = true; start(); }
        onmove(e.clientX - x0, e.clientY - y0);
    };
    var up = function () {
        document.removeEventListener('pointermove', move);
        document.removeEventListener('pointerup', up);
        document.removeEventListener('pointercancel', up);
        if (started) { stop(); }
    };
    document.addEventListener('pointermove', move);
    document.addEventListener('pointerup', up);
    document.addEventListener('pointercancel', up);
}

// HANDLE_ID, if given, is the id of the element inside EL to drag it by.
function clog_draggable(el, handle_id) {
    var grip = handle_id ? el.querySelector('#' + CSS.escape(handle_id)) : el;
    grip.style.touchAction = 'none';
    grip.addEventListener('pointerdown', function (e) {
        if (e.button != 0 || e.target.closest('input,textarea,button,select,option')) {
            return;
        }
        var left = el.offsetLeft, top = el.offsetTop;
        e.preventDefault();
        clog_follow_pointer(e, function (dx, dy) {
            el.style.left = (left + dx) + 'px';
            el.style.top = (top + dy) + 'px';
        }, function () { clog_trigger(el, 'dragstart'); },
           function () { clog_trigger(el, 'dragstop'); });
    });
}

function clog_resizable(el) {
    var h = document.createElement('div');
    h.style.cssText = 'position:absolute;right:0;bottom:0;width:12px;height:12px;'
        + 'cursor:se-resize;touch-action:none;z-index:90;'
        + 'background:linear-gradient(135deg,transparent 50%,rgba(0,0,0,.35) 50%)';
    el.appendChild(h);
    h.addEventListener('pointerdown', function (e) {
        if (e.button != 0) { return; }
        var w = el.offsetWidth, ht = el.offsetHeight;
        e.preventDefault();
        e.stopPropagation();
        clog_follow_pointer(e, function (dx, dy) {
            clog_set_box(el, 'Width', 'border', w + dx);
            clog_set_box(el, 'Height', 'border', ht + dy);
        }, function () { clog_trigger(el, 'resizestart'); },
           function () { clog_trigger(el, 'resizestop'); });
    });
}

function Ping_ws() {
    if (ws.readyState == 1) {
        ws.send ('0');
    }
}

function Shutdown_ws(event) {
    if (ws != null) {
	ws.onerror = null;
	ws.onclose = null;
	ws.close ();
	ws = null;
    }
    clearInterval (pingerid);
    if (clog['html_on_close'] != '') {
        document.body.innerHTML = clog['html_on_close'];
    }
}

function Setup_ws() {
    ws.onmessage = function (event) {
        try {
            if (clog_debug == true) {
		console.log ('eval data = ' + event.data);
            }
            eval (event.data);
        } catch (e) {
            console.error (e.message);
        }
    }

    var rc = function (event) {
	console.log (event);
	clearInterval (retryid);
	ws = null;
	ws = new WebSocket (adr  + '?r=' + clog['connection_id']);
        ws.onopen = function (event) {
            console.log ('reconnect successful');
            Setup_ws();
        }
        ws.onclose = function (event) {
            console.log ('reconnect failure');
	    console.log (Date.now());
	    retryid = setInterval(function () {rc("Failed reconnect - trying again")}, 500);
        }
    }

    ws.onerror = function (event) {
        console.log ('onerror: reconnect');
	rc("onerror - trying reconnect")
    }

    ws.onclose = function (event) {
        if (event.code && event.code === 1000) {
            console.log("WebSocket connection got normal close from server. Don't reconnect.");
            Shutdown_ws(event);
        } else {
	    rc("onclose - trying reconnnect");
        }
    }
}

function Open_ws() {
    if (location.protocol == 'https:') {
	adr = 'wss://' + location.hostname;
    } else {
	adr = 'ws://' + location.hostname;
    }

    if (location.port != '') { adr = adr + ':' + location.port; }
    adr = adr + '/clog';

    if (clog['connection_id']) {
      adrc = adr  + '?r=' + clog['connection_id'];
    } else { adrc = adr }

    try {
	console.log ('connecting to ' + adrc);
	ws = new WebSocket (adrc);
    } catch (e) {
	console.log ('trying again, connecting to ' + adrc);
	ws = new WebSocket (adrc);
    }

    if (ws != null) {
	ws.onopen = function (event) {
            console.log ('connection successful');
            Setup_ws();
	}
	pingerid = setInterval (function () {Ping_ws ();}, 10000);
    } else {
	document.writeln ('If you are seeing this your browser or your connection to the internet is blocking websockets.');
    }
}

// boot.js runs in the head, before there is a body.
function Ready_ws() {
    clog['body'] = document.body;
    if (ws == null) { Open_ws(); }
}

if (document.readyState == 'loading') {
    document.addEventListener('DOMContentLoaded', Ready_ws);
} else {
    Ready_ws();
}
