// boot.js: the browser's end of a CLOG page, in bhw-www's fork of CLOG.
//
// The server draws the page, and handles its events, over a websocket: it
// sends scripts, each message a JSON array of them, which run in order, and
// the page sends back its events ("E:"), the values of mirrored form
// elements ("M:"), pings saying how many messages it has run ("P:"), and the
// answers to the server's queries.  When the socket drops, the page keeps
// what it has to send, reconnects with its connection's ID and the number of
// messages it has run, and the server sends what it missed; a page the
// server no longer knows is reloaded.

var ws = null;
var clog = {};

clog['body'] = document.body;
clog['head'] = document.head;
clog['documentElement'] = document.documentElement;
clog['window'] = window;
clog['navigator'] = navigator;
clog['document'] = window.document;
clog['location'] = window.location;

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

// Mirrored form elements: the server keeps their values, which the page
// sends whenever they change, and at once.  A select's value changes with
// its options too.
function clog_mirror_send(el, id) {
    clog_send('M:' + id + ' ' + el.value);
}

// Its listeners capture, so that they come before the element's handlers:
// the server has the new value when it handles the event.
function clog_mirror(el, id) {
    var send = function () { clog_mirror_send(el, id); };
    el.addEventListener('input', send, true);
    el.addEventListener('change', send, true);
    if (el.tagName == 'SELECT') {
        new MutationObserver(send).observe(el, {childList: true, subtree: true});
    }
    send();
}

// The connection.

var clog_seq = 0;        // the messages from the server run
var clog_outbox = [];    // what to send once the socket is open
var clog_retries = 0;    // attempts to reconnect since the socket was open
var clog_retry = null;   // the next attempt
var clog_pinged = 0;     // when the last ping went
var clog_heard = 0;      // when the server last sent anything
var clog_ended = false;  // the server ended the page: no reconnecting

function clog_send(m) {
    if (ws && ws.readyState == 1) {
        ws.send(m);
    } else {
        clog_outbox.push(m);
    }
}

function clog_run(script) {
    if (clog_debug) {
        console.log('eval data = ' + script);
    }
    try {
        eval(script);
    } catch (e) {
        console.error(e);
    }
}

// Run SCRIPT, an expression, for the server's query ID.
function clog_answer(id, script) {
    var answer;
    try {
        answer = id + ':' + eval(script);
    } catch (e) {
        answer = id + '!' + e;
    }
    clog_send(answer);
}

function clog_receive(data) {
    clog_heard = Date.now();
    if (data === '') {          // a ping's answer
        return;
    }
    clog_seq++;
    var scripts;
    try {
        scripts = JSON.parse(data);
    } catch (e) {
        console.error(e);
        return;
    }
    scripts.forEach(clog_run);
}

function clog_address() {
    var params;
    if (clog['connection_id']) {
        params = {r: clog['connection_id'], s: clog_seq};
    } else {
        params = {p: location.pathname, q: location.search.replace(/^\?/, ''),
                  tz: new Date().getTimezoneOffset(),
                  vw: window.innerWidth, vh: window.innerHeight,
                  dpr: window.devicePixelRatio || 1,
                  lang: navigator.language || ''};
        var boot = document.querySelector('meta[name="clog-boot"]');
        if (boot) {
            params.b = boot.content;
        }
    }
    return (location.protocol == 'https:' ? 'wss://' : 'ws://') + location.host + '/clog?'
        + Object.keys(params).map(function (k) {
            return k + '=' + encodeURIComponent(params[k]);
        }).join('&');
}

function clog_open() {
    var sock;
    try {
        sock = new WebSocket(clog_address());
    } catch (e) {
        console.error(e);
        clog_reconnect();
        return;
    }
    ws = sock;
    sock.onopen = function () {
        if (sock !== ws) { return; }
        clog_retries = 0;
        clog_heard = Date.now();
        clog_pinged = 0;
        var out = clog_outbox;
        clog_outbox = [];
        out.forEach(function (m) { sock.send(m); });
    };
    sock.onmessage = function (event) {
        if (sock === ws) { clog_receive(event.data); }
    };
    // An error is always followed by a close, which acts.
    sock.onclose = function (event) {
        if (sock !== ws) { return; }
        ws = null;
        if (event.code == 4000) {         // the server no longer knows this page
            location.reload();
        } else if (event.code == 1000) {  // the server ended it
            clog_ended = true;
        } else {
            clog_reconnect();
        }
    };
}

// Reconnect after a while: a quarter of a second, doubling up to ten,
// each a random half to whole of that.
function clog_reconnect() {
    if (clog_retry || clog_ended) { return; }
    var delay = Math.min(10000, 250 * Math.pow(2, clog_retries)) * (0.5 + Math.random() / 2);
    clog_retries++;
    clog_retry = setTimeout(function () {
        clog_retry = null;
        clog_open();
    }, delay);
}

// Give up on a socket the server has stopped answering, and reconnect.
function clog_abandon() {
    var sock = ws;
    ws = null;
    if (sock) {
        sock.onclose = null;
        try { sock.close(4001); } catch (e) {}
    }
    clog_reconnect();
}

// Every ten seconds (once a minute in a hidden tab, as browsers slow its
// timers), a ping, which the server answers; one unanswered for 20 seconds
// means the connection is dead though the socket does not know it.
setInterval(function () {
    if (!ws || ws.readyState != 1) { return; }
    var now = Date.now();
    if (clog_pinged > clog_heard) {
        if (now - clog_pinged > 20000) { clog_abandon(); }
    } else {
        clog_pinged = now;
        ws.send('P:' + clog_seq);
    }
}, 10000);

// Back online: reconnect now rather than at the next attempt.
window.addEventListener('online', function () {
    if (!ws && !clog_ended) {
        clearTimeout(clog_retry);
        clog_retry = null;
        clog_open();
    }
});

// boot.js runs in the head, before there is a body.
function clog_ready() {
    clog['body'] = document.body;
    if (ws == null) { clog_open(); }
}

if (document.readyState == 'loading') {
    document.addEventListener('DOMContentLoaded', clog_ready);
} else {
    clog_ready();
}
