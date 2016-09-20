! function(a) {
    "undefined" != typeof exports ? a(exports) : (window.hljs = a({}), "function" == typeof define && define.amd && define("hljs", [], function() {
        return window.hljs
    }))
}(function(a) {
    function b(a) {
        return a.replace(/&/gm, "&amp;").replace(/</gm, "&lt;").replace(/>/gm, "&gt;")
    }

    function c(a) {
        return a.nodeName.toLowerCase()
    }

    function d(a, b) {
        var c = a && a.exec(b);
        return c && 0 == c.index
    }

    function e(a) {
        return /^(no-?highlight|plain|text)$/i.test(a)
    }

    function f(a) {
        var b, c, d, f = a.className + " ";
        if (f += a.parentNode ? a.parentNode.className : "", c = /\blang(?:uage)?-([\w-]+)\b/i.exec(f)) return u(c[1]) ? c[1] : "no-highlight";
        for (f = f.split(/\s+/), b = 0, d = f.length; d > b; b++)
            if (u(f[b]) || e(f[b])) return f[b]
    }

    function g(a, b) {
        var c, d = {};
        for (c in a) d[c] = a[c];
        if (b)
            for (c in b) d[c] = b[c];
        return d
    }

    function h(a) {
        var b = [];
        return function a(d, e) {
            for (var f = d.firstChild; f; f = f.nextSibling) 3 == f.nodeType ? e += f.nodeValue.length : 1 == f.nodeType && (b.push({
                event: "start",
                offset: e,
                node: f
            }), e = a(f, e), c(f).match(/br|hr|img|input/) || b.push({
                event: "stop",
                offset: e,
                node: f
            }));
            return e
        }(a, 0), b
    }

    function i(a, d, e) {
        function f() {
            return a.length && d.length ? a[0].offset != d[0].offset ? a[0].offset < d[0].offset ? a : d : "start" == d[0].event ? a : d : a.length ? a : d
        }

        function g(a) {
            function d(a) {
                return " " + a.nodeName + '="' + b(a.value) + '"'
            }
            k += "<" + c(a) + Array.prototype.map.call(a.attributes, d).join("") + ">"
        }

        function h(a) {
            k += "</" + c(a) + ">"
        }

        function i(a) {
            ("start" == a.event ? g : h)(a.node)
        }
        for (var j = 0, k = "", l = []; a.length || d.length;) {
            var m = f();
            if (k += b(e.substr(j, m[0].offset - j)), j = m[0].offset, m == a) {
                l.reverse().forEach(h);
                do i(m.splice(0, 1)[0]), m = f(); while (m == a && m.length && m[0].offset == j);
                l.reverse().forEach(g)
            } else "start" == m[0].event ? l.push(m[0].node) : l.pop(), i(m.splice(0, 1)[0])
        }
        return k + b(e.substr(j))
    }

    function j(a) {
        function b(a) {
            return a && a.source || a
        }

        function c(c, d) {
            return new RegExp(b(c), "m" + (a.cI ? "i" : "") + (d ? "g" : ""))
        }

        function d(e, f) {
            if (!e.compiled) {
                if (e.compiled = !0, e.k = e.k || e.bK, e.k) {
                    var h = {},
                        i = function(b, c) {
                            a.cI && (c = c.toLowerCase()), c.split(" ").forEach(function(a) {
                                var c = a.split("|");
                                h[c[0]] = [b, c[1] ? Number(c[1]) : 1]
                            })
                        };
                    "string" == typeof e.k ? i("keyword", e.k) : Object.keys(e.k).forEach(function(a) {
                        i(a, e.k[a])
                    }), e.k = h
                }
                e.lR = c(e.l || /\b\w+\b/, !0), f && (e.bK && (e.b = "\\b(" + e.bK.split(" ").join("|") + ")\\b"), e.b || (e.b = /\B|\b/), e.bR = c(e.b), e.e || e.eW || (e.e = /\B|\b/), e.e && (e.eR = c(e.e)), e.tE = b(e.e) || "", e.eW && f.tE && (e.tE += (e.e ? "|" : "") + f.tE)), e.i && (e.iR = c(e.i)), void 0 === e.r && (e.r = 1), e.c || (e.c = []);
                var j = [];
                e.c.forEach(function(a) {
                    a.v ? a.v.forEach(function(b) {
                        j.push(g(a, b))
                    }) : j.push("self" == a ? e : a)
                }), e.c = j, e.c.forEach(function(a) {
                    d(a, e)
                }), e.starts && d(e.starts, f);
                var k = e.c.map(function(a) {
                    return a.bK ? "\\.?(" + a.b + ")\\.?" : a.b
                }).concat([e.tE, e.i]).map(b).filter(Boolean);
                e.t = k.length ? c(k.join("|"), !0) : {
                    exec: function() {
                        return null
                    }
                }
            }
        }
        d(a)
    }

    function k(a, c, e, f) {
        function g(a, b) {
            for (var c = 0; c < b.c.length; c++)
                if (d(b.c[c].bR, a)) return b.c[c]
        }

        function h(a, b) {
            if (d(a.eR, b)) {
                for (; a.endsParent && a.parent;) a = a.parent;
                return a
            }
            return a.eW ? h(a.parent, b) : void 0
        }

        function i(a, b) {
            return !e && d(b.iR, a)
        }

        function m(a, b) {
            var c = t.cI ? b[0].toLowerCase() : b[0];
            return a.k.hasOwnProperty(c) && a.k[c]
        }

        function n(a, b, c, d) {
            var e = d ? "" : v.classPrefix,
                f = '<span class="' + e,
                g = c ? "" : "</span>";
            return f += a + '">', f + b + g
        }

        function o() {
            if (!y.k) return b(B);
            var a = "",
                c = 0;
            y.lR.lastIndex = 0;
            for (var d = y.lR.exec(B); d;) {
                a += b(B.substr(c, d.index - c));
                var e = m(y, d);
                e ? (C += e[1], a += n(e[0], b(d[0]))) : a += b(d[0]), c = y.lR.lastIndex, d = y.lR.exec(B)
            }
            return a + b(B.substr(c))
        }

        function p() {
            var a = "string" == typeof y.sL;
            if (a && !w[y.sL]) return b(B);
            var c = a ? k(y.sL, B, !0, z[y.sL]) : l(B, y.sL.length ? y.sL : void 0);
            return y.r > 0 && (C += c.r), a && (z[y.sL] = c.top), n(c.language, c.value, !1, !0)
        }

        function q() {
            return void 0 !== y.sL ? p() : o()
        }

        function r(a, c) {
            var d = a.cN ? n(a.cN, "", !0) : "";
            a.rB ? (A += d, B = "") : a.eB ? (A += b(c) + d, B = "") : (A += d, B = c), y = Object.create(a, {
                parent: {
                    value: y
                }
            })
        }

        function s(a, c) {
            if (B += a, void 0 === c) return A += q(), 0;
            var d = g(c, y);
            if (d) return A += q(), r(d, c), d.rB ? 0 : c.length;
            var e = h(y, c);
            if (e) {
                var f = y;
                f.rE || f.eE || (B += c), A += q();
                do y.cN && (A += "</span>"), C += y.r, y = y.parent; while (y != e.parent);
                return f.eE && (A += b(c)), B = "", e.starts && r(e.starts, ""), f.rE ? 0 : c.length
            }
            if (i(c, y)) throw new Error('Illegal lexeme "' + c + '" for mode "' + (y.cN || "<unnamed>") + '"');
            return B += c, c.length || 1
        }
        var t = u(a);
        if (!t) throw new Error('Unknown language: "' + a + '"');
        j(t);
        var x, y = f || t,
            z = {},
            A = "";
        for (x = y; x != t; x = x.parent) x.cN && (A = n(x.cN, "", !0) + A);
        var B = "",
            C = 0;
        try {
            for (var D, E, F = 0; y.t.lastIndex = F, D = y.t.exec(c), D;) E = s(c.substr(F, D.index - F), D[0]), F = D.index + E;
            for (s(c.substr(F)), x = y; x.parent; x = x.parent) x.cN && (A += "</span>");
            return {
                r: C,
                value: A,
                language: a,
                top: y
            }
        } catch (a) {
            if (-1 != a.message.indexOf("Illegal")) return {
                r: 0,
                value: b(c)
            };
            throw a
        }
    }

    function l(a, c) {
        c = c || v.languages || Object.keys(w);
        var d = {
                r: 0,
                value: b(a)
            },
            e = d;
        return c.forEach(function(b) {
            if (u(b)) {
                var c = k(b, a, !1);
                c.language = b, c.r > e.r && (e = c), c.r > d.r && (e = d, d = c)
            }
        }), e.language && (d.second_best = e), d
    }

    function m(a) {
        return v.tabReplace && (a = a.replace(/^((<[^>]+>|\t)+)/gm, function(a, b) {
            return b.replace(/\t/g, v.tabReplace)
        })), v.useBR && (a = a.replace(/\n/g, "<br>")), a
    }

    function n(a, b, c) {
        var d = b ? x[b] : c,
            e = [a.trim()];
        return a.match(/\bhljs\b/) || e.push("hljs"), -1 === a.indexOf(d) && e.push(d), e.join(" ").trim()
    }

    function o(a) {
        var b = f(a);
        if (!e(b)) {
            var c;
            v.useBR ? (c = document.createElementNS("http://www.w3.org/1999/xhtml", "div"), c.innerHTML = a.innerHTML.replace(/\n/g, "").replace(/<br[ \/]*>/g, "\n")) : c = a;
            var d = c.textContent,
                g = b ? k(b, d, !0) : l(d),
                j = h(c);
            if (j.length) {
                var o = document.createElementNS("http://www.w3.org/1999/xhtml", "div");
                o.innerHTML = g.value, g.value = i(j, h(o), d)
            }
            g.value = m(g.value), a.innerHTML = g.value, a.className = n(a.className, b, g.language), a.result = {
                language: g.language,
                re: g.r
            }, g.second_best && (a.second_best = {
                language: g.second_best.language,
                re: g.second_best.r
            })
        }
    }

    function p(a) {
        v = g(v, a)
    }

    function q() {
        if (!q.called) {
            q.called = !0;
            var a = document.querySelectorAll("pre code");
            Array.prototype.forEach.call(a, o)
        }
    }

    function r() {
        addEventListener("DOMContentLoaded", q, !1), addEventListener("load", q, !1)
    }

    function s(b, c) {
        var d = w[b] = c(a);
        d.aliases && d.aliases.forEach(function(a) {
            x[a] = b
        })
    }

    function t() {
        return Object.keys(w)
    }

    function u(a) {
        return a = (a || "").toLowerCase(), w[a] || w[x[a]]
    }
    var v = {
            classPrefix: "hljs-",
            tabReplace: null,
            useBR: !1,
            languages: void 0
        },
        w = {},
        x = {};
    return a.highlight = k, a.highlightAuto = l, a.fixMarkup = m, a.highlightBlock = o, a.configure = p, a.initHighlighting = q, a.initHighlightingOnLoad = r, a.registerLanguage = s, a.listLanguages = t, a.getLanguage = u, a.inherit = g, a.IR = "[a-zA-Z]\\w*", a.UIR = "[a-zA-Z_]\\w*", a.NR = "\\b\\d+(\\.\\d+)?", a.CNR = "(\\b0[xX][a-fA-F0-9]+|(\\b\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?)", a.BNR = "\\b(0b[01]+)", a.RSR = "!|!=|!==|%|%=|&|&&|&=|\\*|\\*=|\\+|\\+=|,|-|-=|/=|/|:|;|<<|<<=|<=|<|===|==|=|>>>=|>>=|>=|>>>|>>|>|\\?|\\[|\\{|\\(|\\^|\\^=|\\||\\|=|\\|\\||~", a.BE = {
        b: "\\\\[\\s\\S]",
        r: 0
    }, a.ASM = {
        cN: "string",
        b: "'",
        e: "'",
        i: "\\n",
        c: [a.BE]
    }, a.QSM = {
        cN: "string",
        b: '"',
        e: '"',
        i: "\\n",
        c: [a.BE]
    }, a.PWM = {
        b: /\b(a|an|the|are|I|I'm|isn't|don't|doesn't|won't|but|just|should|pretty|simply|enough|gonna|going|wtf|so|such|will|you|your|like)\b/
    }, a.C = function(b, c, d) {
        var e = a.inherit({
            cN: "comment",
            b: b,
            e: c,
            c: []
        }, d || {});
        return e.c.push(a.PWM), e.c.push({
            cN: "doctag",
            b: "(?:TODO|FIXME|NOTE|BUG|XXX):",
            r: 0
        }), e
    }, a.CLCM = a.C("//", "$"), a.CBCM = a.C("/\\*", "\\*/"), a.HCM = a.C("#", "$"), a.NM = {
        cN: "number",
        b: a.NR,
        r: 0
    }, a.CNM = {
        cN: "number",
        b: a.CNR,
        r: 0
    }, a.BNM = {
        cN: "number",
        b: a.BNR,
        r: 0
    }, a.CSSNM = {
        cN: "number",
        b: a.NR + "(%|em|ex|ch|rem|vw|vh|vmin|vmax|cm|mm|in|pt|pc|px|deg|grad|rad|turn|s|ms|Hz|kHz|dpi|dpcm|dppx)?",
        r: 0
    }, a.RM = {
        cN: "regexp",
        b: /\//,
        e: /\/[gimuy]*/,
        i: /\n/,
        c: [a.BE, {
            b: /\[/,
            e: /\]/,
            r: 0,
            c: [a.BE]
        }]
    }, a.TM = {
        cN: "title",
        b: a.IR,
        r: 0
    }, a.UTM = {
        cN: "title",
        b: a.UIR,
        r: 0
    }, a
}), hljs.registerLanguage("propane", function(a) {
    return {
        aliases: ["pro"],
        k: {
            keyword: "define",
            literal: "true false",
            built_in: "in out exit enter path through avoid later valleyfree end start any drop"
        },
        c: [{
            cN: "number",
            b: "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+(/[0-9]+)?",
            r: 0
        },  {
            cN: "number",
            b: "[0-9]+\\.\\.[0-9]+",
            r: 0
        }]
    }
});
hljs.registerLanguage("quagga", function(a) {
    return {
        aliases: ["cfg"],
        k: {
            keyword: "interface router id route map",
            literal: "",
            built_in: "neighbor match set"
        },
        c: [{
            cN: "number",
            b: "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+(/[0-9]+)?",
            r: 0
        }, {
            cN: "number",
            b: "[0-9]+\\:[0-9]+",
            r: 0
        }]
    }
});
hljs.registerLanguage("xml", function(s) {
    var e = "[A-Za-z0-9\\._:-]+",
        t = {
            eW: !0,
            i: /</,
            r: 0,
            c: [{
                cN: "attr",
                b: e,
                r: 0
            }, {
                b: /=\s*/,
                r: 0,
                c: [{
                    cN: "string",
                    endsParent: !0,
                    v: [{
                        b: /"/,
                        e: /"/
                    }, {
                        b: /'/,
                        e: /'/
                    }, {
                        b: /[^\s"'=<>`]+/
                    }]
                }]
            }]
        };
    return {
        aliases: ["html", "xhtml", "rss", "atom", "xjb", "xsd", "xsl", "plist"],
        cI: !0,
        c: [{
            cN: "meta",
            b: "<!DOCTYPE",
            e: ">",
            r: 10,
            c: [{
                b: "\\[",
                e: "\\]"
            }]
        }, s.C("<!--", "-->", {
            r: 10
        }), {
            b: "<\\!\\[CDATA\\[",
            e: "\\]\\]>",
            r: 10
        }, {
            b: /<\?(php)?/,
            e: /\?>/,
            sL: "php",
            c: [{
                b: "/\\*",
                e: "\\*/",
                skip: !0
            }]
        }, {
            cN: "tag",
            b: "<style(?=\\s|>|$)",
            e: ">",
            k: {
                name: "style"
            },
            c: [t],
            starts: {
                e: "</style>",
                rE: !0,
                sL: ["css", "xml"]
            }
        }, {
            cN: "tag",
            b: "<script(?=\\s|>|$)",
            e: ">",
            k: {
                name: "script"
            },
            c: [t],
            starts: {
                e: "</script>",
                rE: !0,
                sL: ["actionscript", "javascript", "handlebars", "xml"]
            }
        }, {
            cN: "meta",
            v: [{
                b: /<\?xml/,
                e: /\?>/,
                r: 10
            }, {
                b: /<\?\w+/,
                e: /\?>/
            }]
        }, {
            cN: "tag",
            b: "</?",
            e: "/?>",
            c: [{
                cN: "name",
                b: /[^\/><\s]+/,
                r: 0
            }, t]
        }]
    }
});