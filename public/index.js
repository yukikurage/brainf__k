(() => {
  var __getOwnPropNames = Object.getOwnPropertyNames;
  var __commonJS = (cb, mod) => function __require() {
    return mod || (0, cb[__getOwnPropNames(cb)[0]])((mod = { exports: {} }).exports, mod), mod.exports;
  };

  // output/Effect.Console/foreign.js
  var require_foreign = __commonJS({
    "output/Effect.Console/foreign.js"(exports) {
      "use strict";
      exports.log = function(s) {
        return function() {
          console.log(s);
        };
      };
      exports.warn = function(s) {
        return function() {
          console.warn(s);
        };
      };
      exports.error = function(s) {
        return function() {
          console.error(s);
        };
      };
      exports.info = function(s) {
        return function() {
          console.info(s);
        };
      };
      exports.time = function(s) {
        return function() {
          console.time(s);
        };
      };
      exports.timeLog = function(s) {
        return function() {
          console.timeLog(s);
        };
      };
      exports.timeEnd = function(s) {
        return function() {
          console.timeEnd(s);
        };
      };
      exports.clear = function() {
        console.clear();
      };
    }
  });

  // output/Data.Show/foreign.js
  var require_foreign2 = __commonJS({
    "output/Data.Show/foreign.js"(exports) {
      "use strict";
      exports.showIntImpl = function(n) {
        return n.toString();
      };
      exports.showNumberImpl = function(n) {
        var str = n.toString();
        return isNaN(str + ".0") ? str : str + ".0";
      };
      exports.showCharImpl = function(c) {
        var code = c.charCodeAt(0);
        if (code < 32 || code === 127) {
          switch (c) {
            case "\x07":
              return "'\\a'";
            case "\b":
              return "'\\b'";
            case "\f":
              return "'\\f'";
            case "\n":
              return "'\\n'";
            case "\r":
              return "'\\r'";
            case "	":
              return "'\\t'";
            case "\v":
              return "'\\v'";
          }
          return "'\\" + code.toString(10) + "'";
        }
        return c === "'" || c === "\\" ? "'\\" + c + "'" : "'" + c + "'";
      };
      exports.showStringImpl = function(s) {
        var l = s.length;
        return '"' + s.replace(/[\0-\x1F\x7F"\\]/g, function(c, i) {
          switch (c) {
            case '"':
            case "\\":
              return "\\" + c;
            case "\x07":
              return "\\a";
            case "\b":
              return "\\b";
            case "\f":
              return "\\f";
            case "\n":
              return "\\n";
            case "\r":
              return "\\r";
            case "	":
              return "\\t";
            case "\v":
              return "\\v";
          }
          var k = i + 1;
          var empty = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
          return "\\" + c.charCodeAt(0).toString(10) + empty;
        }) + '"';
      };
      exports.showArrayImpl = function(f) {
        return function(xs) {
          var ss = [];
          for (var i = 0, l = xs.length; i < l; i++) {
            ss[i] = f(xs[i]);
          }
          return "[" + ss.join(",") + "]";
        };
      };
      exports.cons = function(head) {
        return function(tail) {
          return [head].concat(tail);
        };
      };
      exports.join = function(separator) {
        return function(xs) {
          return xs.join(separator);
        };
      };
    }
  });

  // output/Data.Symbol/foreign.js
  var require_foreign3 = __commonJS({
    "output/Data.Symbol/foreign.js"(exports) {
      "use strict";
      exports.unsafeCoerce = function(arg) {
        return arg;
      };
    }
  });

  // output/Type.Proxy/index.js
  var require_Type = __commonJS({
    "output/Type.Proxy/index.js"(exports, module) {
      "use strict";
      var Proxy3 = function() {
        function Proxy32() {
        }
        ;
        Proxy32.value = new Proxy32();
        return Proxy32;
      }();
      var Proxy2 = function() {
        function Proxy22() {
        }
        ;
        Proxy22.value = new Proxy22();
        return Proxy22;
      }();
      var $$Proxy = function() {
        function $$Proxy2() {
        }
        ;
        $$Proxy2.value = new $$Proxy2();
        return $$Proxy2;
      }();
      module.exports = {
        "Proxy": $$Proxy,
        Proxy2,
        Proxy3
      };
    }
  });

  // output/Data.Symbol/index.js
  var require_Data = __commonJS({
    "output/Data.Symbol/index.js"(exports, module) {
      "use strict";
      var $foreign = require_foreign3();
      var Type_Proxy = require_Type();
      var SProxy = function() {
        function SProxy2() {
        }
        ;
        SProxy2.value = new SProxy2();
        return SProxy2;
      }();
      var reifySymbol = function(s) {
        return function(f) {
          return $foreign.unsafeCoerce(function(dictIsSymbol) {
            return f(dictIsSymbol);
          })({
            reflectSymbol: function(v) {
              return s;
            }
          })(Type_Proxy["Proxy"].value);
        };
      };
      var reflectSymbol = function(dict) {
        return dict.reflectSymbol;
      };
      module.exports = {
        reflectSymbol,
        reifySymbol,
        SProxy
      };
    }
  });

  // output/Record.Unsafe/foreign.js
  var require_foreign4 = __commonJS({
    "output/Record.Unsafe/foreign.js"(exports) {
      "use strict";
      exports.unsafeHas = function(label) {
        return function(rec) {
          return {}.hasOwnProperty.call(rec, label);
        };
      };
      exports.unsafeGet = function(label) {
        return function(rec) {
          return rec[label];
        };
      };
      exports.unsafeSet = function(label) {
        return function(value) {
          return function(rec) {
            var copy = {};
            for (var key in rec) {
              if ({}.hasOwnProperty.call(rec, key)) {
                copy[key] = rec[key];
              }
            }
            copy[label] = value;
            return copy;
          };
        };
      };
      exports.unsafeDelete = function(label) {
        return function(rec) {
          var copy = {};
          for (var key in rec) {
            if (key !== label && {}.hasOwnProperty.call(rec, key)) {
              copy[key] = rec[key];
            }
          }
          return copy;
        };
      };
    }
  });

  // output/Record.Unsafe/index.js
  var require_Record = __commonJS({
    "output/Record.Unsafe/index.js"(exports, module) {
      "use strict";
      var $foreign = require_foreign4();
      module.exports = {
        unsafeHas: $foreign.unsafeHas,
        unsafeGet: $foreign.unsafeGet,
        unsafeSet: $foreign.unsafeSet,
        unsafeDelete: $foreign.unsafeDelete
      };
    }
  });

  // output/Data.Show/index.js
  var require_Data2 = __commonJS({
    "output/Data.Show/index.js"(exports, module) {
      "use strict";
      var $foreign = require_foreign2();
      var Data_Symbol = require_Data();
      var Record_Unsafe = require_Record();
      var Type_Proxy = require_Type();
      var showString = {
        show: $foreign.showStringImpl
      };
      var showRecordFieldsNil = {
        showRecordFields: function(v) {
          return function(v1) {
            return [];
          };
        }
      };
      var showRecordFields = function(dict) {
        return dict.showRecordFields;
      };
      var showRecord = function() {
        return function(dictShowRecordFields) {
          return {
            show: function(record) {
              var v = showRecordFields(dictShowRecordFields)(Type_Proxy["Proxy"].value)(record);
              if (v.length === 0) {
                return "{}";
              }
              ;
              return $foreign.join(" ")(["{", $foreign.join(", ")(v), "}"]);
            }
          };
        };
      };
      var showProxy3 = {
        show: function(v) {
          return "Proxy3";
        }
      };
      var showProxy2 = {
        show: function(v) {
          return "Proxy2";
        }
      };
      var showProxy = {
        show: function(v) {
          return "Proxy";
        }
      };
      var showNumber = {
        show: $foreign.showNumberImpl
      };
      var showInt = {
        show: $foreign.showIntImpl
      };
      var showChar = {
        show: $foreign.showCharImpl
      };
      var showBoolean = {
        show: function(v) {
          if (v) {
            return "true";
          }
          ;
          if (!v) {
            return "false";
          }
          ;
          throw new Error("Failed pattern match at Data.Show (line 20, column 1 - line 22, column 23): " + [v.constructor.name]);
        }
      };
      var show = function(dict) {
        return dict.show;
      };
      var showArray = function(dictShow) {
        return {
          show: $foreign.showArrayImpl(show(dictShow))
        };
      };
      var showRecordFieldsCons = function(dictIsSymbol) {
        return function(dictShowRecordFields) {
          return function(dictShow) {
            return {
              showRecordFields: function(v) {
                return function(record) {
                  var tail = showRecordFields(dictShowRecordFields)(Type_Proxy["Proxy"].value)(record);
                  var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Type_Proxy["Proxy"].value);
                  var focus = Record_Unsafe.unsafeGet(key)(record);
                  return $foreign.cons($foreign.join(": ")([key, show(dictShow)(focus)]))(tail);
                };
              }
            };
          };
        };
      };
      module.exports = {
        show,
        showRecordFields,
        showBoolean,
        showInt,
        showNumber,
        showChar,
        showString,
        showArray,
        showProxy,
        showProxy2,
        showProxy3,
        showRecord,
        showRecordFieldsNil,
        showRecordFieldsCons
      };
    }
  });

  // output/Effect.Console/index.js
  var require_Effect = __commonJS({
    "output/Effect.Console/index.js"(exports, module) {
      "use strict";
      var $foreign = require_foreign();
      var Data_Show = require_Data2();
      var warnShow = function(dictShow) {
        return function(a) {
          return $foreign.warn(Data_Show.show(dictShow)(a));
        };
      };
      var logShow = function(dictShow) {
        return function(a) {
          return $foreign.log(Data_Show.show(dictShow)(a));
        };
      };
      var infoShow = function(dictShow) {
        return function(a) {
          return $foreign.info(Data_Show.show(dictShow)(a));
        };
      };
      var errorShow = function(dictShow) {
        return function(a) {
          return $foreign.error(Data_Show.show(dictShow)(a));
        };
      };
      module.exports = {
        logShow,
        warnShow,
        errorShow,
        infoShow,
        log: $foreign.log,
        warn: $foreign.warn,
        error: $foreign.error,
        info: $foreign.info,
        time: $foreign.time,
        timeLog: $foreign.timeLog,
        timeEnd: $foreign.timeEnd,
        clear: $foreign.clear
      };
    }
  });

  // output/Main/index.js
  var require_Main = __commonJS({
    "output/Main/index.js"(exports, module) {
      "use strict";
      var Effect_Console = require_Effect();
      var main = Effect_Console.log("\u{1F35D}\u3060\u3088\uFF5E\uFF5E\uFF5E\uFF01\uFF01");
      module.exports = {
        main
      };
    }
  });

  // entry.js
  var Main = require_Main();
  Main.main();
})();
