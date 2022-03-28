"use strict";

addEventListener(
  "message",
  (e) => {
    let i = 0;

    const input = e.data.input;
    const compiled = e.data.compiled;

    const instance = new WebAssembly.Instance(new WebAssembly.Module(compiled), {
      env: {
        output: (value) => {
          postMessage(value);
        },
        input: (value) => {
          if (i < input.length) return e.input.codePointAt(i++);
          return value;
        },
      },
    });

    instance.exports.main();

    postMessage("f");
  },
  false
);
