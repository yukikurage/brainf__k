"use strict";

exports.exec_ = (just) => (nothing) => (input) => (compiled) => () => {
  let output = "";

  let i = 0;

  const instance = new WebAssembly.Instance(compiled, {
    env: {
      log: (value) => {
        console.log(value);
      },
      output: (value) => {
        output += String.fromCodePoint(value);
      },
      input: (value) => {
        if (i < input.length) return input.codePointAt(i++);
        return value;
      },
    },
  });

  // const workerContent = func;

  // console.log(workerContent);

  // const workerUrl = URL.createObjectURL(new Blob([workerContent]));

  // const worker = new Worker(workerUrl);

  // let terminateCallbackFunc = () => {};
  // const terminateCallback = () => terminateCallbackFunc();

  const res = {
    getOutput: () => {
      const res = output;
      output = "";
      return res;
    },
    waitFinish: new Promise((resolve) => {
      // worker.addEventListener(
      //   "message",
      //   (e) => {
      //     if (e.data === "f") {
      //       worker.terminate();
      //       URL.revokeObjectURL(workerUrl);
      //       resolve(nothing);
      //     } else {
      //       try {
      //         output += String.fromCodePoint(e.data);
      //       } catch (e) {
      //         resolve(just(e));
      //         worker.terminate();
      //         URL.revokeObjectURL(workerUrl);
      //       }
      //     }
      //   },
      //   false
      // );
      // worker.addEventListener(
      //   "error",
      //   (e) => {
      //     resolve(just(e));
      //     worker.terminate();
      //     URL.revokeObjectURL(workerUrl);
      //   },
      //   false
      // );
      // terminateCallbackFunc = () => {
      //   worker.terminate();
      //   URL.revokeObjectURL(workerUrl);
      //   resolve(just(new Error("Process terminated")));
      // };
      resolve(nothing);
    }),
    stop: () => {
      // terminateCallback();
    },
  };

  instance.exports.main();

  // worker.postMessage("");
  return res;
};
