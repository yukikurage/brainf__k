"use strict";

exports.exec_ = (just) => (nothing) => (input) => (compiled) => () => {
  const worker = new Worker('./ExecWorker.js');

  let output = ""

  let terminateCallback = () => {};

  const res = {
    getOutput: () => {
      const res = output;
      output = "";
      return res;
    },
    waitFinish: new Promise((resolve) => {
      worker.addEventListener(
        "message",
        (e) => {
          if (e.data === "f") {
            worker.terminate();
            resolve(nothing);
          } else {
            try {
              output += String.fromCodePoint(e.data);
            } catch (e) {
              resolve(just(e));
              worker.terminate();
            }
          }
        },
        false
      );
      worker.addEventListener(
        "error",
        (e) => {
          resolve(just(e));
          worker.terminate();
        },
        false
      );
      terminateCallback = () => {
        worker.terminate();
        resolve(just(new Error("Process terminated")));
      };
    }),
    stop: () => {
      terminateCallback();
    },
  };

  worker.postMessage({input, compiled}, compiled.buffer);

  return res;
};
