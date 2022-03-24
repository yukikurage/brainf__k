"use strict";

exports.exec_ = (just) => (nothing) => (func) => () => {
  const workerContent = `self.addEventListener('message',()=>{${func}},false);`;

  console.log(workerContent);

  const workerUrl = URL.createObjectURL(new Blob([workerContent]));

  const worker = new Worker(workerUrl);

  let output = "";

  let terminateCallbackFunc = () => {};
  const terminateCallback = () => terminateCallbackFunc();

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
            URL.revokeObjectURL(workerUrl);
            resolve(nothing);
          } else {
            try {
              output += String.fromCodePoint(e.data);
            } catch (e) {
              resolve(just(e));
              worker.terminate();
              URL.revokeObjectURL(workerUrl);
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
          URL.revokeObjectURL(workerUrl);
        },
        false
      );
      terminateCallbackFunc = () => {
        worker.terminate();
        URL.revokeObjectURL(workerUrl);
        resolve(just(new Error("Process terminated")));
      };
    }),
    stop: () => {
      terminateCallback();
    },
  };

  worker.postMessage("");
  return res;
};
