"use strict";

exports.exec_ = (just) => (nothing)  => (setZeroTimeout) => (func) => () => {
  const res = Function("setZeroTimeout", func)(setZeroTimeout);
  res.waitFinish = res.waitFinish.then(() => nothing).catch((e) => just(e));
  return res;
}
