"use strict";

exports.exec_ = (callback) => (setZeroTimeout) => (func) => () =>
  Function("callback", "setZeroTimeout", func)(callback, setZeroTimeout);
