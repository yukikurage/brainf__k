"use strict";

const append = (op1, op2) => {
  if (op1.type === "Add" && op2.type === "Add") {
    return { type: "Add", value: op1.value + op2.value };
  }
  if (op1.type === "Set" && op2.type === "Add") {
    return { type: "Set", value: op1.value + op2.value };
  }
  if (op1.type === "Add" && op2.type === "Set") {
    return { type: "Set", value: op2.value };
  }
  if (op1.type === "Set" && op2.type === "Set") {
    return { type: "Set", value: op2.value };
  }
};

const memory = (n) => {
  if (n === 0) {
    return "m[p]";
  }
  if (n > 0) {
    return `m[p+${n}]`;
  }
  if (n < 0) {
    return `m[p-${-n}]`;
  }
};

const addOperation = (left, right) => {
  if (right == 0) {
    return "";
  }
  if (right == 1) {
    return `${left}++;`;
  }
  if (right == -1) {
    return `${left}--;`;
  }
  if (right > 0) {
    return `${left}+=${right};`;
  }
  if (right < 0) {
    return `${left}-=${-right};`;
  }
};

/**
 * memorySize: number
 * cellSize: '8' | '16' | '32'
 */
exports.transpile_ =
  ({ memorySize, cellSize }) =>
  (code) =>
    require("binaryen").then(({ default: binaryen }) => {
      console.log("transpile start");

      let position = 0;

      const module = new binaryen.Module();

      const go = (operated) => {
        let pointer = 0;
        let transpiled = "";
        let expressions = []; //: binaryen.Expression[]
        let stack = new Map();

        const constE = (value) => module.i32.const(value);

        const loadMemoryE = (n) =>
          module.i32.load(
            0,
            0,
            module.i32.add(
              module.local.get(1, binaryen.i32),
              module.i32.const(n)
            )
          );

        const storeMemoryE = (n, value) =>
          module.i32.store(
            0,
            0,
            module.i32.add(
              module.local.get(1, binaryen.i32),
              module.i32.const(n)
            ),
            value
          );

        const outputE = (value) =>
          module.call("output", [value], binaryen.none);

        /**
         * @param {number} target
         * @param {number | Expression} value
         * @returns
         */
        const setE = (target, value) => {
          if (typeof value === "number") {
            return module.local.set(
              target,
              module.i32.add(module.local.get(target), constE(value))
            );
          } else {
            return module.local.set(
              target,
              module.i32.add(module.local.get(target), value)
            );
          }
        };

        /**
         * @param {number} target
         * @param {number | Expression} value
         * @returns
         */
        const addE = (target, value) => {
          if (typeof value === "number") {
            return module.local.set(
              target,
              module.i32.add(module.local.get(target), constE(value))
            );
          } else {
            return module.local.set(
              target,
              module.i32.add(module.local.get(target), value)
            );
          }
        };

        const inputE = module.call("input", [], binaryen.i32);

        const use = (n) => {
          const res = stack.get(n);
          if (res === undefined) {
            return;
          }
          if (res.type === "Add") {
            transpiled += addOperation(memory(n), res.value);
            expressions.push(
              storeMemoryE(n, module.i32.add(constE(res.value), loadMemoryE(n)))
            );
          } else {
            // Set
            transpiled += `${memory(n)}=${res.value};`;
            expressions.push(storeMemoryE(n, constE(res.value)));
          }
          stack.delete(n);
          if (operated.get(n) !== undefined) {
            operated.set(
              n,
              res.type === "Add" ? res.value + operated.get(n) : res.value
            );
          }
        };

        const useAll = () => {
          for (const entry of stack.entries()) {
            const [n, v] = entry;
            if (v.type === "Add") {
              transpiled += addOperation(memory(n), v.value);
              expressions.push(
                storeMemoryE(n, module.i32.add(constE(v.value), loadMemoryE(n)))
              );
            } else {
              transpiled += `${memory(n)}=${v.value};`;
              expressions.push(storeMemoryE(n, constE(v.value)));
            }
          }
          stack.clear();
          operated = new Map();
        };

        const knownValue = (n) => {
          const res = stack.get(n);
          if (res !== undefined && res.type === "Set") {
            return res.value;
          }
          if (res === undefined && operated.get(n) !== undefined) {
            return operated.get(n);
          }
          if (
            res !== undefined &&
            res.type === "Add" &&
            operated.get(n) !== undefined
          ) {
            return res.value + operated.get(n);
          }
          return undefined;
        };

        const addStack = (n, type, value) => {
          const res = stack.get(n);
          if (res === undefined) {
            stack.set(n, { type, value });
          } else {
            stack.set(n, append(res, { type, value }));
          }
        };

        while (true) {
          const command = code[position];
          // Break
          if (position >= code.length || command === "]") {
            break;
          }

          // メイン処理
          if (command === ">") {
            pointer++;
          }
          if (command === "<") {
            pointer--;
          }
          if (command === "+") {
            addStack(pointer, "Add", 1);
          }
          if (command === "-") {
            addStack(pointer, "Add", -1);
          }
          if (command === ".") {
            const res = knownValue(pointer);
            if (res !== undefined) {
              transpiled += `f(${res});`;
              module.call("output", [module.i32.const(res)], binaryen.none);
            } else {
              use(pointer);
              transpiled += `f(${memory(pointer)});`;
              expressions.push(outputE(loadMemoryE(pointer)));
            }
          }
          if (command === ",") {
            use(pointer);
            transpiled += `if(x<i.length){${memory(pointer)}=i[x++];}`;
            expressions.push(storeMemoryE(pointer, inputE));
            operated.delete(pointer);
          }
          if (command === "[") {
            position++;
            const loop = go(new Map());
            const res = knownValue(pointer);
            if (res === 0) {
              // do nothing
            } else {
              if (
                loop.pointer === 0 &&
                loop.transpiled === "" &&
                loop.expressions === [] &&
                loop.stack.get(0) &&
                loop.stack.get(0).type === "Add" &&
                loop.stack.get(0).value === -1
              ) {
                // 最適化 1
                if (res !== undefined) {
                  for (const entry of loop.stack.entries()) {
                    const [n, v] = entry;
                    addStack(
                      n + pointer,
                      v.type,
                      v.type === "Set" ? v.value : v.value * res
                    );
                  }
                  addStack(pointer, "Set", 0);
                } else {
                  // 最適化 2
                  use(pointer);
                  loop.stack.delete(0);
                  transpiled += `t=${memory(pointer)};`;
                  expressions.push(setE(0, loadMemoryE(pointer)));
                  expressions.push();
                  for (const entry of loop.stack.entries()) {
                    const [n, v] = entry;
                    if (v.type === "Set") {
                      transpiled += `if(t){${memory(pointer + n)}=${v.value};}`;
                      transpiled += "else{";
                      use(pointer + n);
                      transpiled += "}";
                    } else {
                      //Add
                      use(pointer + n);
                      transpiled += memory(pointer + n);
                      (() => {
                        if (v.value === 0) {
                          return;
                        }
                        if (v.value === 1) {
                          transpiled += `+=t;`;
                          return;
                        }
                        if (v.value >= 2) {
                          transpiled += `+=${v.value}*t;`;
                          return;
                        }
                        if (v.value === -1) {
                          transpiled += `-=t;`;
                          return;
                        }
                        if (v.value <= -2) {
                          transpiled += `-=${-v.value}*t;`;
                          return;
                        }
                      })();
                    }
                    operated.set(pointer + n, undefined);
                  }
                  addStack(pointer, "Set", 0);
                }
              } else {
                // 最適化なし
                useAll();
                transpiled += addOperation("p", pointer);
                pointer = 0;
                stack.clear();
                transpiled += `while(m[p]){${loop.transpiled}`;
                for (const entry of loop.stack.entries()) {
                  const [n, v] = entry;
                  if (v.type === "Add") {
                    transpiled += addOperation(memory(n), v.value);
                  } else {
                    transpiled += `${memory(n)}=${v.value};`;
                  }
                }
                transpiled += addOperation("p", loop.pointer);
                transpiled += "}";
              }
            }
          }
          position++;
        }

        return { pointer, transpiled, stack, expressions };
      };

      const res = go(
        new Map(new Array(memorySize).fill(0).map((_, i) => [i, 0]))
      );

      module.addFunctionImport(
        "output",
        "env",
        "output",
        binaryen.i32,
        binaryen.none
      ); // {env: {output: (data: i32) => void}} で渡す
      module.addFunctionImport(
        "input",
        "env",
        "input",
        binaryen.none,
        binaryen.i32
      ); // {env: {input: (data: none) => i32}} で渡す
      module.setMemory(2, 2); // {env: {memory: メモリ}} で渡す

      module.addFunction(
        "main",
        binaryen.none,
        binaryen.none,
        [
          binaryen.i32, // temp .. 0
          binaryen.i32, // pointer .. 1
        ],
        module.block(null, [
          module.local.set(0, module.i32.const(0)),
          module.local.set(1, module.i32.const(0)),
          ...res.expressions,
          module.return(),
        ])
      );

      module.addFunctionExport("main", "main");

      const textData1 = module.emitText();
      console.log(textData1);

      module.optimize();

      if (!module.validate()) throw new Error("validation error");

      const textData2 = module.emitText();
      console.log("optimized");
      console.log(textData2);

      const wasmData = module.emitBinary();

      return new WebAssembly.Module(wasmData);

      // return `self.addEventListener('message',()=>{let t=0;let p=0;let m=new Uint${cellSize}Array(${memorySize});let i=[${input
      //   .split("")
      //   .map((v) => v.codePointAt(0))}];let x=0;let f=postMessage;${
      //   res.transpiled
      // }f('f');},false);`;
    });
