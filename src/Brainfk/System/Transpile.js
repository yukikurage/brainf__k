"use strict";

const binaryenPromise = require("binaryen");

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
/**
 * memorySize: number
 * cellSize: 0 | 1 | 2
 */
exports.transpile_ =
  ({ memorySize, cellSize }) =>
  (code) =>
    binaryenPromise.then(({ default: binaryen }) => {
      const memoryBase = Math.pow(2, cellSize);

      let position = 0;

      const module = new binaryen.Module();

      const constE = (value) => module.i32.const(value);

      const loadMemoryE = (n) => {
        const targetFunc =
          cellSize === 0
            ? module.i32.load8_u
            : cellSize === 1
            ? module.i32.load16_u
            : module.i32.load;

        return n === 0
          ? targetFunc(0, cellSize, module.local.get(0, binaryen.i32))
          : targetFunc(
              0,
              cellSize,
              module.i32.add(
                module.local.get(0, binaryen.i32),
                constE(n * memoryBase)
              )
            );
      };

      const storeMemoryE = (n, value) => {
        const targetFunc =
          cellSize === 0
            ? module.i32.store8
            : cellSize === 1
            ? module.i32.store16
            : module.i32.store;

        return n === 0
          ? targetFunc(0, cellSize, module.local.get(0, binaryen.i32), value)
          : targetFunc(
              0,
              cellSize,
              module.i32.add(
                module.local.get(0, binaryen.i32),
                constE(n * memoryBase)
              ),
              value
            );
      };

      const outputE = (value) => module.call("output", [value], binaryen.none);

      const inputE = (value) => module.call("input", [value], binaryen.i32);

      const go = (operated) => {
        let pointer = 0;
        let expressions = []; //: binaryen.Expression[]
        let stack = new Map();

        /**
         * @param {number} target
         * @param {Expression} value
         * @returns
         */
        const addE = (target, value) =>
          module.local.set(
            target,
            module.i32.add(module.local.get(target, binaryen.i32), value)
          );

        const use = (n) => {
          const res = stack.get(n);
          if (res === undefined) {
            return [];
          }
          let result = [];
          if (res.type === "Add") {
            result = [
              storeMemoryE(
                n,
                module.i32.add(constE(res.value), loadMemoryE(n))
              ),
            ];
          } else {
            // Set
            result = [storeMemoryE(n, constE(res.value))];
          }

          stack.delete(n);

          if (operated.get(n) !== undefined) {
            operated.set(
              n,
              res.type === "Add" ? res.value + operated.get(n) : res.value
            );
          }
          return result;
        };

        const useAll = () => {
          let result = [];
          for (const entry of stack.entries()) {
            const [n, v] = entry;
            if (v.type === "Add") {
              result.push(
                storeMemoryE(n, module.i32.add(constE(v.value), loadMemoryE(n)))
              );
            } else {
              result.push(storeMemoryE(n, constE(v.value)));
            }
          }
          return result;
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
              expressions.push(
                module.call("output", [constE(res)], binaryen.none)
              );
            } else {
              expressions.push(...use(pointer));
              expressions.push(outputE(loadMemoryE(pointer)));
            }
          }
          if (command === ",") {
            expressions.push(...use(pointer));
            expressions.push(
              storeMemoryE(pointer, inputE(loadMemoryE(pointer)))
            );
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
                loop.expressions.length === 0 &&
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
                  expressions.push(...use(pointer));
                  loop.stack.delete(0);
                  for (const entry of loop.stack.entries()) {
                    const [n, v] = entry;
                    if (v.type === "Set") {
                      expressions.push(...use(pointer + n));
                      expressions.push(
                        module.if(
                          loadMemoryE(pointer),
                          module.block(null, [
                            ...use(pointer + n),
                            storeMemoryE(pointer + n, constE(v.value)),
                          ])
                        )
                      );
                    } else {
                      //Add
                      expressions.push(...use(pointer + n));
                      expressions.push(
                        storeMemoryE(
                          pointer + n,
                          module.i32.add(
                            loadMemoryE(pointer + n),
                            module.i32.mul(
                              constE(v.value),
                              loadMemoryE(pointer)
                            )
                          )
                        )
                      );
                    }
                    operated.set(pointer + n, undefined);
                  }
                  addStack(pointer, "Set", 0);
                }
              } else {
                // 最適化なし
                expressions.push(...useAll());
                pointer !== 0 &&
                  expressions.push(addE(0, constE(pointer * memoryBase)));
                pointer = 0;
                stack.clear();
                operated.clear();

                let result = [];
                for (const entry of loop.stack.entries()) {
                  const [n, v] = entry;
                  if (v.type === "Add") {
                    result.push(
                      storeMemoryE(
                        n,
                        module.i32.add(constE(v.value), loadMemoryE(n))
                      )
                    );
                  } else {
                    result.push(storeMemoryE(n, constE(v.value)));
                  }
                }
                expressions.push(
                  module.loop(
                    `loop${position}`,
                    module.if(
                      loadMemoryE(0),
                      module.block(null, [
                        ...loop.expressions,
                        ...result,
                        ...(loop.pointer !== 0
                          ? [addE(0, constE(loop.pointer * memoryBase))]
                          : []),
                        module.br(`loop${position}`),
                      ])
                    )
                  )
                );
              }
            }
          }
          position++;
        }

        return { pointer, stack, expressions };
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
        binaryen.i32,
        binaryen.i32
      ); // {env: {input: (data: none) => i32}} で渡す

      module.addFunction(
        "main",
        binaryen.none,
        binaryen.none,
        [
          binaryen.i32, // temp .. 0
        ],
        module.block(null, [
          module.local.set(0, constE(128)),
          ...res.expressions,
          module.return(),
        ])
      );

      module.addFunctionExport("main", "main");
      module.setMemory(2, 2); // {env: {memory: メモリ}} で渡す
      module.addMemoryExport("0", "memory");

      console.log(module.emitText());

      module.optimize();

      if (!module.validate()) throw new Error("validation error");

      const wasmData = module.emitBinary();

      return wasmData;
    });
