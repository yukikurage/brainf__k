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

/**
 * memorySize: number
 * cellSize: '8' | '16' | '32'
 */
exports.transpile_ =
  ({ memorySize, cellSize }) =>
  (code) =>
  (input) => {
    let position = 0;
    const go = () => {
      let pointer = 0;
      let transpiled = "";
      let stack = new Map();

      const use = (n) => {
        const res = stack.get(n);
        if (res === undefined) {
          return;
        }
        if (res.type === "Add") {
          transpiled += `${memory(n)}+=${res.value};`;
        } else {
          transpiled += `${memory(n)}=${res.value};`;
        }
        stack.delete(n);
      };

      const useAll = () => {
        for (const entry of stack.entries()) {
          const [n, v] = entry;
          if (v.type === "Add") {
            transpiled += `${memory(n)}+=${v.value};`;
          } else {
            transpiled += `${memory(n)}=${v.value};`;
          }
        }
        stack.clear();
      };

      const knownValue = (n) => {
        const res = stack.get(n);
        if (res !== undefined && res.type === "Set") {
          return res.value;
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
          } else {
            use(pointer);
            transpiled += `f(${memory(pointer)});`;
          }
        }
        if (command === ",") {
          use(pointer);
          transpiled += `if(x<i.length){${memory(pointer)}=i[x++];}`;
        }
        if (command === "[") {
          position++;
          const loop = go();
          const res = knownValue(pointer);
          if (res === 0) {
            // do nothing
          } else {
            if (
              loop.pointer === 0 &&
              loop.transpiled === "" &&
              loop.stack.get(0) &&
              loop.stack.get(0).type === "Add" &&
              loop.stack.get(0) &&
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
                for (const entry of loop.stack.entries()) {
                  const [n, v] = entry;
                  if (v.type === "Set") {
                    transpiled += `if(${memory(pointer)}){${memory(
                      pointer + n
                    )}=${v.value};}`;
                    transpiled += "else{";
                    use(pointer + n);
                    transpiled += "}";
                  } else {
                    use(pointer + n);
                    transpiled += `${memory(pointer + n)}+=${v.value}*${memory(
                      pointer
                    )};`;
                  }
                }
                addStack(pointer, "Set", 0);
              }
            } else {
              // 最適化なし
              useAll();
              transpiled += `p+=${pointer};`;
              pointer = 0;
              stack.clear();
              transpiled += `while(m[p]){${loop.transpiled}`;
              for (const entry of loop.stack.entries()) {
                const [n, v] = entry;
                if (v.type === "Add") {
                  transpiled += `${memory(n)}+=${v.value};`;
                } else {
                  transpiled += `${memory(n)}=${v.value};`;
                }
              }
              transpiled += `p+=${loop.pointer};}`;
            }
          }
        }
        position++;
      }

      return { pointer, transpiled, stack };
    };

    return `let p=0;let m=new Uint${cellSize}Array(${memorySize});let i=${input};let x=0;let f=postMessage;${
      go().transpiled
    }f('f');`;
  };
