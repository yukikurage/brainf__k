const func = `
"use strict";
let pointer=0; //pointer
const memory=Array(MEMORY_SIZE).map(() => 0); //memory
let input="input"; //input
let inputIndex=0; //inputIndex
let output=""; //output
let isStop=false; //isStop
let step=0; //step
let chunk=0; //chunk
const setZeroTimeoutPromise = () => new Promise((resolve) => {
  setZeroTimeout(resolve)();
});
const mod=(a,b)=>(a%b+b)%b;

const wait = (async () => {
  //pointer increment
  pointer=mod(pointer+POINTER_INCREMENT,MEMORY_SIZE);
  step+=POINTER_INCREMENT;
  chunk+=1;

  //reference increment
  memory[pointer]=mod(memory[pointer]+REFERENCE_INCREMENT),CELL_SIZE);
  step+=REFERENCE_INCREMENT;
  chunk+=1;

  //output
  output+=String.fromCodePoint(memory[pointer]);
  step+=1;
  chunk+=1;

  //input
  memory[pointer]=input.codePointAt(inputIndex);
  inputIndex++;
  step+=1;
  chunk+=1;

  //loop
  while(memory[pointer]!==0){
    ...
  }
  if(chunk>=CHUNK_NUM){chunk=0;await setZeroTimeoutPromise();}
  ...;
}();

return {
  getMemory: ()=>[...memory],
  getOutput: ()=>{
    const res=output;
    output="";
    return res;
  },
  getStep: ()=>step,
  stop: ()=>{
    isStop=true;
  },
  waitFinish: wait
}
`;
const testFunc = Function("callback", "setZeroTimeout", func);

testFunc = () => {};
