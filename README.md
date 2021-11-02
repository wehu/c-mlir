# c-mlir

## A translator from c to MLIR

Only a subset of c is supported based on the current semantics of MLIR dialects.

* Scalar builtin types -> MLIR types
* Static sizes of array and operations -> MemRef dialect
* Arithmetic operations -> Arith dialect
* A limited version of flow control(for/if) -> Affine/SCF dialect

For `for`, if possiable, try to lower to `affine.for`, and if not, try to lower to `scf.for`, elsewise `scf.while`.

`Pointer` is translated as `unranked memref`.

For opencl, `__local` is mapped to memory space `1`, `__global` is mapped to memory space `2`.

Dynamic sizes array, `break`, `continue`, `goto`, `&` and `switch`/`case` are not supported.

```c
__kernel void foo(__global float* input, __local float *a) {
  for (int i=0; i<100; i+=1) {
    input[i] = a[i];
    a[i] = input[i];
  }
}
```

Output IR as below:

```mlir
module  {
  func @foo(%arg0: memref<*xf32, 2>, %arg1: memref<*xf32, 1>) {
    affine.for %arg2 = 0 to 100 {
      %0 = arith.index_cast %arg2 : index to i32
      %1 = arith.index_cast %0 : i32 to index
      %2 = memref.cast %arg1 : memref<*xf32, 1> to memref<?xf32, 1>
      %3 = memref.load %2[%1] : memref<?xf32, 1>
      %4 = arith.index_cast %0 : i32 to index
      %5 = memref.cast %arg0 : memref<*xf32, 2> to memref<?xf32, 2>
      memref.store %3, %5[%4] : memref<?xf32, 2>
      %6 = arith.index_cast %0 : i32 to index
      %7 = memref.cast %arg0 : memref<*xf32, 2> to memref<?xf32, 2>
      %8 = memref.load %7[%6] : memref<?xf32, 2>
      %9 = arith.index_cast %0 : i32 to index
      %10 = memref.cast %arg1 : memref<*xf32, 1> to memref<?xf32, 1>
      memref.store %8, %10[%9] : memref<?xf32, 1>
    }
    return
  }
}
```

## Install

Install stack

```shell
curl https://get-ghcup.haskell.org -sSf | sh
```

Install [mlir-hs](https://github.com/wehu/mlir-hs) (forked from [mlir-hs](https://github.com/google/mlir-hs))

```shell
git clone https://github.com/wehu/mlir-hs
cd mlir-hs

# link your llvm-project in mlir-hs directory

mkdir build
cd build
cmake ..
make -j4 install
```

Add local mlir-hs package to stack.yaml as external package

Run tests

```shell
stack test
```

Translate a C file
```shell
stack run -- test.c
```

Translate to LLVM IR
```shell
stack run -- test.c -llvm
```

Dump IR with locations
```shell
stack run -- test.c -loc
```