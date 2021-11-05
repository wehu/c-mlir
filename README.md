# c-mlir

## A translator from c to MLIR

Only a subset of c is supported based on the current semantics of MLIR dialects.

* Scalar builtin types -> MLIR types
* Static sizes of array and operations -> MemRef dialect
* Arithmetic operations -> Arith dialect
* A limited version of flow control(for/if) -> Affine/SCF dialect

For `for`, if possiable, try to lower to `affine.for`, and if not, try to lower to `scf.for`, elsewise `scf.while`.

For opencl, `__local` is mapped to memory space `1`, `__global` is mapped to memory space `2`.

Dynamic sizes array, `break`, `continue`, `goto`, `&` and `switch`/`case` are not supported.

```c
#define N 10
#define M 20
#define K 30

void matmul() {
  float lhs[N][K];
  float rhs[K][M];
  float output[N][M];
  for (int i=0; i<N; ++i) {
    for (int j=0; j<M; ++j) {
      for (int k=0; k<K; ++k) {
        output[i][j] += lhs[i][k] * rhs[k][j];
      }
    }
  }
}
```

Output IR as below:

```mlir
module  {
  func @matmul() attributes {llvm.emit_c_interface} {
    %0 = memref.alloca() : memref<10x30xf32>
    %1 = memref.alloca() : memref<30x20xf32>
    %2 = memref.alloca() : memref<10x20xf32>
    affine.for %arg0 = 0 to 10 {
      affine.for %arg1 = 0 to 20 {
        affine.for %arg2 = 0 to 30 {
          %3 = affine.load %2[%arg0, %arg1] : memref<10x20xf32>
          %4 = affine.load %0[%arg0, %arg2] : memref<10x30xf32>
          %5 = affine.load %1[%arg2, %arg1] : memref<30x20xf32>
          %6 = arith.mulf %4, %5 : f32
          %7 = arith.addf %3, %6 : f32
          affine.store %7, %2[%arg0, %arg1] : memref<10x20xf32>
        }
      }
    }
    return
  }
}
```


```c
__kernel void foo(__global float* input, __local float *a) {
  for (int i=0; i<100; i++) {
    input[i] = a[i];
    a[i] = input[i];
  }
}
```

Output IR as below:

```mlir
module  {
  func @foo(%arg0: memref<?xf32, 2>, %arg1: memref<?xf32, 1>) attributes {cl.kernel = true, llvm.emit_c_interface} {
    affine.for %arg2 = 0 to 100 {
      %0 = affine.load %arg1[%arg2] : memref<?xf32, 1>
      affine.store %0, %arg0[%arg2] : memref<?xf32, 2>
      %1 = affine.load %arg0[%arg2] : memref<?xf32, 2>
      affine.store %1, %arg1[%arg2] : memref<?xf32, 1>
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

Jit run
```shell
stack run -- test.c -jit=func_name
```