# c-mlir

## A translator from c to MLIR

Only a subset of c is supported based on the current semantics of MLIR dialects.

* Scalar builtin types -> MLIR types
* Static sizes of array and operations -> MemRef dialect
* Arithmetic operations -> Arith dialect
* A limited version of flow control(for/if) -> Affine/SCF dialect

For `for`, if possible, try to lower to `affine.for`, and if not, try to lower to `scf.for`, elsewise `scf.while`.

For opencl, `__local` is mapped to memory space `1`, `__global` is mapped to memory space `2`.

Dynamic sizes array, `break`, `continue`, `goto`, `&` and `switch`/`case` are not supported.

```c
#define N 1
#define M 2
#define K 3
void matmul() {
  float lhs[N][K];
  float rhs[K][M];
  float output[N][M];
  for (int i=0; i<N; ++i) {
    for (int j=0; j<M; ++j) {
      output[i][j] = 0.0;
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
    %cst = arith.constant 0.000000e+00 : f32
    %0 = memref.alloca() : memref<1x3xf32>
    %1 = memref.alloca() : memref<3x2xf32>
    %2 = memref.alloca() : memref<1x2xf32>
    affine.for %arg0 = 0 to 1 {
      affine.for %arg1 = 0 to 2 {
        affine.store %cst, %2[%arg0, %arg1] : memref<1x2xf32>
        affine.for %arg2 = 0 to 3 {
          %3 = affine.load %2[%arg0, %arg1] : memref<1x2xf32>
          %4 = affine.load %0[%arg0, %arg2] : memref<1x3xf32>
          %5 = affine.load %1[%arg2, %arg1] : memref<3x2xf32>
          %6 = arith.mulf %4, %5 : f32
          %7 = arith.addf %3, %6 : f32
          affine.store %7, %2[%arg0, %arg1] : memref<1x2xf32>
        }
      }
    }
    return
  }
}
```


```c
int get_global_id(int);

__kernel void GEMM(const int M, const int N, const int K,
                  const __global float* A,
                  const __global float* B,
                  __global float* C) {
    
    // Thread identifiers
    const int globalRow = get_global_id(0); // Row ID of C (0..M)
    const int globalCol = get_global_id(1); // Col ID of C (0..N)
 
    // Compute a single element (loop over K)
    float acc = 0.0f;
    for (int k=0; k<K; k++) {
        acc += A[k*M + globalRow] * B[globalCol*K + k];
    }
 
    // Store the result
    C[globalCol*M + globalRow] = acc;
}
```

Output IR as below:

```mlir
module  {
  func private @get_global_id(i32) -> i32
  func @GEMM(%arg0: i32, %arg1: i32, %arg2: i32, %arg3: memref<?xf32, 2>, %arg4: memref<?xf32, 2>, %arg5: memref<?xf32, 2>) attributes {cl.kernel = true, llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %c0 = arith.constant 0 : index
    %c1_i32 = arith.constant 1 : i32
    %c0_i32 = arith.constant 0 : i32
    %cst = arith.constant 0.000000e+00 : f32
    %0 = call @get_global_id(%c0_i32) : (i32) -> i32
    %1 = memref.alloca() : memref<i32>
    affine.store %0, %1[] : memref<i32>
    %2 = call @get_global_id(%c1_i32) : (i32) -> i32
    %3 = memref.alloca() : memref<i32>
    affine.store %2, %3[] : memref<i32>
    %4 = memref.alloca() : memref<f32>
    affine.store %cst, %4[] : memref<f32>
    %5 = arith.index_cast %arg2 : i32 to index
    scf.for %arg6 = %c0 to %5 step %c1 {
      %12 = arith.index_cast %arg6 : index to i32
      %13 = affine.load %4[] : memref<f32>
      %14 = arith.muli %12, %arg0 : i32
      %15 = affine.load %1[] : memref<i32>
      %16 = arith.addi %14, %15 : i32
      %17 = arith.index_cast %16 : i32 to index
      %18 = memref.load %arg3[%17] : memref<?xf32, 2>
      %19 = affine.load %3[] : memref<i32>
      %20 = arith.muli %19, %arg2 : i32
      %21 = arith.addi %20, %12 : i32
      %22 = arith.index_cast %21 : i32 to index
      %23 = memref.load %arg4[%22] : memref<?xf32, 2>
      %24 = arith.mulf %18, %23 : f32
      %25 = arith.addf %13, %24 : f32
      affine.store %25, %4[] : memref<f32>
    }
    %6 = affine.load %4[] : memref<f32>
    %7 = affine.load %3[] : memref<i32>
    %8 = arith.muli %7, %arg0 : i32
    %9 = affine.load %1[] : memref<i32>
    %10 = arith.addi %8, %9 : i32
    %11 = arith.index_cast %10 : i32 to index
    memref.store %6, %arg5[%11] : memref<?xf32, 2>
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