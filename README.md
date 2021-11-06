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

Basic matmul example:
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

Opencl GEMM example:
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
    %cst = arith.constant 0.000000e+00 : f32
    %c1_i32 = arith.constant 1 : i32
    %c0_i32 = arith.constant 0 : i32
    %0 = arith.index_cast %arg0 : i32 to index
    %1 = arith.index_cast %arg2 : i32 to index
    %2 = call @get_global_id(%c0_i32) : (i32) -> i32
    %3 = arith.index_cast %2 : i32 to index
    %4 = call @get_global_id(%c1_i32) : (i32) -> i32
    %5 = arith.index_cast %4 : i32 to index
    %6 = memref.alloca() : memref<f32>
    affine.store %cst, %6[] : memref<f32>
    affine.for %arg6 = 0 to %1 {
      %8 = affine.load %6[] : memref<f32>
      %9 = affine.load %arg3[%arg6 * symbol(%0) + symbol(%3)] : memref<?xf32, 2>
      %10 = affine.load %arg4[%arg6 + symbol(%5) * symbol(%1)] : memref<?xf32, 2>
      %11 = arith.mulf %9, %10 : f32
      %12 = arith.addf %8, %11 : f32
      affine.store %12, %6[] : memref<f32>
    }
    %7 = affine.load %6[] : memref<f32>
    affine.store %7, %arg5[symbol(%5) * symbol(%0) + symbol(%3)] : memref<?xf32, 2>
    return
  }
}
```

Opencl GEMM with local memory:
```c
#define TS 10
#define CLK_LOCAL_MEM_FENCE 1

int get_local_id(int);
int get_group_id(int);
void barrier(int);

// Tiled and coalesced version
__kernel void myGEMM2(const int M, const int N, const int K,
                      const __global float* A,
                      const __global float* B,
                      __global float* C,
                     // Local memory to fit a tile of TS*TS elements of A and B
                     __local float Asub[TS][TS],
                     __local float Bsub[TS][TS]) {
    
    // Thread identifiers
    const int row = get_local_id(0); // Local row ID (max: TS)
    const int col = get_local_id(1); // Local col ID (max: TS)
    const int globalRow = TS*get_group_id(0) + row; // Row ID of C (0..M)
    const int globalCol = TS*get_group_id(1) + col; // Col ID of C (0..N)
 
 
    // Initialise the accumulation register
    float acc = 0.0f;
    
    // Loop over all tiles
    const int numTiles = K/TS;
    for (int t=0; t<numTiles; t++) {
 
        // Load one tile of A and B into local memory
        const int tiledRow = TS*t + row;
        const int tiledCol = TS*t + col;
        Asub[col][row] = A[tiledCol*M + globalRow];
        Bsub[col][row] = B[globalCol*K + tiledRow];
 
        // Synchronise to make sure the tile is loaded
        barrier(CLK_LOCAL_MEM_FENCE);
 
        // Perform the computation for a single tile
        for (int k=0; k<TS; k++) {
            acc += Asub[k][row] * Bsub[col][k];
        }
 
        // Synchronise before loading the next tile
        barrier(CLK_LOCAL_MEM_FENCE);
    }
 
    // Store the final result in C
    C[globalCol*M + globalRow] = acc;
}
```

Output IR as below:
```mlir
module  {
  func private @get_local_id(i32) -> i32
  func private @get_group_id(i32) -> i32
  func private @barrier(i32)
  func @myGEMM2(%arg0: i32, %arg1: i32, %arg2: i32, %arg3: memref<?xf32, 2>, %arg4: memref<?xf32, 2>, %arg5: memref<?xf32, 2>, %arg6: memref<10x10xf32, 1>, %arg7: memref<10x10xf32, 1>) attributes {cl.kernel = true, llvm.emit_c_interface} {
    %c10_i32 = arith.constant 10 : i32
    %cst = arith.constant 0.000000e+00 : f32
    %c1_i32 = arith.constant 1 : i32
    %c0_i32 = arith.constant 0 : i32
    %0 = arith.index_cast %arg0 : i32 to index
    %1 = call @get_local_id(%c0_i32) : (i32) -> i32
    %2 = arith.index_cast %1 : i32 to index
    %3 = call @get_local_id(%c1_i32) : (i32) -> i32
    %4 = arith.index_cast %3 : i32 to index
    %5 = call @get_group_id(%c0_i32) : (i32) -> i32
    %6 = arith.muli %5, %c10_i32 : i32
    %7 = arith.addi %6, %1 : i32
    %8 = arith.index_cast %7 : i32 to index
    %9 = call @get_group_id(%c1_i32) : (i32) -> i32
    %10 = arith.muli %9, %c10_i32 : i32
    %11 = arith.addi %10, %3 : i32
    %12 = arith.index_cast %11 : i32 to index
    %13 = memref.alloca() : memref<f32>
    affine.store %cst, %13[] : memref<f32>
    %14 = arith.divsi %arg2, %c10_i32 : i32
    %15 = arith.index_cast %14 : i32 to index
    affine.for %arg8 = 0 to %15 {
      %17 = arith.index_cast %arg8 : index to i32
      %18 = arith.muli %17, %c10_i32 : i32
      %19 = arith.addi %18, %1 : i32
      %20 = arith.muli %17, %c10_i32 : i32
      %21 = arith.addi %20, %3 : i32
      %22 = arith.muli %21, %arg0 : i32
      %23 = arith.addi %22, %7 : i32
      %24 = arith.index_cast %23 : i32 to index
      %25 = memref.load %arg3[%24] : memref<?xf32, 2>
      affine.store %25, %arg6[symbol(%4), symbol(%2)] : memref<10x10xf32, 1>
      %26 = arith.muli %11, %arg2 : i32
      %27 = arith.addi %26, %19 : i32
      %28 = arith.index_cast %27 : i32 to index
      %29 = memref.load %arg4[%28] : memref<?xf32, 2>
      affine.store %29, %arg7[symbol(%4), symbol(%2)] : memref<10x10xf32, 1>
      call @barrier(%c1_i32) : (i32) -> ()
      affine.for %arg9 = 0 to 10 {
        %30 = affine.load %13[] : memref<f32>
        %31 = affine.load %arg6[%arg9, symbol(%2)] : memref<10x10xf32, 1>
        %32 = affine.load %arg7[symbol(%4), %arg9] : memref<10x10xf32, 1>
        %33 = arith.mulf %31, %32 : f32
        %34 = arith.addf %30, %33 : f32
        affine.store %34, %13[] : memref<f32>
      }
      call @barrier(%c1_i32) : (i32) -> ()
    }
    %16 = affine.load %13[] : memref<f32>
    affine.store %16, %arg5[symbol(%12) * symbol(%0) + symbol(%8)] : memref<?xf32, 2>
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