# c-mlir

## A translator from c to MLIR

Only a subset of c is supported based on the current semantics of MLIR dialects.

* Scalar builtin types -> MLIR types
* Static sizes of array and operations -> MemRef dialect
* Arithmetic operations -> Arith dialect
* A limited version of flow control(for/if) -> Affine/SCF dialect
* math function calls -> Math dialect
* 1D operations -> Vector dialect
* 2D operations -> Linalg dialect

For `for`, if possible, try to lower to `affine.for`, and if not, try to lower to `scf.for`, elsewise `scf.while`.

For opencl, `__local` is mapped to memory space `1`, `__global` is mapped to memory space `2`.

Dynamic sizes array, pointer arithmetic, `break`, `continue`, `goto` and `switch`/`case` are not allowed.

### Basic matmul example
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

### Opencl GEMM example
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

### Opencl GEMM with local memory
```c
#define TS 10
#define CLK_LOCAL_MEM_FENCE 1

int get_local_id(int);
int get_group_id(int);
void barrier(int);

// Tiled and coalesced version
__kernel void GEMM(const int M, const int N, const int K,
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
#map = affine_map<()[s0] -> (s0 floordiv 10)>
module  {
  func private @get_local_id(i32) -> i32
  func private @get_group_id(i32) -> i32
  func private @barrier(i32)
  func @GEMM(%arg0: i32, %arg1: i32, %arg2: i32, %arg3: memref<?xf32, 2>, %arg4: memref<?xf32, 2>, %arg5: memref<?xf32, 2>, %arg6: memref<10x10xf32, 1>, %arg7: memref<10x10xf32, 1>) attributes {cl.kernel = true, llvm.emit_c_interface} {
    %c10_i32 = arith.constant 10 : i32
    %cst = arith.constant 0.000000e+00 : f32
    %c1_i32 = arith.constant 1 : i32
    %c0_i32 = arith.constant 0 : i32
    %0 = arith.index_cast %arg0 : i32 to index
    %1 = arith.index_cast %arg2 : i32 to index
    %2 = call @get_local_id(%c0_i32) : (i32) -> i32
    %3 = arith.index_cast %2 : i32 to index
    %4 = call @get_local_id(%c1_i32) : (i32) -> i32
    %5 = arith.index_cast %4 : i32 to index
    %6 = call @get_group_id(%c0_i32) : (i32) -> i32
    %7 = arith.muli %6, %c10_i32 : i32
    %8 = arith.addi %7, %2 : i32
    %9 = arith.index_cast %8 : i32 to index
    %10 = call @get_group_id(%c1_i32) : (i32) -> i32
    %11 = arith.muli %10, %c10_i32 : i32
    %12 = arith.addi %11, %4 : i32
    %13 = arith.index_cast %12 : i32 to index
    %14 = memref.alloca() : memref<1xf32>
    affine.store %cst, %14[0] : memref<1xf32>
    affine.for %arg8 = 0 to #map()[%1] {
      %16 = affine.load %arg3[(%arg8 * 10 + symbol(%5)) * symbol(%0) + symbol(%9)] : memref<?xf32, 2>
      affine.store %16, %arg6[symbol(%5), symbol(%3)] : memref<10x10xf32, 1>
      %17 = affine.load %arg4[%arg8 * 10 + symbol(%3) + symbol(%13) * symbol(%1)] : memref<?xf32, 2>
      affine.store %17, %arg7[symbol(%5), symbol(%3)] : memref<10x10xf32, 1>
      call @barrier(%c1_i32) : (i32) -> ()
      affine.for %arg9 = 0 to 10 {
        %18 = affine.load %14[0] : memref<1xf32>
        %19 = affine.load %arg6[%arg9, symbol(%3)] : memref<10x10xf32, 1>
        %20 = affine.load %arg7[symbol(%5), %arg9] : memref<10x10xf32, 1>
        %21 = arith.mulf %19, %20 : f32
        %22 = arith.addf %18, %21 : f32
        affine.store %22, %14[0] : memref<1xf32>
      }
      call @barrier(%c1_i32) : (i32) -> ()
    }
    %15 = affine.load %14[0] : memref<1xf32>
    affine.store %15, %arg5[symbol(%13) * symbol(%0) + symbol(%9)] : memref<?xf32, 2>
    return
  }
}
```

### Vector type example

```c
typedef int int2 __attribute__((__ext_vector_type__(2)));

void foo() {
  int2 v0;
  int v1[2];
  vload(v1[0], &v0);
  v0 += v0;
  vstore(v0, v1[0]);
}
```

Output IR as below
```mlir
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c0 = arith.constant 0 : index
    %0 = memref.alloca() : memref<1xvector<2xi32>>
    %1 = memref.alloca() : memref<2xi32>
    %2 = vector.load %1[%c0] : memref<2xi32>, vector<2xi32>
    affine.store %2, %0[0] : memref<1xvector<2xi32>>
    %3 = affine.load %0[0] : memref<1xvector<2xi32>>
    %4 = affine.load %0[0] : memref<1xvector<2xi32>>
    %5 = arith.addi %3, %4 : vector<2xi32>
    affine.store %5, %0[0] : memref<1xvector<2xi32>>
    %6 = affine.load %0[0] : memref<1xvector<2xi32>>
    vector.store %6, %1[%c0] : memref<2xi32>, vector<2xi32>
    return
  }
}
```

### Dma example
```c
void foo() {
  int src[2];
  int dst[2];
  int tag[1];
  dma_start(src[0], dst[0], tag[0], 2);
  dma_wait(tag[0], 2);
}
```

Output IR as below
```mlir
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c2 = arith.constant 2 : index
    %c0 = arith.constant 0 : index
    %0 = memref.alloca() : memref<2xi32>
    %1 = memref.alloca() : memref<2xi32>
    %2 = memref.alloca() : memref<1xi32>
    affine.dma_start %0[%c0], %1[%c0], %2[%c0], %c2 : memref<2xi32>, memref<2xi32>, memref<1xi32>
    affine.dma_wait %2[%c0], %c2 : memref<1xi32>
    return
  }
}
```

### Builtin Conv2D example

```c
void foo() {
  float lhs[3][4][5][6];
  float rhs[1][4][1][1];
  float output[3][1][5][6];
  conv_2d_nchw_fchw(lhs, rhs, output, 1, 1, 1, 1);
}
```

Output IR as below:
```mlir
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %0 = memref.alloca() : memref<3x4x5x6xf32>
    %1 = memref.alloca() : memref<1x4x1x1xf32>
    %2 = memref.alloca() : memref<3x1x5x6xf32>
    linalg.conv_2d_nchw_fchw {dilations = dense<1> : vector<2xi64>, strides = dense<1> : vector<2xi64>} ins(%0, %1 : memref<3x4x5x6xf32>, memref<1x4x1x1xf32>) outs(%2 : memref<3x1x5x6xf32>)
    return
  }
}
```

### Opencl example with linalg.matmul

```c
#define TS 10
#define CLK_LOCAL_MEM_FENCE 1

int get_local_id(int);
int get_group_id(int);
void barrier(int);

// Tiled and coalesced version
__kernel void GEMM(const int M, const int N, const int K,
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
    float acc[1][1];
    
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
        matmul(Bsub, Asub, acc);
 
        // Synchronise before loading the next tile
        barrier(CLK_LOCAL_MEM_FENCE);
    }
 
    // Store the final result in C
    C[globalCol*M + globalRow] = acc[0][0];
}
```

Output IR as below:
```mlir
#map = affine_map<()[s0] -> (s0 floordiv 10)>
module  {
  func private @get_local_id(i32) -> i32
  func private @get_group_id(i32) -> i32
  func private @barrier(i32)
  func @GEMM(%arg0: i32, %arg1: i32, %arg2: i32, %arg3: memref<?xf32, 2>, %arg4: memref<?xf32, 2>, %arg5: memref<?xf32, 2>, %arg6: memref<10x1xf32, 1>, %arg7: memref<1x10xf32, 1>) attributes {cl.kernel = true, llvm.emit_c_interface} {
    %c0_i32 = arith.constant 0 : i32
    %c10_i32 = arith.constant 10 : i32
    %c1_i32 = arith.constant 1 : i32
    %0 = arith.index_cast %arg0 : i32 to index
    %1 = arith.index_cast %arg2 : i32 to index
    %2 = call @get_local_id(%c0_i32) : (i32) -> i32
    %3 = arith.index_cast %2 : i32 to index
    %4 = call @get_local_id(%c1_i32) : (i32) -> i32
    %5 = arith.index_cast %4 : i32 to index
    %6 = call @get_group_id(%c0_i32) : (i32) -> i32
    %7 = arith.muli %6, %c10_i32 : i32
    %8 = arith.addi %7, %2 : i32
    %9 = arith.index_cast %8 : i32 to index
    %10 = call @get_group_id(%c1_i32) : (i32) -> i32
    %11 = arith.muli %10, %c10_i32 : i32
    %12 = arith.addi %11, %4 : i32
    %13 = arith.index_cast %12 : i32 to index
    %14 = memref.alloca() : memref<1x1xf32>
    affine.for %arg8 = 0 to #map()[%1] {
      %16 = affine.load %arg3[(%arg8 * 10 + symbol(%5)) * symbol(%0) + symbol(%9)] : memref<?xf32, 2>
      affine.store %16, %arg6[symbol(%5), symbol(%3)] : memref<10x1xf32, 1>
      %17 = affine.load %arg4[%arg8 * 10 + symbol(%3) + symbol(%13) * symbol(%1)] : memref<?xf32, 2>
      affine.store %17, %arg7[symbol(%5), symbol(%3)] : memref<1x10xf32, 1>
      call @barrier(%c1_i32) : (i32) -> ()
      linalg.matmul ins(%arg7, %arg6 : memref<1x10xf32, 1>, memref<10x1xf32, 1>) outs(%14 : memref<1x1xf32>)
      call @barrier(%c1_i32) : (i32) -> ()
    }
    %15 = affine.load %14[0, 0] : memref<1x1xf32>
    affine.store %15, %arg5[symbol(%13) * symbol(%0) + symbol(%9)] : memref<?xf32, 2>
    return
  }
}
```

### Launch example
```c
__kernel void foo(__global float *input) {}

void launch(const char *, int, int, char **);

void main() {
    char *inputs[1];
    inputs[0] = malloc(10);
    launch("foo", 1, 1, (char **)inputs);
    free(inputs[0]);
}
```

Output IR as below:
```mlir
module  {
  func private @launch(memref<?xi8>, i32, i32, memref<?xmemref<?xi8>>)
  func @foo(%arg0: memref<?xf32, 2>) attributes {cl.kernel = true, llvm.emit_c_interface} {
    return
  }
  func @main() attributes {llvm.emit_c_interface} {
    %c0 = arith.constant 0 : index
    %c1_i32 = arith.constant 1 : i32
    %cst = arith.constant dense<[102, 111, 111]> : vector<3xi8>
    %0 = memref.alloca() : memref<1xmemref<?xi8>>
    %1 = memref.alloc() : memref<10xi8>
    %2 = memref.cast %1 : memref<10xi8> to memref<?xi8>
    affine.store %2, %0[0] : memref<1xmemref<?xi8>>
    %3 = memref.alloca() : memref<3xi8>
    %4 = memref.cast %3 : memref<3xi8> to memref<?xi8>
    vector.store %cst, %3[%c0] : memref<3xi8>, vector<3xi8>
    %5 = memref.cast %0 : memref<1xmemref<?xi8>> to memref<?xmemref<?xi8>>
    call @launch(%4, %c1_i32, %c1_i32, %5) : (memref<?xi8>, i32, i32, memref<?xmemref<?xi8>>) -> ()
    %6 = affine.load %0[0] : memref<1xmemref<?xi8>>
    memref.dealloc %6 : memref<?xi8>
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