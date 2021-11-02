# c-mlir

Translator from c to MLIR

Only a small subset of c is supported based on the current semantics of MLIR dialects.

* Scalar builtin types -> MLIR types
* Static sizes of array and operation -> MemRef dialect
* Arithmetic operations -> Arith dialect
* A limited version of flow control(for/if) -> Affine/SCF dialect

for `for`, if possiable, try to lower to affine.for, and if not, try to lower to scf.for, elsewise scf.while

## Install

Install stack

```shell
curl https://get-ghcup.haskell.org -sSf | sh
```

Install [mlir-hs](https://github.com/wehu/mlir-hs)

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