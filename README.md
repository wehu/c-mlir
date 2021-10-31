# c-mlir

Translator from c to MLIR

## Install

Install stack

```shell
curl https://get-ghcup.haskell.org -sSf | sh
```

Install [mlir-hs](https://github.com/google/mlir-hs)

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