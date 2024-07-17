#!/usr/bin/env bash

# exit script if any command fails
set -e
set -o pipefail

# Check that git version is at least 1.7.8
MYGIT=$(git --version)
MYGIT=${MYGIT#'git version '} # Strip prefix
case ${MYGIT} in
[1-9]*) ;;
*) echo 'warning: unknown git version' ;;
esac
MINGIT="1.7.8"
if [ "$MINGIT" != "$(echo -e "$MINGIT\n$MYGIT" | sort -V | head -n1)" ]; then
  echo "This script requires git version $MINGIT or greater. Exiting."
  false
fi

# Make new directory for toolchain in home
cd $HOME
mkdir rvv09
cd rvv09

# Setup Spike simulator
git clone https://github.com/riscv/riscv-isa-sim.git
cd riscv-isa-sim
git checkout 759f4eba829d299eb34cd1568d3f4694e0d198cb
mkdir build
cd build
../configure --prefix=$HOME/rvv09/rvv09-tools/spike
make
make install
cd ../..

# Setup RISC-V GNU toolchain
git clone https://github.com/riscv/riscv-gnu-toolchain.git --branch rvv-0.9.x --single-branch riscv-gnu-toolchain_rvv-0.9.x
cd riscv-gnu-toolchain_rvv-0.9.x
git submodule update --init --recursive riscv-binutils riscv-gcc riscv-glibc riscv-dejagnu riscv-newlib riscv-gdb
mkdir build
cd build
../configure --prefix=$HOME/rvv09/rvv09-tools/gnu --enable-multilib
make
make install
cd ../..

# Setup Proxy Kernel
git clone https://github.com/riscv/riscv-pk.git
cd riscv-pk
mkdir build
cd build
PATH=$HOME/rvv09/rvv09-tools/gnu/bin:$PATH ../configure --prefix=$HOME/rvv09/rvv09-tools/pk --host=riscv64-unknown-elf
PATH=$HOME/rvv09/rvv09-tools/gnu/bin:$PATH make
PATH=$HOME/rvv09/rvv09-tools/gnu/bin:$PATH make install
cd ../..

# Set path and RISC-V environment variables in .bashrc
echo 'export RISCV=$HOME/rvv09/rvv09-tools/gnu' >> ~/.bashrc
echo 'export PATH=$HOME/rvv09/rvv09-tools/gnu/bin:$HOME/rvv09/rvv09-tools/gnu/riscv64-unknown-elf/bin:$HOME/rvv09/rvv09-tools/spike/bin:$HOME/rvv09/rvv09-tools/pk/riscv64-unknown-elf/bin:$PATH' >> ~/.bashrc
