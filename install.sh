#!/bin/bash

function getLlvmClone() {
    local llvm_dir="~/llvm-project"
    local llvm_rep="git@github.com:llvm/llvm-project.git"
    read -p "Where do you want to clone llvm? [$llvm_dir] " wished_dir
    if [[ ! -z "$wished_dir" ]]
    then
        llvm_dir="$wished_dir"
    fi
    llvm_dir=${llvm_dir/"~"/"${HOME}"}
    echo "llvm will be cloned to the directory: ${llvm_dir}"
    echo "This might take some time to complete..."
    git clone --depth 1 "$llvm_rep" "$llvm_dir"
    result="$llvm_dir"
}

function buildLlvm() {
    local cmake_flags=("-DCMAKE_BUILD_TYPE=Release" "-DLLVM_ENABLE_BINDINGS=ON")
    cmake -G Ninja ${cmake_flags[*]} ../llvm
    ninja
}

while [[ "$1" =~ ^- && ! "$1" == "--" ]]; do case $1 in
    -c | --llvm-clone )
    shift; llvm_clone=$1
    ;;
    --skip-opam )
    skip_opam=1
    ;;
    --skip-build )
    skip_build=1
    ;;
    --skip-install )
    skip_install=1
    ;;
    --delete-libs )
    delete_libs=1
    ;;
esac; shift; done

echo "Checking for the needed commands..."

commands_needed=("opam" "cmake" "ninja")

for cmd in ${commands_needed[*]}; do
    if ! command -v "$cmd" &> /dev/null
    then
        echo "You need to have ${cmd} installed"
        exit 1
    else
        echo "'${cmd}': found"
    fi
done

if [[ ! $skip_opam ]]
then
    echo "Installing opam dependencies: ctypes ctypes-foreign"
    opam install ctypes ctypes-foreign
fi

if [[ ! $llvm_clone ]]
then
    read -p "Do you already have a clone of llvm? [Y/n] " -n 1 -r
    echo ""
    if [[ ! $REPLY =~ ^[Yy]$ ]]
    then
        getLlvmClone
    else
        read -p "Where is your clone of llvm? [~/llvm-project] " result
        if [[ -z "$result" ]]
        then
            result="${HOME}/llvm-project"
        else
            result=${result/"~"/"${HOME}"}
        fi
    fi
else
    result=${llvm_clone/"~"/"${HOME}"}
fi

base_dir=$(pwd)
should_build=true

cd "${result}"

if [[ ! $skip_build ]]
then
    echo "Moving to the llvm clone directory: $result"
    if [ -d "build" ]
    then
        echo "A 'build' directory has been detected."
        read -p "Do you wish to build again llvm? [Y/n](if not we assume that you also have build the ocaml binding) " -n 1 -r
        if [[ ! $REPLY =~ ^[Yy]$ ]]
        then
            should_build=false
        else
            rm -rf build
            mkdir build
        fi
    else
        mkdir build
    fi

    cd build

    if $should_build
    then
            buildLlvm
    fi
else
    cd build
fi

if [[ ! $skip_install ]]
then
    echo "Installing the libraries from llvm"
    sudo ninja bindings/ocaml/install
fi

opam_lib=$(opam var lib)

echo "Moving to ${opam_lib}"

llvm_files=("all_backends" "analysis" "bitreader" "bitwriter" "debuginfo" "executionengine" "ipo" "irreader" "linker" "passmgr_builder" "scalar_opts" "target" "transform_utils" "vectorize")

llvm_archs=("AArch64" "AMDGPU" "ARM" "AVR" "BPF" "Hexagon" "Lanai" "MSP430" "Mips" "NVPTX" "PowerPC" "RISCV" "Sparc" "SystemZ" "WebAssembly" "X86" "XCore")

echo "Moving llvm libs to the ${opam_lib}/llvm folder..."

cd "${opam_lib}"

mkdir llvm

sudo mv -v ocaml/META.llvm llvm/META

sudo mv -v ocaml/llvm/llvm.* llvm/

sudo mv -v ocaml/llvm/libllvm.a llvm/

cd llvm

sudo sed -i "" '/directory/d' META

cd ..

for file in ${llvm_files[*]}
do
    sudo mv -v ocaml/llvm/llvm_"${file}".* ocaml/llvm/libllvm_"${file}".a llvm/
done

for arch in ${llvm_archs[*]}
do
    arch_dir="llvm_${arch}"
    meta_name="META.${arch_dir}"
    mkdir "${arch_dir}"
    sudo mv -v ocaml/"${meta_name}" "${arch_dir}"/META
    sudo mv -v ocaml/llvm/"${arch_dir}".* "${arch_dir}"/
    sudo mv -v ocaml/llvm/lib"${arch_dir}".a "${arch_dir}"/
done

rm -rf ocaml/llvm
