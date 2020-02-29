#!/bin/bash
#
#  msys2 的构建脚本
#

set -e

#######################################
# 复制所有dll依赖到指定目录
# Arguments:
# binary_file: 二进制文件
# target_dir: 目标目录
# dll_path_regex: 匹配正则的dll路径才会复制
#######################################

function copy_all_dll() {
    local binary_file=$1
    local target_dir=$2
    local dll_path_regex=$3
    if [[ -z ${dll_path_regex} ]]; then
        dll_path_regex=".*"
    fi

    OLD_IFS=$IFS
    IFS=$'\n'

    for dll_file in $(ldd ${binary_file} | grep "${dll_path_regex}" | cut -d " " -f 3); do
        if [[ -z ${dll_file} ]]; then
            continue
        fi
        
        if [[ ! -f "${target_dir}/${dll_file##*/}" ]]; then
            cp ${dll_file} ${target_dir}
            copy_all_dll ${dll_file} ${target_dir} ${dll_path_regex}
        fi
    done

    IFS=$OLD_IFS
}


# 安装依赖
function install_deps() {
    local dep_packages=(
        base-devel
        zip
        git
        mingw-w64-x86_64-cmake
        mingw-w64-x86_64-gcc
        mingw-w64-x86_64-boost
        mingw-w64-x86_64-glog
        mingw-w64-x86_64-yaml-cpp
    )
    pacman -S --needed --noconfirm ${dep_packages[@]}
}

# 编译 leveldb
function build_leveldb() {
    if [[ ! -d "leveldb" ]]; then
        git clone https://github.com/google/leveldb.git
    fi
    pushd leveldb
    cmake -H. -Bbuild -G "MSYS Makefiles" -DCMAKE_INSTALL_PREFIX="/usr" -DLEVELDB_BUILD_TESTS=OFF -DLEVELDB_BUILD_BENCHMARKS=OFF
    cmake --build build --config Release --target install
    popd
}

# 编译 marisa
function build_marisa() {
    if [[ ! -d "marisa-trie" ]]; then
        git clone https://github.com/s-yata/marisa-trie.git
    fi
    pushd marisa-trie
    autoreconf -i
    ./configure --enable-native-code --prefix="/usr"
    make && make install
    popd
}

# 编译 OpenCC
function build_opencc() {
    if [[ ! -d "OpenCC" ]]; then
        git clone https://github.com/BYVoid/OpenCC.git
    fi
    pushd OpenCC
    cmake -H. -Bbuild -G "MSYS Makefiles" -DCMAKE_INSTALL_PREFIX="/usr" -DENABLE_GTEST=OFF
    cmake --build build --config Release --target install
    popd
}

# 编译 librime
function build_librime() {
    if [[ ! -d "librime" ]]; then
        git clone https://github.com/rime/librime.git   
    fi
    pushd librime
    cmake -H. -Bbuild -G "MSYS Makefiles" -DCMAKE_INSTALL_PREFIX="/usr" -DBUILD_DATA=ON -DBUILD_TEST=OFF -DBOOST_USE_CXX11=ON -DCMAKE_CXX_STANDARD_LIBRARIES="-lbcrypt"
    cmake --build build --config Release --target install
    popd
}

# 编译 liberime
function build_liberime() {
    install_deps
    if [[ ! -d "third_party_build" ]]; then
        mkdir third_party_build
    fi
    # 编译第三方依赖
    pushd third_party_build
    build_leveldb
    build_marisa
    build_opencc
    build_librime
    popd
    cmake -H. -Bbuild -G "MSYS Makefiles" -DCMAKE_INSTALL_PREFIX="/usr"
    cmake --build build --config Release
  
    # 复制非系统依赖
    cp /usr/lib/librime.dll build
    copy_all_dll "build/librime.dll" build "mingw64/bin\\|mingw64/lib\\|usr/bin\\|usr/lib"

    ## 复制 opencc 词典
    mkdir -p build/data
    cp -r /usr/share/opencc "build/data/"

    ## 复制 schema
    echo "fetch schema"
    echo "curl -fsSL https://git.io/rime-install | bash"
    curl -fsSL https://git.io/rime-install | bash
    if [[ -d "plum" ]]; then
        cp plum/package/rime/*/*.yaml "build/data/"
        rm -rf plum
    else
        echo "can not download schema, skip..."
    fi
   
}

# 打包liberime
function package_liberime() {
    local package_dir= "liberime-$(git rev-parse --short HEAD)"
    local data_dir="${package_dir}/build"
    if [[ -d "${package_dir}" ]]; then
        rm -rf "${package_dir}"
    fi
    mkdir -p ${data_dir} 
    cp liberime-config.el ${package_dir}
    cp README.org ${package_dir}
    cp build/liberime.dll ${data_dir}
    cp -r build/data ${data_dir}
    cp /usr/lib/librime.dll ${data_dir}
    ## 复制 librime.dll 的所有依赖
    copy_all_dll "${package_dir}/librime.dll" ${data_dir} "mingw64/bin\\|mingw64/lib\\|usr/bin\\|usr/lib"
    
    ## 压缩
    zip  -r "${package_dir}.zip" "${package_dir}/*"
    rm -rf ${package_dir}
    echo "package liberime finished: $(PWD)/${package_dir}.zip"
}

function display_usage() {
    cat <<EOF
用法: ./msys2_build.sh [--package]

      使用 msys2 构建 liberime.dll

选项:

    --package   打包 dll 依赖, opencc 词典, 默认的 scheme 到一个 zip 文件

EOF
}

function main() {
    if [ "$1" == "-h" -o "$1" == "--help" ]; then
        display_usage
        exit 0
    fi
    build_liberime
    if [ "$1" == "-p" -o "$1" == "--package" ]; then
        package_liberime
    fi
    exit 0
}

main "$@"
