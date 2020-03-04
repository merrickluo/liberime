#!/bin/bash
#
#  msys2 的构建脚本
#

set -e

# 架构
ARCH="${MSYSTEM_CARCH}"

# 包前缀
PACKAGE_PREFIX="${MINGW_PACKAGE_PREFIX}"

# 安装位置
INSTALL_PREFIX="${MINGW_PREFIX}"

# git 协议
GIT_PROTOCOL="https"
GIT_PROTOCOL_URL="https://github.com/"

# job 数量
JOB_NUMBER=1

# archive 名字
ARCHIVE_NAME=""

# 激活 rime log
RIME_ENABLE_LOG="OFF"

INSTALL_SCHEMA=""


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
        ${PACKAGE_PREFIX}-cmake
        ${PACKAGE_PREFIX}-gcc
        ${PACKAGE_PREFIX}-boost
        ${PACKAGE_PREFIX}-glog
        ${PACKAGE_PREFIX}-yaml-cpp
    )
    pacman -S --needed --noconfirm ${dep_packages[@]}
}

# 编译 leveldb
function build_leveldb() {
    if [[ ! -d "leveldb" ]]; then
        git clone --depth 1 "${GIT_PROTOCOL_URL}google/leveldb.git"
    fi
    pushd leveldb
    cmake -H. -Bbuild -G "MSYS Makefiles" -DCMAKE_INSTALL_PREFIX="${INSTALL_PREFIX}" -DLEVELDB_BUILD_TESTS=OFF -DLEVELDB_BUILD_BENCHMARKS=OFF
    cmake --build build --config Release --target install -j ${JOB_NUMBER}
    popd
}

# 编译 marisa
function build_marisa() {
    if [[ ! -d "marisa-trie" ]]; then
        git clone --depth 1 "${GIT_PROTOCOL_URL}s-yata/marisa-trie.git"
    fi
    pushd marisa-trie
    autoreconf -i
    if [[ "${ARCH}" == "x86_64" ]]; then
        ./configure --enable-native-code --disable-shared --prefix="${INSTALL_PREFIX}"
    else
        ./configure  --prefix="${INSTALL_PREFIX}"
    fi
    make -j ${JOB_NUMBER} && make install
    popd
}

# 编译 OpenCC
function build_opencc() {
    if [[ ! -d "OpenCC" ]]; then
        git clone --depth 1 "${GIT_PROTOCOL_URL}BYVoid/OpenCC.git"
    fi
    pushd OpenCC
    cmake -H. -Bbuild -G "MSYS Makefiles" -DCMAKE_INSTALL_PREFIX="${INSTALL_PREFIX}" -DENABLE_GTEST=OFF -DBUILD_SHARED_LIBS=OFF
    cmake --build build --config Release --target install
    popd
}

# 编译 librime
function build_librime() {
    if [[ ! -d "librime" ]]; then
        git clone  --depth 1 "${GIT_PROTOCOL_URL}rime/librime.git"
    fi
    pushd librime
    cmake -H. -Bbuild -G "MSYS Makefiles" -DCMAKE_INSTALL_PREFIX="${INSTALL_PREFIX}" -DBUILD_TEST=OFF -DBOOST_USE_CXX11=ON -DBUILD_STATIC=ON -DENABLE_LOGGING="${RIME_ENABLE_LOG}" -DCMAKE_CXX_STANDARD_LIBRARIES="-lbcrypt"
    cmake --build build --config Release --target install -j ${JOB_NUMBER}
    # liberime 通过 PATH 环境变量寻找 librime.dll, 将 librime.dll 复制到 bin, 就
    # 不需要用户自己设置 PATH 环境变量了。
    cp "${INSTALL_PREFIX}/lib/librime.dll" "${INSTALL_PREFIX}/bin/"
    popd
}

# 用 plum 安裝 schema
function install_schema() {
    local install_dir="$1"
    if [[ ! -d "plum" ]]; then
        git clone --depth 1 "${GIT_PROTOCOL_URL}rime/plum.git"
    fi
    pushd plum
    rime_dir="${install_dir}" bash rime-install
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
    cmake -H. -Bbuild -G "MSYS Makefiles" -DCMAKE_INSTALL_PREFIX="${INSTALL_PREFIX}"
    cmake --build build --config Release -j ${JOB_NUMBER}
    
    # 复制非系统依赖
    cp "${INSTALL_PREFIX}/lib/librime.dll" build
    strip build/librime.dll
    copy_all_dll "build/librime.dll" build "mingw32/bin\\|mingw32/lib\\|mingw64/bin\\|mingw64/lib\\|usr/bin\\|usr/lib"

    ## 复制 opencc 词典
    mkdir -p build/data
    cp -r "${INSTALL_PREFIX}/share/opencc" "build/data/"

    # 安裝 schema
    cp -r "third_party_build/librime/data/minimal" "build/data/"
    
    echo ""
    echo "Build Finished!!!"
}

# 打包liberime
function archive_liberime() {
    local archive_dir="${ARCHIVE_NAME}"
    local data_dir="${archive_dir}/build"
    if [[ -d "${archive_dir}" ]]; then
        rm -rf "${archive_dir}"
    fi
    mkdir -p ${data_dir} 
    cp liberime.el ${package_dir}
    cp liberime-config.el ${package_dir}
    cp README.org ${package_dir}
    cp build/liberime-core.dll ${data_dir}
    cp -r build/data ${data_dir}
    cp "${INSTALL_PREFIX}/lib/librime.dll" ${data_dir}
    ## 复制 librime.dll 的所有依赖
    copy_all_dll "${archive_dir}/librime.dll" ${data_dir} "mingw32/bin\\|mingw32/lib\\|mingw64/bin\\|mingw64/lib\\|usr/bin\\|usr/lib"
    
    ## 压缩
    if [[ -f "${archive_dir}.zip" ]]; then
        rm -rf "${archive_dir}.zip"
    fi

    zip  -r "${archive_dir}.zip" "${archive_dir}/*"
    rm -rf ${archive_dir}
    echo "archive liberime finished: ${archive_dir}.zip"
}

function display_usage() {
    cat <<HELP
用法: ./msys2_build.sh [选项]

      使用 msys2 构建 liberime-core.dll

选项:

    -a, --archive=FILENAME      打包 dll 依赖, opencc 词典, 默认的 scheme 到一个 FILENAME.zip 文件

    -p, --protocol=PROTOCOL     git clone 时用到的协议，https 或者 ssh 

    -j, --job=JOB_NUMBER        编译时的 job 数

    -l, --log                   激活 rime 的 log

    -s --schema                 安裝所有 schema

    -h, --help                  查看帮助

HELP
}

function main() {
    while true
    do
        case "$1" in
            -h|--help)
                display_usage;
                exit 0
                ;;
            -l|--log)
                RIME_ENABLE_LOG="ON"
                shift
                ;;
            -s|--schema)
                INSTALL_SCHEMA="TRUE"
                shift
                ;;
            -a|--archive)
                ARCHIVE_NAME="$2";
                shift 2
                ;;
            -p|--protocol)
                GIT_PROTOCOL="$2";
                shift 2
                ;;
            -j|--job)
                JOB_NUMBER="$2";
                shift 2
                ;;
            --)
                shift
                break
                ;;
            *)
                echo "错误的选项！"
                exit 1
        esac
    done

    if [[ "${GIT_PROTOCOL}" == "ssh" ]]; then
        GIT_PROTOCOL_URL="git@github.com:"
    elif [[ "${GIT_PROTOCOL}" == "https" ]]; then
        GIT_PROTOCOL_URL="https://github.com/"
    else
        echo "错误的协议：使用 https 或 ssh"
        exit 1
    fi

    echo "start build liberime..."
    build_liberime

    if [[ -n "${ARCHIVE_NAME}" ]]; then
        echo "start archive liberime..."
        archive_liberime
    fi

    if [[ -n "${INSTALL_SCHEMA}" ]]; then
        echo "install schema to ./build/data"
        install_schema "build/data"
    fi

}

# 选项
ARGS=$(getopt -o hlsa:p:j: --long help,log,schema,archive:,protocol:,job: -n "$0" -- "$@")


if [[ $? != 0 ]]; then
    echo "错误的选项！"
    display_usage
    exit 1
fi

eval set -- "${ARGS}"

main "$@"
