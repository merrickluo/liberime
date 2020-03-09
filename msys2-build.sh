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
GIT_PROTOCOL="ssh"
GIT_PROTOCOL_URL="https://github.com/"

# 重新编译所有
REBUILD_ALL=""

# job 数量
JOB_NUMBER=1

# 激活 rime log
RIME_ENABLE_LOG="OFF"

# rime-data
RIME_DATA_DIR="${MINGW_PREFIX}/share/rime-data"

# Archive
ARCHIVE_DIR="/d/liberime-archive"

# travis name
TRAVIS_ARCHIVE_NAME=""

function repeatcmd() {
    set +e
    count=0
    while [ 0 -eq 0 ]
    do
        echo "Run $@ ..."
        $@
        if [ $? -eq 0 ]; then
            break;
        else
            count=$[${count}+1]
            if [ ${count} -eq 50 ]; then
                echo 'Timeout and exit.'
                exit 1;
            fi
            echo "Retry ..."
            sleep 3
        fi
    done
    set -e
}

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
    echo ""
    echo "########## Install build dependences ##########"
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
    echo ""
    echo "########## Build and install leveldb ##########"
    if [[ ! -d "leveldb" ]]; then
        repeatcmd git clone --depth 1 "${GIT_PROTOCOL_URL}google/leveldb.git"
    fi
    pushd leveldb
    cmake -H. -Bbuild -G "MSYS Makefiles" -DCMAKE_INSTALL_PREFIX="${INSTALL_PREFIX}" -DLEVELDB_BUILD_TESTS=OFF -DLEVELDB_BUILD_BENCHMARKS=OFF
    cmake --build build --config Release --target install -j ${JOB_NUMBER}
    popd
}

# 编译 marisa
function build_marisa() {
    echo ""
    echo "########## Build and install marisa-tries ##########"
    if [[ ! -d "marisa-trie" ]]; then
        repeatcmd git clone --depth 1 "${GIT_PROTOCOL_URL}s-yata/marisa-trie.git"
    fi
    pushd marisa-trie
    autoreconf -i
    if [[ "${ARCH}" == "x86_64" ]]; then
        ./configure --enable-native-code --disable-shared --prefix="${INSTALL_PREFIX}"
    else
        ./configure  --disable-shared --prefix="${INSTALL_PREFIX}"
    fi
    make -j ${JOB_NUMBER} && make install
    popd
}

# 编译 OpenCC
function build_opencc() {
    echo ""
    echo "########## Build and install opencc ##########"
    if [[ ! -d "OpenCC" ]]; then
        repeatcmd git clone --depth 1 "${GIT_PROTOCOL_URL}BYVoid/OpenCC.git"
    fi
    pushd OpenCC
    cmake -H. -Bbuild -G "MSYS Makefiles" -DCMAKE_INSTALL_PREFIX="${INSTALL_PREFIX}" -DENABLE_GTEST=OFF -DBUILD_SHARED_LIBS=OFF
    cmake --build build --config Release --target install
    popd
}

# 编译 librime
function build_librime() {
    echo ""
    echo "########## Build and install librime ##########"
    if [[ ! -d "librime" ]]; then
        repeatcmd git clone  --depth 1 "${GIT_PROTOCOL_URL}rime/librime.git"
    fi
    pushd librime
    cmake -H. -Bbuild -G "MSYS Makefiles" -DCMAKE_INSTALL_PREFIX="${INSTALL_PREFIX}" -DBUILD_TEST=OFF -DBOOST_USE_CXX11=ON -DBUILD_STATIC=ON -DENABLE_LOGGING="${RIME_ENABLE_LOG}" -DCMAKE_CXX_STANDARD_LIBRARIES="-lbcrypt"
    cmake --build build --config Release --target install -j ${JOB_NUMBER}
    # liberime 通过 PATH 环境变量寻找 librime.dll, 将 librime.dll 复制到 bin, 就
    # 不需要用户自己设置 PATH 环境变量了。
    cp -f "${INSTALL_PREFIX}/lib/librime.dll" "${INSTALL_PREFIX}/bin/"
    popd
}

# 安装 rime-data
function install_rime_data() {
    echo ""
    echo "########## Install rime data from plum.git ##########"

    rm -rf $RIME_DATA_DIR
    mkdir -p "$RIME_DATA_DIR"/opencc
    cp -r "${INSTALL_PREFIX}/share/opencc" "$RIME_DATA_DIR"/

    if [[ ! -d "plum" ]]; then
        repeatcmd git clone --depth 1 "${GIT_PROTOCOL_URL}rime/plum.git"
    fi
    pushd plum
    export rime_dir="$RIME_DATA_DIR"
    repeatcmd bash rime-install
    popd
}

# 编译 liberime
function build_liberime() {
    if [[ ! -n "${REBUILD_ALL}" ]] && [[ -f "${INSTALL_PREFIX}/lib/librime.dll" ]]; then
        echo "Note: ONLY rebuild liberime-core, Use --rebuildall to build all dependences."
    else
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
        install_rime_data
        popd
    fi

    echo ""
    echo "########## Build and install liberime ##########"

    ## 删除 liberime-core.dll, 不然重新编译的时候，可能会导致 ld.exe 报类似：
    ## "cannot open output file liberime-core.dll: Permission denied " 的错误。
    rm -f build/liberime-core.dll

    cmake -H. -Bbuild -G "MSYS Makefiles" -DCMAKE_INSTALL_PREFIX="${INSTALL_PREFIX}"
    cmake --build build --config Release -j ${JOB_NUMBER}
    
    echo ""
    echo "Build liberime Finished!!!"
}

# 打包liberime
function archive_liberime() {
    echo ""
    echo "########## Archive liberime ##########"

    local zip_file
    if [[ -n "${TRAVIS_ARCHIVE_NAME}" ]]; then
        ARCHIVE_DIR="${PWD}/${TRAVIS_ARCHIVE_NAME}"
        zip_file="${ARCHIVE_DIR}/${TRAVIS_ARCHIVE_NAME}.zip"
    else
        zip_file="${ARCHIVE_DIR}/liberime-archive.zip"
    fi

    local temp_dir="${ARCHIVE_DIR}/temp"
    local temp_bin_dir="${ARCHIVE_DIR}/temp/bin"
    local temp_site_lisp_dir="${ARCHIVE_DIR}/temp/share/emacs/site-lisp"
    local temp_rime_data_dir="${ARCHIVE_DIR}/temp/share/rime-data"
    if [[ -d "${ARCHIVE_DIR}" ]]; then
        rm -rf "${ARCHIVE_DIR}"
    fi

    ## 复制 el 文件
    mkdir -p ${temp_site_lisp_dir}
    cp liberime.el ${temp_site_lisp_dir}
    cp liberime-config.el ${temp_site_lisp_dir}

    ## 复制 liberime-core.dll 和它的所有依赖
    mkdir -p ${temp_bin_dir} 
    cp build/liberime-core.dll ${temp_bin_dir}
    cp "${INSTALL_PREFIX}/lib/librime.dll" ${temp_bin_dir}
    copy_all_dll "${temp_bin_dir}/librime.dll" ${temp_bin_dir} "mingw32/bin\\|mingw32/lib\\|mingw64/bin\\|mingw64/lib\\|usr/bin\\|usr/lib"

    ## strip all dll
    strip ${temp_bin_dir}/*

    ## 复制 rime-data
    mkdir -p ${temp_rime_data_dir}
    cp -r "${RIME_DATA_DIR}"/* ${temp_rime_data_dir}
    
    ## 复制 README.txt
    cp README-archive.txt ${temp_dir}/README.txt

    ## 压缩
    if [[ -f "${zip_file}" ]]; then
        rm -rf "${zip_file}"
    fi

    cd ${temp_dir}
    zip -r "${zip_file}" ./ > /dev/null
    cd ..
    rm -rf ${temp_dir}
    echo "Archive liberime to file: ${zip_file}"

    if [[ -n "${TRAVIS_ARCHIVE_NAME}" ]]; then
        mv *.zip ..
        rm -rf ${ARCHIVE_DIR}
    fi

}

function display_usage() {
    cat <<HELP
用法: ./msys2_build.sh [选项]

      使用 msys2 构建 liberime-core.dll

选项:

    -r, --rebuildall            是否重新编译所有库

    -t, --travis=FILENAME       travis 打包名

    -p, --protocol=PROTOCOL     git clone 时用到的协议，https 或者 ssh 

    -j, --job=JOB_NUMBER        编译时的 job 数

    -l, --log                   激活 rime 的 log

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
            -r|--rebuildall)
                REBUILD_ALL="TRUE"
                shift
                ;;
            -l|--log)
                RIME_ENABLE_LOG="ON"
                shift
                ;;
            -p|--protocol)
                GIT_PROTOCOL="$2";
                shift 2
                ;;
            -j|--job)
                JOB_NUMBER="$2";
                shift 2
                ;;
            -t|--travis)
                TRAVIS_ARCHIVE_NAME="$2";
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

    build_liberime
    archive_liberime

}

# 选项
ARGS=$(getopt -o rhla:p:j:t: --long rebuildall,help,log,archive:,protocol:,job:,travis: -n "$0" -- "$@")


if [[ $? != 0 ]]; then
    echo "错误的选项！"
    display_usage
    exit 1
fi

eval set -- "${ARGS}"

main "$@"
