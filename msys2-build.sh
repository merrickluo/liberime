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

# rime-data
RIME_DATA_DIR="${MINGW_PREFIX}/share/rime-data"

# Archive
ARCHIVE_DIR="/d/liberime-archive"

# travis name
TRAVIS_ARCHIVE_NAME=""

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
        ${PACKAGE_PREFIX}-leveldb
        ${PACKAGE_PREFIX}-marisa
        ${PACKAGE_PREFIX}-opencc
        ${PACKAGE_PREFIX}-librime
        ${PACKAGE_PREFIX}-librime-data
        ${PACKAGE_PREFIX}-liberime
    )
    pacman -S --overwrite "*" --needed --noconfirm ${dep_packages[@]}
}

# 编译 liberime
function build_liberime() {
    install_deps

    echo ""
    echo "########## Build and install liberime ##########"

    ## 删除 liberime-core.dll, 不然重新编译的时候，可能会导致 ld.exe 报类似：
    ## "cannot open output file liberime-core.dll: Permission denied " 的错误。
    rm -f build/liberime-core.dll

    cmake -H. -Bbuild -G "MSYS Makefiles" -DCMAKE_INSTALL_PREFIX="${INSTALL_PREFIX}"
    cmake --build build --config Release
    
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
    
    ## 复制 liberime-core.dll 和它的所有依赖
    mkdir -p ${temp_bin_dir} 
    cp build/liberime-core.dll ${temp_bin_dir}
    cp "${INSTALL_PREFIX}/bin/librime.dll" ${temp_bin_dir}
    copy_all_dll "${temp_bin_dir}/librime.dll" ${temp_bin_dir} "mingw32/bin\\|mingw32/lib\\|mingw64/bin\\|mingw64/lib\\|usr/bin\\|usr/lib"

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

    -t, --travis=FILENAME       travis 打包名

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

    build_liberime
    archive_liberime

}

# 选项
ARGS=$(getopt -o ha:t: --long help,archive:,travis: -n "$0" -- "$@")


if [[ $? != 0 ]]; then
    echo "错误的选项！"
    display_usage
    exit 1
fi

eval set -- "${ARGS}"

main "$@"
