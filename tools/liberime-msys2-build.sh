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

function install_all_dll() {
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
            install -Dm644 ${dll_file} -t ${target_dir}
            install_all_dll ${dll_file} ${target_dir} ${dll_path_regex}
        fi
    done

    IFS=$OLD_IFS
}

# 编译 liberime
function build_liberime() {

    echo ""
    echo "########## Install build dependences ##########"
    local dep_packages=(
        base-devel zip
        ${PACKAGE_PREFIX}-gcc
        ${PACKAGE_PREFIX}-librime
        ${PACKAGE_PREFIX}-librime-data
        ${PACKAGE_PREFIX}-rime-wubi
        ${PACKAGE_PREFIX}-rime-double-pinyin
        ${PACKAGE_PREFIX}-rime-emoji
    )

    ## 不安装 liberime, 因为可能和自己编译的 liberime 产生版本冲突。
    if pacman -Qs liberime; then
        pacman -R --noconfirm ${PACKAGE_PREFIX}-liberime
    fi

    pacman -Sy --overwrite "*" --needed --noconfirm ${dep_packages[@]}

    echo ""
    echo "########## Build and install liberime ##########"

    make clean
    make
    
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

    if [[ -d "${ARCHIVE_DIR}" ]]; then
        rm -rf "${ARCHIVE_DIR}"
    fi

    install -Dm644 tools/README-archive.txt ${temp_dir}/README.txt

    install -Dm644 liberime.el -t ${temp_dir}/share/emacs/site-lisp/liberime/
    install -Dm644 src/liberime-core.dll -t ${temp_dir}/share/emacs/site-lisp/liberime/

    install -Dm644 ${MINGW_PREFIX}/bin/librime.dll -t ${temp_dir}/bin/
    install_all_dll ${MINGW_PREFIX}/bin/librime.dll    \
                    ${temp_dir}/bin/                   \
                    "mingw32/bin\\|mingw32/lib\\|mingw64/bin\\|mingw64/lib\\|usr/bin\\|usr/lib"

    install -Dm644 ${INSTALL_PREFIX}/lib/librime* -t ${temp_dir}/lib/
    install -Dm644 ${INSTALL_PREFIX}/include/rime* -t ${temp_dir}/include/

    install -Dm644 ${INSTALL_PREFIX}/share/opencc/* -t ${temp_dir}/share/rime-data/opencc/

    install -Dm644 ${RIME_DATA_DIR}/*.* -t ${temp_dir}/share/rime-data/

    ## 有些 rime schema 会自带 opencc 文件，保存在 rime-data/opencc 目录下面。
    ## 比如： rime-emoji
    install -Dm644 ${RIME_DATA_DIR}/opencc/* -t ${temp_dir}/share/rime-data/opencc/

    ## 压缩
    if [[ -f "${zip_file}" ]]; then
        rm -rf "${zip_file}"
    fi

    cd ${temp_dir}
    zip -r "${zip_file}" . > /dev/null
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
用法: ./liberime-msys2-build.sh [选项]

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
