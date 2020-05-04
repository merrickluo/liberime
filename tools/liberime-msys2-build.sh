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

# archive name
ARCHIVE_NAME=""

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
    if [[ -n "${ARCHIVE_NAME}" ]]; then
        zip_file="${PWD}/${ARCHIVE_NAME}.zip"
    else
        zip_file="${PWD}/liberime-windows-${ARCH}.zip"
    fi

    local temp_dir="temp"

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

    cd ${temp_dir}
    zip -r "${zip_file}" . > /dev/null
    
    cd ..
    rm -rf ${temp_dir}
    
    echo "Archive liberime to file: ${zip_file}"
}

function display_usage() {
    cat <<HELP
用法: ./liberime-msys2-build.sh [选项]

      使用 msys2 构建 liberime-core.dll

选项:

    -a, --archive=FILENAME      打包名称

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
            -a|--archive)
                ARCHIVE_NAME="$2";
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
ARGS=$(getopt -o ha:a: --long help,archive:,archive: -n "$0" -- "$@")


if [[ $? != 0 ]]; then
    echo "错误的选项！"
    display_usage
    exit 1
fi

eval set -- "${ARGS}"

main "$@"
