#!/bin/bash
# Install the six engines at their releases of 2026-07-01 and name them as
# conform-test looks them up, in PREFIX/home/.jsvu/bin; pass
# -Duser.home=PREFIX/home to use them. Each engine takes the first way that
# works: jsvu at the pinned version, the release archive (Linux x86-64), and,
# for JavaScriptCore, which no Linux archive keeps at 316211, a source build.
# PREFIX/engines.txt records which way each engine came, since two builds of
# one release can differ (the Linux and macOS XS 8.2.3 disagree on Set methods).
#
#   experiment/engine-install.sh [PREFIX]    # default ~/frozen
set -euo pipefail
export LC_ALL=C.UTF-8

PREFIX=${1:-$HOME/frozen}
HOME_DIR=$PREFIX/home
BIN=$HOME_DIR/.jsvu/bin
ENG=$PREFIX/engines
LOG=$PREFIX/engines.txt
mkdir -p "$BIN" "$ENG"
: > "$LOG"

case "$(uname -s)-$(uname -m)" in
  Linux-x86_64) OS=linux64 ;;
  Darwin-arm64) OS=mac64arm ;;
  Darwin-x86_64) OS=mac64 ;;
  *) echo "unsupported platform $(uname -s)-$(uname -m)" >&2; exit 1 ;;
esac

WEBKIT=b8b79998fe36292b84c8a3d4a00582e844befa4e # 316211@main

have() { [ -x "$BIN/$1" ] && echo 'print(1+1)' > "$ENG/probe.js" && [ "$("$BIN/$1" "$ENG/probe.js" 2>/dev/null)" = 2 ]; }
note() { printf '%-8s %s\n' "$1" "$2" >> "$LOG"; }
wrap() { printf '#!/bin/sh\n%s "$@"\n' "$2" > "$BIN/$1"; chmod +x "$BIN/$1"; }
fetch() { [ -e "$ENG/$2" ] || curl -fsSL -o "$ENG/$2" "$1"; }

# jsvu itself is pinned too; it writes into $HOME/.jsvu, so HOME points at PREFIX
via_jsvu() { # name engine version jsvu-binary
  command -v npx > /dev/null || return 1
  HOME=$HOME_DIR npx -y jsvu@3.0.5 --os="$OS" "$2@$3" > /dev/null 2>&1 || return 1
  [ -x "$BIN/$4" ] || return 1
  ln -sf "$4" "$BIN/$1"
  have "$1" && note "$1" "jsvu $2@$3"
}

via_archive() { # name
  # GraalJS has no jsvu build for Apple silicon, so it alone has a macOS archive
  [ "$OS" = linux64 ] || [ "$1/$OS" = graaljs/mac64arm ] || return 1
  case $1 in
    v8)
      fetch https://storage.googleapis.com/chromium-v8/official/canary/v8-linux64-rel-14.9.207.zip v8.zip
      unzip -qo "$ENG/v8.zip" -d "$ENG/v8"
      wrap v8 "exec $ENG/v8/d8 --snapshot_blob=$ENG/v8/snapshot_blob.bin" ;;
    sm)
      fetch https://archive.mozilla.org/pub/firefox/releases/153.0b6/jsshell/jsshell-linux-x86_64.zip sm.zip
      unzip -qo "$ENG/sm.zip" -d "$ENG/sm"
      wrap sm "LD_LIBRARY_PATH=$ENG/sm exec $ENG/sm/js" ;;
    graaljs)
      local g=graaljs-community-25.1.3-linux-amd64
      [ "$OS" = mac64arm ] && g=graaljs-community-25.1.3-macos-aarch64
      fetch https://github.com/oracle/graaljs/releases/download/graal-25.1.3/$g.tar.gz graaljs.tar.gz
      tar -xzf "$ENG/graaljs.tar.gz" -C "$ENG"
      wrap graaljs "exec $ENG/$g/bin/js" ;;
    xs)
      fetch https://github.com/Moddable-OpenSource/moddable/releases/download/8.2.3/xst-lin64.zip xs.zip
      unzip -qo "$ENG/xs.zip" -d "$ENG/xs"
      chmod +x "$ENG/xs/xst"
      wrap xs "exec $ENG/xs/xst" ;;
    qjs)
      fetch https://github.com/quickjs-ng/quickjs/releases/download/v0.15.1/qjs-linux-x86_64 qjs
      chmod +x "$ENG/qjs"
      wrap qjs "exec $ENG/qjs" ;;
    *) return 1 ;;
  esac
  have "$1" && note "$1" "release archive"
}

# WebKit's archive of 316211 now answers 403 on every platform, so Linux builds
# it; a static JavaScriptCore breaks the JSCOnly configuration, and miniforge
# supplies the toolchain, so no root is needed; this takes about an hour
via_build_jsc() {
  [ "$OS" = linux64 ] || { echo "jsc: build it on Linux; macOS is not scripted" >&2; return 1; }
  local conda=$PREFIX/conda env=$PREFIX/conda/envs/jsc src=$PREFIX/webkit out=$ENG/jsc
  if [ ! -x "$out/jsc" ]; then
    if [ ! -x "$conda/bin/conda" ]; then
      curl -fsSL -o "$ENG/miniforge.sh" \
        https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-Linux-x86_64.sh
      bash "$ENG/miniforge.sh" -b -p "$conda"
    fi
    [ -d "$env" ] || "$conda/bin/conda" create -y -p "$env" -c conda-forge \
      gcc_linux-64=14 gxx_linux-64=14 sysroot_linux-64=2.28 \
      cmake ninja ruby perl python icu pkg-config
    if [ ! -d "$src/.git" ]; then
      git init -q "$src"
      git -C "$src" fetch --depth 1 https://github.com/WebKit/WebKit.git "$WEBKIT"
      git -C "$src" checkout -q FETCH_HEAD
    fi
    (
      # the activation sets CC, CXX and the flags that point at the environment
      set +u; source "$conda/bin/activate" "$env"; set -u
      cd "$src"
      Tools/Scripts/build-jsc --jsc-only --release \
        --cmakeargs="-DENABLE_STATIC_JSC=OFF -DCMAKE_PREFIX_PATH=$env"
      # jsc, its libraries from the build, and those it resolves in the environment
      local build=WebKitBuild/JSCOnly/Release
      mkdir -p "$out/lib"
      cp "$build/bin/jsc" "$out/"
      cp -a "$build"/lib/*.so* "$out/lib/"
      LD_LIBRARY_PATH=$out/lib ldd "$out/jsc" | awk -v env="$env" '$3 ~ env {print $3}' |
        xargs -r -I{} cp -L {} "$out/lib/"
    )
  fi
  wrap jsc "LD_LIBRARY_PATH=$out/lib exec $out/jsc"
  have jsc && note jsc "source build $WEBKIT"
}

via_jsvu v8 v8 14.9.207 v8-14.9.207 || via_archive v8 || echo "v8: not installed" >&2
via_jsvu jsc javascriptcore 316211 jsc-316211 || via_build_jsc || echo "jsc: not installed" >&2
via_jsvu graaljs graaljs 25.1.3 graaljs-25.1.3 || via_archive graaljs || echo "graaljs: not installed" >&2
via_jsvu sm spidermonkey 153.0b6 sm-153.0b6 || via_archive sm || echo "sm: not installed" >&2
via_jsvu xs xs 8.2.3 xs-8.2.3 || via_archive xs || echo "xs: not installed" >&2
via_jsvu qjs quickjs 0.15.1 quickjs-0.15.1 || via_archive qjs || echo "qjs: not installed" >&2

cat "$LOG"
