#!/usr/bin/env bash

set -euo pipefail

usage() {
  cat <<USAGE
Usage: $0 [-m plain|bat|bat-color] <file>

Modes:
  plain      Use raw protoc output
  bat        Run through bat without colour (useful for logs)
  bat-color  Run through bat with colour (default)
USAGE
}

mode="bat-color"

while getopts ":m:h" opt; do
  case "$opt" in
    m)
      mode="$OPTARG"
      ;;
    h)
      usage
      exit 0
      ;;
    *)
      usage >&2
      exit 1
      ;;
  esac
done
shift $((OPTIND - 1))

if [ $# -ne 1 ]; then
  usage >&2
  exit 1
fi

input="$1"

if [ ! -f "$input" ]; then
  echo "File not found: $input" >&2
  exit 1
fi

# Use our szt tool to handle decompression and header parsing
szt_cat="cabal run -v0 szt -- cat"

case "$mode" in
  plain)
    $szt_cat "$input" | protoc -I proto --decode=szt.SZTFile proto/serializotron.proto
    ;;
  bat)
    $szt_cat "$input" | protoc -I proto --decode=szt.SZTFile proto/serializotron.proto | bat --color=never --language=pbtxt --paging=never
    ;;
  bat-color)
    $szt_cat "$input" | protoc -I proto --decode=szt.SZTFile proto/serializotron.proto | bat -f --language=pbtxt --paging=never
    ;;
  *)
    echo "Unknown mode: $mode" >&2
    usage >&2
    exit 1
    ;;
esac
