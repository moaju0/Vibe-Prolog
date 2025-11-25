#!/usr/bin/env bash
set -euo pipefail

# Usage: ./markdown_timelapse.sh path/to/file.md [output_basename]
#
# - path/to/file.md must be relative to the git repo root.
# - output_basename (optional) defaults to the filename without .md
#
# Environment:
#   CHROME_BIN  Path to Chrome binary (optional)
#               default: /Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome
#
# Dependencies (macOS / Homebrew):
#   brew install pandoc ffmpeg imagemagick

if [ $# -lt 1 ] || [ $# -gt 2 ]; then
  echo "Usage: $0 path/to/file.md [output_basename]" >&2
  exit 1
fi

FILE="$1"
if [ ! -f "$FILE" ]; then
  echo "Warning: $FILE does not exist in the working tree; that's OK as long as it existed in history." >&2
fi

BASENAME="${2:-$(basename "$FILE" .md)}"
FRAMES_DIR="frames_${BASENAME}"

# Chrome binary for macOS
CHROME_BIN="${CHROME_BIN:-/Applications/Google Chrome.app/Contents/MacOS/Google Chrome}"
if [ ! -x "$CHROME_BIN" ]; then
  echo "Chrome binary not found at: $CHROME_BIN" >&2
  echo "Set CHROME_BIN to your Chrome executable path, e.g.:" >&2
  echo "  export CHROME_BIN=\"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome\"" >&2
  exit 1
fi

# Check dependencies
for cmd in pandoc ffmpeg magick; do
  if ! command -v "$cmd" >/dev/null 2>&1; then
    echo "Required command not found: $cmd" >&2
    echo "Install with Homebrew, e.g.: brew install pandoc ffmpeg imagemagick" >&2
    exit 1
  fi
done

# Temporary files
COMMITS_FILE="$(mktemp /tmp/markdown_commits.XXXXXX)"
TMP_MD="$(mktemp /tmp/markdown_version.XXXXXX.md)"
TMP_HTML="$(mktemp /tmp/markdown_version.XXXXXX.html)"

cleanup() {
  rm -f "$COMMITS_FILE" "$TMP_MD" "$TMP_HTML"
}
trap cleanup EXIT

# Get commits that touched this file (oldest first)
# macOS doesn't have 'tac', but BSD 'tail -r' reverses lines.
if command -v tac >/dev/null 2>&1; then
    git log --follow --format='%H' -- "$FILE" | tac > "$COMMITS_FILE"
else
    git log --follow --format='%H' -- "$FILE" | tail -r > "$COMMITS_FILE"
fi

if [ ! -s "$COMMITS_FILE" ]; then
  echo "No commits found for $FILE" >&2
  exit 1
fi

mkdir -p "$FRAMES_DIR"

i=0
while read -r commit; do
  echo "Rendering commit $commit"

  # Extract this version of the file; skip if it didn't exist in that commit
  if ! git show "${commit}:${FILE}" > "$TMP_MD" 2>/dev/null; then
    echo "  Skipping (file not present in this commit)" >&2
    continue
  fi

  # Convert markdown to HTML
  pandoc "$TMP_MD" -s -o "$TMP_HTML"

  # Names for this frame
  frame_base=$(printf "frame_%04d" "$i")
  PDF="${FRAMES_DIR}/${frame_base}.pdf"
  PNG="${FRAMES_DIR}/${frame_base}.png"

  # Full-document PDF: Chrome will paginate but include the entire content
  "$CHROME_BIN" \
    --headless \
    --disable-gpu \
    --print-to-pdf="$PDF" \
    "file://$TMP_HTML"

  # Convert all PDF pages to a single tall PNG so nothing is truncated
  # -density 150 for decent resolution; -append stacks pages vertically.
  magick convert -density 150 "$PDF[0-]" -append "$PNG"

  i=$((i+1))
done < "$COMMITS_FILE"

if [ "$i" -eq 0 ]; then
  echo "No frames were generated; something went wrong." >&2
  exit 1
fi

echo "Generated $i frame(s) in $FRAMES_DIR"

# Build MP4 video from the PNG frames
OUTPUT_MP4="${BASENAME}_evolution.mp4"
ffmpeg -y -framerate 2 -i "$FRAMES_DIR/frame_%04d.png" \
  -vf "scale=1200:-2" \
  -pix_fmt yuv420p \
  "$OUTPUT_MP4"

echo "Timeline video written to: $OUTPUT_MP4"
echo "Done."
