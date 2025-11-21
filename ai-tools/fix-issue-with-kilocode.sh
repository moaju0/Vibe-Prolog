#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <issue-number>"
  exit 1
fi

ISSUE_NUMBER="$1"

gh issue view "$ISSUE_NUMBER" \
  --json title,body \
  --template '{{.title}}

{{.body}}' | kilocode --auto --parallel
