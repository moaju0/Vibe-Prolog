#!/usr/bin/env bash
set -euo pipefail

for cmd in jq kilocode llm; do
  if ! command -v "$cmd" >/dev/null 2>&1; then
    echo "Error: required command '$cmd' is not available in PATH" >&2
    exit 1
  fi
done

export LLM_MODEL="${LLM_MODEL:-gpt-5-nano}"

if [ $# -lt 1 ]; then
  echo "Usage: $0 <issue-number>"
  exit 1
fi

ISSUE_NUMBER="$1"

# Get the issue title, body, and comments into a form that works for models
ISSUE_CONTENT="$(
  gh issue view "$ISSUE_NUMBER" \
    --json title,body,comments \
    --template '# Issue: 
{{.title}}

# Description
{{.body}}

# Comments

{{range .comments}}## Comment
{{.body}}

{{end}}'
)"

branch_name="$(
  printf '%s\n' "$ISSUE_CONTENT" | llm "create a good git branch title for a branch that addresses this issue. It should start with \`fix-kilocode/$ISSUE_NUMBER-\`"
)"

git checkout -b "$branch_name"

printf '%s\n' "$ISSUE_CONTENT" | kilocode --auto 

git add -A

if git diff --cached --quiet; then
  echo "No changes to commit."
else
  commit_message="$(
    git diff --cached | llm -s 'give me a git commit message for these changes'
  )"
  git commit -m "$commit_message"
fi

git push --set-upstream origin "$branch_name"


pr_title_body_json="$(
  git log origin/main.. | llm --schema 'title,body' 'Looking at this git log \
 output, summarise into a \`title\` and \`body\` suitable for a pull request. \
 The \`body\` MUST start with \`Closes #$ISSUE_NUMBER\` '
)"

pr_title="$(printf '%s' "$pr_title_body_json" | jq -r '.title')"
pr_body="$(printf '%s' "$pr_title_body_json" | jq -r '.body')"

gh pr create --title "$pr_title" --body "$pr_body"
