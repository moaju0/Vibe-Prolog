# Kilocode automations

This directory contains small tools that automate GitHub workflows using the
`kilocode` CLI and the `llm` CLI.

- `fix-issue-with-kilocode`: start from a GitHub issue, create a branch, run
  `kilocode`, commit, push, and open a PR.
- `address-pr-comments-with-kilocode`: start from a GitHub pull request,
  switch to the PR branch, run `kilocode` against the review comments, commit,
  and push.

Both tools share common helpers in the `kilocode_autocoder_utils` package and
can be installed as a `uv` tool or run directly from this directory.

## Requirements (both tools)

You need the following tools installed and available on your `PATH`:

- `uv` (for installing and running this tool)
- `git`
- `gh` (GitHub CLI, authenticated against the repo you are working in)
- `kilocode`
- `llm`

Environment:

- `OPENAI_API_KEY` must be set (used by `llm`).
- Optionally, `LLM_MODEL` can be set; if not, the script defaults to
  `gpt-5-nano`.

Repository assumptions:

- You are running the tool from a git repository.
- The default branch is `main`, and the remote is named `origin`
  (the tool uses `git log origin/main..`).

## Installation with uv

From the root of this repository:

```bash
cd ai-tools/kilocode
uv tool install .
```

This will install the `fix-issue-with-kilocode` command based on the
`[project.scripts]` entry in `pyproject.toml`. You can still run both scripts
directly from this directory.

You can verify installation with:

```bash
fix-issue-with-kilocode --help  # will show usage error but confirm the command exists
```

## Usage

Run the tool with a GitHub issue number:

```bash
fix-issue-with-kilocode 123
```

This will:

- Create a new branch like `fix-kilocode/123-some-description`.
- Run `kilocode` against the issue content.
- Commit and push any changes.
- Open a pull request on GitHub with a generated title and body.

You can still run the script directly if you prefer:

```bash
./fix-issue-with-kilocode 123
```

## `address-pr-comments-with-kilocode`

This tool helps you address review comments on an existing pull request using
`kilocode`.

Given a GitHub pull request number, the tool:

1. Detects the current repository’s GitHub `owner` and `repo` from
   `remote.origin.url`.
2. Verifies that the PR exists via `gh pr view --json` and reads the
   `headRefName` (the PR branch).
3. Checks out the PR branch (or creates it from `origin/<headRefName>` if it
   does not exist locally).
4. Runs `git pull` on that branch.
5. Invokes `ai-tools/gh-pr-helper/gh-pr-helper --owner <owner> --repo <repo> --pr <pr-number>`
   to gather the PR description and comments, capturing its output.
6. Pipes that output into `kilocode --auto` to apply automated changes that
   address the review comments.
7. Stages all changes (`git add -A`).
8. If there are staged changes, asks `llm` to generate a commit message:
   `printf '%s\n' "$GH_PR_OUTPUT" | llm -s 'give me a git commit message for changes that address these review comments'`,
   then commits the changes.
9. Pushes the branch with `git push`.

Usage from the repository root:

```bash
./ai-tools/kilocode/address-pr-comments-with-kilocode 456
```

Where `456` is the pull request number in the current repository.
