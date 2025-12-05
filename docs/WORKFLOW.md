
## Creating new work

```
read [FEATURES.md](./FEATURES.md) and [ARCHITECTURE.md](./ARCHITECTURE.md) carefully, and review the goals in [AGENTS.md](../AGENTS.md) 

look at [currentissuelist.json](.paige/currentissuelist.json) to understand the current github issues.

Given this information and the goals of this project, after the current issues are addressed what should we work on next?
```

response...

```
how many issues should we raise for these?
```

response...

```
use the `gh issue` command line tool and create issues for all of these.

Be explicit in outlining exactly what needs to be done so an inexperienced developer can follow it. 

Be sure to include the following in each issue
- include comprehensive test coverage
- update docs/FEATURES.md
- update docs/SYNTAX_NOTES.md if needed

Don't include estimates or line numbers

Make sure you escape properly when raising the issue. Take extra care with this because it has tripped you up before. 
```

### Grooming issues

```
use `gh issue` to inspect issue <issue number>.

Can we split this into multiple issues?
```

[response]

what would be in each of the issues?

```
Use the `gh issue` tool and create these issues.

Note which ones block others

Provide comprehensive instructions for how to implement these changes.

Make sure each issue contains instructions to update FEATURES.md and to provide comprehensive test coverage. 

No estimates or line numbers should be provided
```


## Prioritise work

These use my `paige-manage-github` CLI which isn't in this project. 

### Get a list of the issues from github with all their details

`uv run --project ../paige-github-tooling paige-manage-github --list-issues`

### Sort them into priority order

`uv run --project ../paige-github-tooling paige-manage-github --prioritize-issues`

## Fix an issue

`next_issue="$(uv run --project ../paige-github-tooling paige-manage-github --next-issue)"`

# Using kilocode
uv tool run --from ./ai-tools fix-issue-with-kilocode "$next_issue"

# Using Claude
uv tool run --from ./ai-tools fix-issue-with-claude "$next_issue"

## Address PR review

Will address issues on the current PR, or can take a PR number
uv tool run --from ./ai-tools address-pr-comments-with-kilocode

