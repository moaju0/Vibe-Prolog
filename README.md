# Vibe Prolog 🎶 💻 🐪

A complete ISO Prolog interpreter implementation in Python, built entirely with AI code generation tools. This project demonstrates what's possible when you leverage multiple AI coding tools working together.

![It's the Vibe](./images/TheCastle.jpg)

## What is this?

Vibe Prolog is a fully functional Prolog interpreter that implements ~90% of the ISO Prolog core standard, including:
- Full backtracking search with Robinson unification
- 40+ built-in predicates (arithmetic, lists, I/O, meta-predicates, database operations)
- ISO-compliant parser supporting advanced syntax (character codes, multiple number bases, operators)
- Dynamic clause assertion/retraction
- Comprehensive test suite with 69+ tests

See [FEATURES.md](FEATURES.md) for complete feature coverage.

## Quick Start

```bash
# Run a Prolog program
uv run main.py ./examples/examples.pl -q "mammal(X)" -v

# Run the test suite
uv run pytest -v

# Use as a Python library
from prolog import PrologInterpreter
prolog = PrologInterpreter()
prolog.consult("my_rules.pl")
for result in prolog.query("my_query(X)"):
    print(result)
```

## Documentation

- **[FEATURES.md](FEATURES.md)** - Complete ISO Prolog coverage map with implementation status
- **[CODE_DOCS.md](CODE_DOCS.md)** - Usage guide, API reference, and examples
- **[AGENTS.md](AGENTS.md)** - Development guide, testing guidelines, and architecture
- **[Website](https://nlothian.github.io/Vibe-Prolog/)** - Full documentation site

## The Experiment

This project started as a weekend experiment to see how far AI code generation could be pushed. The rules:

- **No human-written code** - Only AI-generated code through prompting and tool use
- **Quality over speed** - Fix slop when found, don't settle for mediocre code
- **All the tools** - Use multiple AI coding assistants, automated testing, code review, security audits

## AI Tools Used

This project was developed using multiple AI coding tools working together:

### Primary Development
- **Claude Code** - Main development interface for iterative coding
- **Claude Sonnet 4.5** - Core implementation and architecture

### Automation & Workflows
- **[Kilocode](./ai-tools/kilocode/README.md)** - Automated issue-to-PR workflow
  - `fix-issue-with-kilocode` - Takes GitHub issue, creates branch, implements fix, opens PR
  - `address-pr-comments-with-kilocode` - Automatically addresses PR review comments
- **[gh-pr-helper](./ai-tools/gh-pr-helper/README.md)** - Fetches and formats PR comments for AI consumption

### Supporting Tools
- **GitHub Copilot** - Code suggestions and completions (via Cursor)
- **LLM CLI** - Commit message generation and text processing
- **UV** - Fast Python package and project management

See [ai-tools/](./ai-tools/) for detailed documentation on each tool.

## Project Status

**Current**: 69/76 tests passing (90.8%) - All core functionality works, remaining failures are parser edge cases

**Goals**: Build toward a complete, robust, fast, and scalable Prolog implementation (see [AGENTS.md](AGENTS.md))

## Contributing 🤝

Contributions welcome! Open a PR with your AI-assisted improvements.

Tool vendors: Want to add your AI coding tool to this project? Open a PR or issue!

## Cautions ⚠️

- This is an experimental project demonstrating AI code generation capabilities
- While extensively tested, use production-ready Prolog implementations (SWI-Prolog, GNU Prolog) for serious work
- Legal status of AI-generated code and copyright is evolving - provided under MIT license
- Code quality and correctness are continually improving through iterative AI refinement
