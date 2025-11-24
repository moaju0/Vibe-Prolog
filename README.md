# Vibe Prolog 🎶 💻 🐪 

In early November 2025, Anthropic had a program giving out $250 credits for Claude Code on the web. It expired on November 18, and on November 15 I still had $220 left. 

I was working on something else and as a side effect accidently vibe coded a prolog interpretor on my phone over the weekend. 

I haven't used Prolog for 20 years, but it does seem to be roughly correct. 

![It's the Vibe](./images/TheCastle.jpg)


This project is to see how far I can push it. 🚀

Did I really ask Codex to add more Emojis to this README? Yes I did...

## Try it out

```
> uv run vibeprolog.py ./examples/examples.pl  -q "mammal(X)" -v

Loading program from './examples/examples.pl'...
Successfully loaded './examples/examples.pl'
Query: mammal(X).
Solution 1:
X = dog

Solution 2:
X = cat

> uv run vibeprolog.py ./examples/examples.pl  -q "grandparent('tom', Y)" -v
Loading program from './examples/examples.pl'...
Successfully loaded './examples/examples.pl'
Query: grandparent('tom', Y).
Solution 1:
Y = ann

Solution 2:
Y = pat
```

## Error Handling

Vibe-Prolog implements ISO Prolog error reporting with structured error terms:

- **Instantiation errors**: Raised when required arguments are unbound
- **Type errors**: Raised when arguments have wrong types
- **Domain errors**: Raised when values are outside valid domains
- **Syntax errors**: Raised for parse failures

Example:
```python
from vibeprolog import PrologInterpreter

prolog = PrologInterpreter()
# This will throw an instantiation error, which is caught by catch/3
prolog.query_once("catch(arg(1, _, _), error(E, _), true), write(E)")
```


## The Rules 📜

- No human written code. Can prompt things, can tell tools what changes to make but no human written code should be used. 
- Don't deliberatly add slop. If you notice it doing something wrong, get it fixed. 
- Use all the tools you can. Add lots of tests! Add automatic code reviews! Add security audits!

## Push the AI harder 💪

- Don't settle on average code. If an AI is generating slop, tell it what is wrong and how to improve it. 

## Have fun 😄

- This is a fun project! Don't take it too seriously.

## Be very cautious ⚠️

- I don't know if this works at all. It *seems* to work, and there are some tests that work but that's it.
- I'm putting it under a MIT license but as AI written code it isn't clear if it can be copyrighted at all
- There are lots of real Prolog implementations that are probably better than this! Use them.

# Contributing 🤝

YES!

Please send in your vibe contributions! Just open a PR.

Are you a tool vendor who wants your tools used? Yes! I'd love to use them. I prefer a PR but opening an Issue also works
