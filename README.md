# hacalc

This is an example compiler for [PatternT](https://github.com/Donaim/PatternT) language

The language expects from compilers to add builtin definitions and other related stuff that cannot be expressed with patterns  
Here is what **hacalc** adds to **PatternT**:
- operations on numbers: `$mult`, `$add`, `$div`, ..
- deep equality checker - `$equal` (it simplifies arguments before actual stupid check)
- `$num?` `$le?` `$lt?`
- staged rule application. Example:

```
a -> b
------
b -> a
```

Above will not create an infinite loop because rules separated by `-----` applied in separate groups, in order

# Examples

TBD

All of tests for the compiler located in separate repo [TBD](TBD)

# HOWTO

This program can be used as a standalone compiler, but there is also a [hacalc-ide](https://github.com/Donaim/hacalc-ide) that is much nicer to use

Instructions for standalone compiler:
1) Write patterns to `some/file`
2) Write expressions to `other/file`
3) `$ stack run -- some/fome other/file`
