# hacalc

This is an example interpreter for [PatternT](https://github.com/Donaim/PatternT) library  
The library expects from interpreters to add builtin definitions and other stuff that cannot be expressed with patterns  
Here is what **hacalc** adds to **PatternT**:
- operations on numbers: `$mult`, `$add`, `$div`, ..
- deep equality checker - `$equal` (it simplifies arguments before actual stupid check)
- `$num?` `$le?` `$lt?`
- staged rule application. Example:

```
* -> +
------
+ -> *
```

Above will not create an infinite loop because rules separated by `-----` are applied in separate groups, in order

# Examples

Using 'Calculator simple' rules from here: https://github.com/Donaim/PatternT-tests/blob/master/examples/test1  
```
3 * x + (1 * x + 5 * x)
>>> 9 * x

3 * x + (1 * x + 1 + 5 * x)
>>> 1 + (9 * x)

3 * x + 3 + 5 * x + 10 + (1 * x + 1 + 5 * x)
>>> 14 + (14 * x)
```

Using 'Lambda calculus' rules from here: https://github.com/Donaim/PatternT-tests/blob/master/examples/test1  
```
+ (succ (succ 0)) (succ 0)
>>> (succ (succ (succ 0)))

show (+ (succ (succ 0)) (succ 0))
>>> 3

show (fac (succ (succ (succ (succ 0
>>> 24

> inf (succ (succ 0))
>>> true

> (succ (succ 0)) inf
>>> false
```

# HOWTO

This program can be used as a standalone compiler, but there is also a [hacalc-ide](https://github.com/Donaim/hacalc-ide) that is much nicer to use

Instructions for standalone compiler:
1) Write rules to `rules/file`
2) `$ stack run -- rules/file`
3) Input expressions
