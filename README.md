
## SOE Notes


This is a demo project that translate subset of SOE into the Elm language.

SOE stands for `The Haskell School of Expression` by Paul Hudak.

* This is educational purpose only tinkering with ML language.
* Idea is old; you can write Fortran in any program language, however
  you have to implement Lisp first.

## [Haskell project setup](https://howistart.org/posts/haskell/1)

```
    # create a folder for our Haskell project
    mkdir refs; cd refs
    cabal init
    cabal sandbox init
    cabal install --only-dependencies
    cabal build
    cabal install --enable-tests
    cabal repl tests
```

### [Elm project setup](https://github.com/urfolomeus/seat_saver)

```
    # create a folder for our Elm project inside the web folder
    mkdir elm;    cd elm;
    # install the core and html Elm packages (leave off the -y if you want to see what's happening)
    elm package install -y
    elm package install evancz/elm-html -y
```

* There is [elm/Makefile](elm/Makefile) that execute tests in console.
  It can be used with any FS watchers for fast feedback.

`$ watchfs 'sync; make compile test' *.elm tests/*.elm`

Simplest is to use `inotify` loop in console:
```bash
#!/bin/sh

CMD=$1
echo $CMD

shift
FILES=$@
echo $FILES

while true
do
    inotifywait -r -q --event=modify --exclude ".*_flymake.*" --exclude "\.#.*" --exclude "tmp" --exclude ".*\.log" $FILES
    if [ $? -eq 0 ]; then
	eval "$CMD"
	RES=$?
	TIME=`date +%T`
	if [ $RES -eq 0 ]; then
	    echo "\033[1;32m ($TIME)  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++\033[m"
	else
	    echo "\033[1;31m ($TIME)  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++\033[m"
	fi
    else
	exit
    fi
done

```

### Graphics in Elm

To make first take on graphics in ELM we follow
[Elm – fun with L-System](http://theburningmonk.com/2015/10/elm-fun-with-l-system-part-4/)
derived from another article
[Fun with L-System](http://www.clear-lines.com/blog/post/Fun-with-L-system.aspx).

* Encoding in [elm/LSystem.elm](elm/LSystem.elm) and display module
  [elm/Turtle.elm](elm/Turtle.elm) is close to original and more
  functional F# code.
* Few examples given in
  [elm/PythagorasTree.elm](elm/PythagorasTree.elm) and
  [elm/Serpinski.elm](elm/Serpinski.elm).

### Useful Links
* [The Haskell School of Expression: Exercises](http://www.elbeno.com/haskell_soe_blog/?page_id=24)

### Chapter  1. Problem Solving, Programming, and Calculation

### Chapter  2. A Module of Shapes: Part I
*elm/Shape.elm*

### Chapter  3. Simple Graphics (Snowflake)
Skipped, see LSystem files a PhD style of simple drawings.

### Chapter  4. Shapes II: Drawing Shapes

### Chapter  5. Polymorphic and Higher-Order Functions

Right not ELM has restriction on higher order polymorphous. Thought I
have not experienced any problem with `*` kind.

### Chapter  6. Shapes III: Perimeters of Shapes

Elm looks like toy language comparably to Haskell. But in fact it is
very sharp tool.  I don't know any FP lang missing basic `zip`;
`zipWith` in library.

In Elm thay expressed as `List.map2 (,)` and `List.map2 f`.

Nice!

### Chapter  7. Trees

### Chapter  8. A Module of Regions
- [Region.elm](elm/Region.elm)

Algebraic properties of Region can be checked using randomized test.

Take example of Union associative from [RegeonTests.elm][elm/tests/RegeonTests.elm].
```elm
        , ( test "Union is associative"
            -- claim that
            (\ (r1, r2, r3, x, y) ->
               (r1 `union` (r2 `union` r3))  `containsR` (x, y) )
            -- is
            (\ (r1, r2, r3, x, y) ->
               ((r1 `union` r2) `union` r3 )`containsR` (x, y) )
            -- for
            (tuple5 (shape, shape, shape, float, float))
            1000                               -- num of test
            (Random.initialSeed 42) -- seed
          )
```
* I did not use nice elm-check DSL syntax only because already has
  setup fro ElmTest console runner.  Shortcut was to employ elm-check
  to ElmTest integration and go with the single runner.

### Chapter  9. More About Higher-Order Functions

### Chapter 10. Drawing Regions
### Chapter 11. Proof by Induction
### Chapter 12. Qualified Types
### Chapter 13. A Module of Simple Animations
### Chapter 14. Programming With Streams (Memoization)
### Chapter 15. A Module of Reactive Animations
### Chapter 16. Communicating With the Outside World
### Chapter 17. Rendering Reactive Animations
### Chapter 18. Higher-Order Types
### Chapter 19. An Imperative Robot Language
### Chapter 20. Functional Music Composition
### Chapter 21. Interpreting Functional Music
### Chapter 22. From Performance to MIDI
### Chapter 23. A Tour of the PreludeList Module
### Chapter 24. A Tour of Haskell's Standard Type Classes
### Chapter 25. Appendix A. Built-in Types Are Not Special
### Chapter 26. Appendix B. Pattern-Matching Details
