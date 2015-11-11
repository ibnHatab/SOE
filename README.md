
## SOE Notes

Richard Feynman said what I cannot create I do not understand.

This project translates subset(??) of SOE into the Elm language.

* SOE stands for `The Haskell School of Expression` by `Paul Hudak`

* [The Haskell School of Expression: Exercises](http://www.elbeno.com/haskell_soe_blog/?page_id=24)

### [Haskell project setup](https://howistart.org/posts/haskell/1)

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
\#!/bin/sh

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


### Chapter  1. Problem Solving, Programming, and Calculation
Basic FP.

### Chapter  2. A Module of Shapes: Part I
It is one to one translation from Haskell code: [elm/Shape.elm](elm/Shape.elm).
There is no multiple function clause in Elm. It force you to the canonical `case/of` syntax.
### Chapter  3. Simple Graphics (Snowflake)
First take on graphics in ELM. We follow
[Elm – fun with L-System](http://theburningmonk.com/2015/10/elm-fun-with-l-system-part-4/)
derived from another article
[Fun with L-System](http://www.clear-lines.com/blog/post/Fun-with-L-system.aspx).

* Encoding in [elm/LSystem.elm](elm/LSystem.elm) and display module
  [elm/Turtle.elm](elm/Turtle.elm) is close to original and more
  functional F# code.
* Few examples given here..

** [elm/PythagorasTree.elm](elm/PythagorasTree.elm) [LiveDEMO](https://raw.githack.com/ibnHatab/SOE/master/elm/demo/PythagorasTree.html)

** [elm/Serpinski.elm](elm/Serpinski.elm) [LiveDEMO](https://raw.githack.com/ibnHatab/SOE/master/elm/demo/Serpinski.html).

- Use `left/right` keys in demo page to walk over L-System generations.

### Chapter  4. Shapes II: Drawing Shapes
SOE use raster graphics library but we will stick to 2D forms in HTML5.
We can't implement some collate like shape intersection and xor.

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

Take example of Union associative from
   [RegeonTests.elm][elm/tests/RegionTests.elm].

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
Here `shape` is generator defined as:
```elm
shape =
  let
    shrinker (RShape (Rectangle x y)) =
      (\ w h -> (RShape (Rectangle w h)))
      `Shrink.map`    shrink float x
      `Shrink.andMap` shrink float y

    generator =
      (\ w h -> (RShape (Rectangle w h)))
      `Random.Extra.map`    random float
      `Random.Extra.andMap` random float
  in
    investigator generator shrinker
```

* I did not use nice elm-check DSL syntax here, only because already
  has setup fro ElmTest console runner.  Shortcut was to employ
  elm-check to ElmTest integration and go with the single runner.

### Chapter  9. More About Higher-Order Functions
### Chapter 10. Drawing Regions
Translation is in one file [elm/Picture.elm](elm/Picture.elm)
- we draw the Picture with given color -> Region with translations -> Shape -> Collage.Shape
- Plug the Mouse clicks into update function and rearange Picture overlapping.
* [LiveDEMO](https://raw.githack.com/ibnHatab/SOE/master/elm/demo/Picture.html)

### Chapter 11. Proof by Induction
! factorial and induction on lists and naturals.
### Chapter 12. Qualified Types
EQ and others typeclasses aren't jet in Elm.
### Chapter 13. A Module of Simple Animations
Type classes seems to be main building blocks for additive Animations.
* [LiveDEMO](https://raw.githack.com/ibnHatab/SOE/master/elm/demo/Animation.html)
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
