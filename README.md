# SOE Notes

This is a demo project that translate subset of SOE into the Elm language.

SOE stands for `The Haskell School of Expression` by Paul Hudak.

## Haskell project setup (https://howistart.org/posts/haskell/1)

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

## Elm project setup (https://github.com/urfolomeus/seat_saver)

    ```
    # create a folder for our Elm project inside the web folder
    mkdir elm;    cd elm;
    # install the core and html Elm packages (leave off the -y if you want to see what's happening)
    elm package install -y
    elm package install evancz/elm-html -y
    ```

    There is *elm/Makefile* that execute tests in console.

## Garphics in Elm
    To make first take on graphics in ELM we follow
    [Elm – fun with L-System](http://theburningmonk.com/2015/10/elm-fun-with-l-system-part-4/)
    derived from another article [Fun with L-System](http://www.clear-lines.com/blog/post/Fun-with-L-system.aspx).
    Encoding in *elm/LSystem.elm* and diplay module *elm/Turtle.elm* is close to original F# code.
    Few examples fiven in *elm/PythagorasTree.elm* and *elm/Serpinski.elm*.


## Usefull Links
* [The Haskell School of Expression: Exercises](http://www.elbeno.com/haskell_soe_blog/?page_id=24)

# Book Notices
## Chapter  1. Problem Solving, Programming, and Calculation
## Chapter  2. A Module of Shapes: Part I
    *elm/Shape.elm*
## Chapter  3. Simple Graphics (Snowflake)
## Chapter  4. Shapes II: Drawing Shapes
## Chapter  5. Polymorphic and Higher-Order Functions
## Chapter  6. Shapes III: Perimeters of Shapes
## Chapter  7. Trees
## Chapter  8. A Module of Regions
    *elm/Region.elm*
    Algebraic properties of Region can be checked using randomized test.
    Take example of Union assotiativity from *elm/tests/TegeonTests.elm*.
    ```elm
        , ( test "Union is assotiative"
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

# Chapter  9. More About Higher-Order Functions
# Chapter 10. Drawing Regions
# Chapter 11. Proof by Induction
# Chapter 12. Qualified Types
# Chapter 13. A Module of Simple Animations
# Chapter 14. Programming With Streams (Memoization)
# Chapter 15. A Module of Reactive Animations
# Chapter 16. Communicating With the Outside World
# Chapter 17. Rendering Reactive Animations
# Chapter 18. Higher-Order Types
# Chapter 19. An Imperative Robot Language
# Chapter 20. Functional Music Composition
# Chapter 21. Interpreting Functional Music
# Chapter 22. From Performance to MIDI
# Chapter 23. A Tour of the PreludeList Module
# Chapter 24. A Tour of Haskell's Standard Type Classes
# Chapter 25. Appendix A. Built-in Types Are Not Special
# Chapter 26. Appendix B. Pattern-Matching Details
