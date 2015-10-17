# SOE Notes

This is a demo project that translate subset of SOE into the Elm language.


## Haskell project setup (https://howistart.org/posts/haskell/1)

```
    # create a folder for our Haskell project
    mkdir refs; cd refs
    cabal init
    cabal sandbox init
    cabal install --only-dependencies
    cabal build
```

## Elm project setup (https://github.com/urfolomeus/seat_saver)

```
    # create a folder for our Elm project inside the web folder
    mkdir elm;    cd elm;
    # install the core and html Elm packages (leave off the -y if you want to see what's happening)
    elm package install -y
    elm package install evancz/elm-html -y
```

## Garphics in Elm
http://theburningmonk.com/2015/10/elm-fun-with-l-system-part-4/
