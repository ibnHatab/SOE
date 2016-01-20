module Arch.TaskTutorialMain where

import Graphics.Element exposing (show)
import Task exposing (Task, andThen, succeed)
import TaskTutorial exposing (print, getCurrentTime)
import Time exposing (second, Time)

import Http
import Markdown
import Html exposing (Html)

clock : Signal Time
clock = Time.every second

printTasks : Signal (Task x ())
printTasks =
  Signal.map print clock

-- port runner : Signal (Task x ())
-- port runner : Task x ()
-- port runner = getCurrentTime `andThen` print

mb : Signal.Mailbox String
mb =
  Signal.mailbox "one"

port updateMb : Task x ()
port updateMb =
  Signal.send mb.address "Hello!"


readme : Signal.Mailbox String
readme =
  Signal.mailbox ""

report : String -> Task x ()
report markdown =
  Signal.send readme.address markdown

port fetchReadme : Task Http.Error ()
port fetchReadme =
  Http.getString readmeUrl `andThen` report

readmeUrl =
  "https://raw.githubusercontent.com/ibnHatab/SOE/master/README.md"

-- main =
--   Signal.map Markdown.toHtml readme.signal

getDuration : Task x Time
getDuration =
  getCurrentTime
  `andThen` \start -> succeed (fibonacci 30)
  `andThen` \fib -> getCurrentTime
  `andThen` \end -> succeed (end - start)

fibonacci : Int -> Int
fibonacci n =
  if n <= 2 then 1
  else
    fibonacci (n-1) + fibonacci (n-2)

port duration : Task x ()
port duration =
  getDuration `andThen` print

main = show (toString (fibonacci 30))
