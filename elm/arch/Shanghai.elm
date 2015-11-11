module Shanghai where

import Signal exposing ((<~))
import Result
import Dict
import Debug


port incomingShip : Signal { name : String, capacity : Int }
port outgoingShip : Signal { name : String }

ships =
  Signal.merge
        (Ok <~ incomingShip)
        (Err <~ outgoingShip)

update ship docks =
  case ship |> Debug.log "ships-port" of
    Ok { name, capacity } -> Dict.insert name capacity docks
    Err { name } -> Dict.remove name docks

dock = Signal.foldp update Dict.empty ships

port totalCapacity : Signal Int
port totalCapacity = (List.sum << Dict.values) <~ dock
