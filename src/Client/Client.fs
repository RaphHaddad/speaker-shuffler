module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Fulma
open Thoth.Json

open Shared

type Speaker = { Name: string }

type Model = { Counter: Counter option }

type Msg =
    | AddSpeaker
    | ShuffleSpeakers
    | Stop

let init () =
    (Seq.empty<Speaker>), Cmd.ofMsg Stop

let update msg currentModel =
    (Seq.empty<Speaker>), Cmd.none

let view model dispatch =
    h2 [] [str "Enter Speakers Seperated by a new line"]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
