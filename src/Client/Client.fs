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

let init () =
    (Seq.empty<Speaker>), Cmd.ofMsg AddSpeaker

let update msg currentModel =
    currentModel, Cmd.ofMsg msg

let view model dispatch =
    div []
        [
            form [] [ h2 [] [str "Enter Speakers Seperated by a new line"]]
        ]

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
