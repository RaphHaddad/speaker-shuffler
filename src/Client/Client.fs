module Client

open Elmish
open Elmish.React

open Fetch.Types
open Thoth.Fetch
open Fulma
open Thoth.Json
open EventMessages
open Views

open Types
open Shuffle

let init () =
    (Seq.empty<Speaker>), Cmd.ofMsg Stop

let update msg currentModel =
    match msg with
    | ShuffleSpeakers ->
                let shuffledSpeakers, _ = shuffle currentModel
                Fable.Core.JS.console.log(System.String.Join(",", shuffledSpeakers))
                (shuffledSpeakers), Cmd.none
    | AddSpeakers speakerNames ->
                speakerNames.Split('\n')
                |> Seq.map (fun n -> { Name = n; Order = 0 }), Cmd.none
    | _ -> (Seq.empty<Speaker>), Cmd.none

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update initialView
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
