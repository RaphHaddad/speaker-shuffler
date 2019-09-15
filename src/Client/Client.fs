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

let init () =
    (Seq.empty<Speaker>), Cmd.ofMsg Stop

let update msg currentModel =
    match msg with
    | ShuffleSpeakers ->
                Fable.Core.JS.console.log("shuffle speakers")
                (Seq.empty<Speaker>), Cmd.none
    | AddSpeaker speakerName ->
                (currentModel |> Seq.append [{ Name = speakerName; Order = 0 }]) , Cmd.none
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
