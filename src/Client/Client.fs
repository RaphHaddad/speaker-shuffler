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
    | AddSpeaker of string
    | ShuffleSpeakers
    | Stop

let init () =
    (Seq.empty<Speaker>), Cmd.ofMsg Stop

let update msg currentModel =
    match msg with
    | ShuffleSpeakers ->
                Fable.Core.JS.console.log("shuffle speakers")
                (Seq.empty<Speaker>), Cmd.none
    | AddSpeaker speakerName ->
                Fable.Core.JS.console.log("add speaker")
                (currentModel |> Seq.append [{ Name = speakerName }]) , Cmd.none
    | _ -> (Seq.empty<Speaker>), Cmd.none

let view model dispatch =
    div [] [
        h1 [] [ str "SpeekUp speaker shuffler" ]
        h2 [] [str "Enter Speakers Seperated by a new line"]
        div [] [
            input [
                OnChange (fun ev -> ev.Value
                                    |> AddSpeaker
                                    |> dispatch)
            ]
            Button.span [
                Button.OnClick (fun _ -> dispatch ShuffleSpeakers)
             ] [str "Shuffle Speakers"]
        ]
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
