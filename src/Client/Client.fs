module Client

open Elmish
open Elmish.React

open EventMessages
open Views

open Types
open Shuffle

let init () =
    { Speakers = Seq.empty<Speaker>
      Introducers = Seq.empty<Speaker>
      ErrorMessage = None},
    Cmd.ofMsg Stop

let update msg (currentModel:SpeakersIntroducers) =
    match msg with
    | ShuffleSpeakers ->
                let hasShuffled = shuffle currentModel.Speakers
                match hasShuffled with
                | Shuffled speakersIntroers -> ({ Speakers = speakersIntroers.Speakers
                                                  Introducers = speakersIntroers.Introducers
                                                  ErrorMessage = None}), Cmd.none
                | Error message -> ({Speakers = currentModel.Speakers
                                     Introducers = []
                                     ErrorMessage = Some message}), Cmd.none
    | AddSpeakers speakerNames ->
                let speakers = speakerNames.Split('\n')
                                |> Seq.map (fun n -> { Name = n; Order = 0 })
                {Speakers = speakers
                 Introducers = []
                 ErrorMessage = None}, Cmd.none
    | _ -> {Speakers = currentModel.Speakers
            Introducers = []
            ErrorMessage = None}, Cmd.none

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
