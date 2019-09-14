module Views

open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React

open EventMessages
open Fulma

let initialView model dispatch =
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