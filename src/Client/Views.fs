module Views

open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React

open EventMessages
open Fulma

let initialView model dispatch =
    div [] [
        form [] [
            h1 [] [ str "SpeekUp speaker shuffler" ]
            h2 [] [str "Enter Speakers Seperated by a new line"]
            textarea [
                OnChange (fun ev -> ev.Value
                                    |> AddSpeakers
                                    |> dispatch)
            ] []
            Button.span [
                Button.OnClick (fun _ -> dispatch ShuffleSpeakers)
             ] [str "Shuffle Speakers"]
        ]
        table [] [
            thead [] [
                tr [] [
                    td [] [
                        str "Speaker"
                    ]
                    td [] [
                        str "Introducer"
                    ]
                ]
            ]
        ]
    ]