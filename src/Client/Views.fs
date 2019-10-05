module Views

open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React

open EventMessages
open Fulma
open Types

let initialView (model:SpeakersIntroducers) (dispatch:Dispatch<Msg>) =
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
            tbody []
                 (
                     model.Speakers
                     |> Seq.map2 (fun introer speaker -> tr [][
                            td [] [ str speaker.Name  ]
                            td [] [ str introer.Name  ]]) model.Introducers
                )
        ]
    ]