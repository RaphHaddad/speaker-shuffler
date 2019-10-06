module Views

open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React

open EventMessages
open Fulma
open Types

let tableFor speakersIntroers =
    table [Class "table is-fullwidth"] [
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
                 speakersIntroers.Speakers
                 |> Seq.map2 (fun introer speaker -> tr [][
                        td [] [ str speaker.Name  ]
                        td [] [ str introer.Name  ]]) speakersIntroers.Introducers
            )
    ]

let initialView (model:SpeakersIntroducers) (dispatch:Dispatch<Msg>) =
    div [] [
        form [] [
            h1 [Class "subtitle is-1"] [ str "SpeekUp speaker shuffler" ]
            h2 [Class "subtitle is-2"] [str "Enter Speakers Seperated by a new line"]
            textarea [
                Class "textarea"
                OnChange (fun ev -> ev.Value
                                    |> AddSpeakers
                                    |> dispatch)
            ] []
            Button.span [
                Button.OnClick (fun _ -> dispatch ShuffleSpeakers)
             ] [str "Shuffle Speakers"]
        ]
        (match model.ErrorMessage with
        | Some message -> div [Class "has-text-danger"] [str message]
        | None -> tableFor model)
    ]