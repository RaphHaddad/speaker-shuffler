module Views

open Fable.React
open Fable.React.Props
open Elmish
open System
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

let shufflerFooter =
    footer [Class "footer"] [ str "©"
                              str (DateTime.Now.Year |> string)
                              str "| A "; a [ Href  "https://raph.ws/"; Target "_blank"] [str "Raphael Haddad"]; str " project |"
                              a [Href "https://github.com/RaphHaddad/speaker-shuffler"; Target "_blank"] [str "Source"] ]

let initialView (model:SpeakersIntroducers) (dispatch:Dispatch<Msg>) =
    div [] [
        form [] [
            h1 [Class "subtitle is-1"] [ str "SpeekUp shuffler" ]
            textarea [
                Class "textarea"
                Placeholder "Enter speakers seperated by a new line"
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
        shufflerFooter
    ]