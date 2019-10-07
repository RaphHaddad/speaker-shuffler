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
            img [Alt "SpeekUp logo - SpeekUp is an internal Telstra Purple program that helps people get better at public speaking"
                 Class "speekup-logo"
                 Src "SpeekUpLogo.png"
                 ]
            p [] [
                str "SpeekUp is a public speaking program created by "
                a [Href "https://twitter.com/David_Cook";
                Target "_blank"] [
                    str "David Cook. "
                ]
                str "It is run internally at ";
                a [Href "https://www.telstra.com.au/business-enterprise/services/telstra-purple"
                   Target "_blank"] [
                       str "Telstra Purple "
                   ]
                str "with the purpose of helping people become better public speakers."
            ]
            p [] [str "Shuffling Rules:"]
            ul [] [
                li [] [str "Speakers can't introduce themselves"]
                li [] [str "Speakers can't introduce after speaking"]
            ]
            textarea [
                Class "textarea"
                Placeholder "Enter speakers seperated by a new line"
                OnChange (fun ev -> ev.Value
                                    |> AddSpeakers
                                    |> dispatch)
            ] []
            button [
                Class "button is-primary"
                OnClick (fun e -> dispatch ShuffleSpeakers
                                  e.preventDefault())
             ] [str "Shuffle Speakers"]
        ]
        (match model.ErrorMessage, model.Introducers |> Seq.length with
        | Some message, _ -> div [Class "has-text-danger"] [str message]
        | _, 0 -> div [] []
        | None, x when x > 0 -> tableFor model)
        shufflerFooter
    ]