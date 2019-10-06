module Types

type Speaker = { Name: string
                 Order: int }

type SpeakersIntroducers = {
    Speakers: seq<Speaker>
    Introducers: seq<Speaker>
    ErrorMessage: string option
}

type HasShuffled =
    | Shuffled of SpeakersIntroducers
    | Error of string
