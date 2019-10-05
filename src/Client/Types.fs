module Types

type Speaker = { Name: string
                 Order: int }

type SpeakersIntroducers = {
    Speakers: seq<Speaker>
    Introducers: seq<Speaker>
}
