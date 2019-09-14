module EventMessages

type Msg =
    | AddSpeaker of string
    | ShuffleSpeakers
    | Stop