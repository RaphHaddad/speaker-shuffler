module EventMessages

type Msg =
    | AddSpeakers of string
    | ShuffleSpeakers
    | Stop