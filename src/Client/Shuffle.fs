module Shuffle

open Types

let shuffle speakers =
    let valid speakers =
        let lengthDistinct = speakers
                             |> Seq.distinctBy (fun s -> s.Name.ToUpperInvariant())
                             |> Seq.length
        let length = speakers
                    |> Seq.length

        length = lengthDistinct

    let randomOrder usedIndexes speaker =
        let randomNumber fromThisList =
            let rand = System.Random()

            let randomIndex = rand.Next(0, fromThisList |> Seq.length)

            fromThisList
            |> Seq.item (randomIndex)

        let lastIndex = (speakers |> Seq.length) - 1
        let order = [0..lastIndex]
                    |> Seq.filter (fun i ->
                                        not (usedIndexes
                                                |> Seq.contains(i)))
                    |> randomNumber

        { speaker with Order = order}, order::usedIndexes

    let rec shuffleIntoers shuffledSpeakers =
        let shuffledIntoers = shuffledSpeakers
                              |> Seq.mapFold randomOrder []
                              |> fst
                              |> Seq.sortBy (fun i -> i.Order)

        let isAnySpeakerIntroingThemselves =
            shuffledIntoers
            |> Seq.exists2 (fun speaker introer ->
                                (introer.Name = speaker.Name) &&
                                 introer.Order = speaker.Order) shuffledSpeakers

        let isSpeakerIntoringAfterSpeach =
            let isIntroing speaker (introer: option<Speaker>) =
                match introer with
                | Some introer -> speaker.Name = introer.Name
                | None -> false

            let length = shuffledIntoers |> Seq.length
            match length with
            | l when l <= 2 -> false
            | _ -> shuffledIntoers
                   |> Seq.mapi2 (fun i speaker _ ->
                                        isIntroing speaker (shuffledIntoers |> Seq.tryItem(i + 1))) shuffledSpeakers
                   |> Seq.exists (fun x -> x)

        match isAnySpeakerIntroingThemselves, isSpeakerIntoringAfterSpeach with
        | false, false -> shuffledIntoers
        | _ -> shuffleIntoers shuffledSpeakers

    let shuffle speakers =
        speakers
        |> Seq.mapFold randomOrder []
        |> fst
        |> Seq.sortBy (fun s -> s.Order)

    match (valid speakers) with
    | false -> Error ("Can't have duplicate names")
    | true ->
            let shuffledSpeakers = shuffle speakers
            let introducers = shuffleIntoers shuffledSpeakers

            Shuffled ({Speakers = shuffledSpeakers
                       Introducers = introducers
                       ErrorMessage = None
                       })