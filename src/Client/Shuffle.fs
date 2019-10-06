module Shuffle

open Types
open System

let shuffle speakers =
    let validate speakers =
        let strippedSpeakers = speakers
                               |> Seq.filter (fun s -> s.Name
                                                       |> String.IsNullOrWhiteSpace
                                                       |> not)

        let lengthDistinct = strippedSpeakers
                             |> Seq.distinctBy (fun s -> s.Name.ToUpperInvariant())
                             |> Seq.length
        let length = strippedSpeakers
                    |> Seq.length

        let noDuplicates = length = lengthDistinct

        match noDuplicates, lengthDistinct with
        | false, 1 -> Some "More than one speaker required with different names"
        | true, 1 -> Some "More than one speaker required"
        | false, _ -> Some "Speakers must have different names"
        | _,_ -> None

    let randomOrder usedIndexes speaker =
        let randomNumber fromThisList =
            let rand = System.Random()

            let randomIndex = rand.Next(0, fromThisList |> Seq.length)

            fromThisList
            |> Seq.item (randomIndex)

        let lastIndex = (speakers |> Seq.length) - 1
        let order = [0..lastIndex]
                    |> Seq.filter (fun i -> usedIndexes
                                            |> Seq.contains i
                                            |> not)
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
        |> Seq.filter (fun s -> s.Name
                                |> String.IsNullOrWhiteSpace
                                |> not)
        |> Seq.mapFold randomOrder []
        |> fst
        |> Seq.sortBy (fun s -> s.Order)

    match (validate speakers) with
    | Some error -> Error error
    | None ->
            let shuffledSpeakers = shuffle speakers
            let introducers = shuffleIntoers shuffledSpeakers

            Shuffled ({Speakers = shuffledSpeakers
                       Introducers = introducers
                       ErrorMessage = None
                       })