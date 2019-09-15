module Shuffle

open Types

let shuffle speakers =
    let randomOrder usedIndexes speaker =
        let randomNumber fromThisList =
            let rand = System.Random()

            let randomIndex = rand.Next(0, fromThisList |> Seq.length)

            fromThisList
            |> Seq.item (randomIndex)

        let lastIndex = (speakers |> Seq.length) - 1
        let order = [0..lastIndex]
                    |> Seq.filter (fun i -> not (usedIndexes |> Seq.contains(i)))
                    |> randomNumber

        { speaker with Order = order}, order::usedIndexes

    let rec shuffleIntoers shuffledSpeakers =
        let shuffled, _ = (shuffledSpeakers
                            |> Seq.mapFold randomOrder [])
        let isAnySpeakerIntroingThemselves =
            shuffled
            |> Seq.exists2 (fun introer speaker ->
                                        (introer.Name = speaker.Name) &&
                                         introer.Order = speaker.Order) shuffledSpeakers

        match isAnySpeakerIntroingThemselves with
        | true -> shuffleIntoers shuffledSpeakers
        | false -> shuffled

    let shuffledSpeakers = speakers
                            |> Seq.mapFold randomOrder []
                            |> fst
                            |> Seq.sortBy (fun s -> s.Order)

    let introducers = shuffleIntoers shuffledSpeakers
                      |> Seq.sortBy (fun i -> i.Order)

    shuffledSpeakers, introducers