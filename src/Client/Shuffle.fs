module Shuffle

open Types

let shuffle (speakers: seq<Speaker>) =
    let shuffleSpeakers usedIndexes speaker =
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

    let shuffleIntroers shuffledSpeakers usedIndexes speaker =
        let currentSpeakerIndex = shuffledSpeakers |> Seq.findIndex (fun s -> s.Name = speaker.Name)
        let speaker, usedIndexes = shuffleSpeakers (currentSpeakerIndex::usedIndexes) speaker
        speaker, (usedIndexes |> Seq.filter (fun i -> i <> currentSpeakerIndex) |> Seq.toList)

    let shuffledSpeakers = fst (speakers
                                    |> Seq.mapFold shuffleSpeakers [])

    let introducers = fst (speakers
                                    |> Seq.mapFold shuffleSpeakers [])

    shuffledSpeakers|> Seq.sortBy (fun s -> s.Order),
    introducers |> Seq.sortBy (fun i -> i.Order)