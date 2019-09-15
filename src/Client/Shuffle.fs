module Shuffle

open Types

let shuffle (speakers: seq<Speaker>) =
    let lastIndex = (speakers |> Seq.length) - 1

    let shuffle (usedIndexes:int list) speaker =
        let randomNumber fromThisList =
            let rand = System.Random()

            let randomIndex = rand.Next(0, fromThisList |> Seq.length)

            fromThisList
            |> Seq.item (randomIndex)

        let order = [0..lastIndex]
                    |> Seq.filter (fun i -> not (usedIndexes |> Seq.contains(i)))
                    |> randomNumber

        { speaker with Order = order}, order::usedIndexes

    let speakersWithOrder = fst (speakers
                                    |> Seq.mapFold shuffle [])

    speakersWithOrder
    |> Seq.sortBy (fun s -> s.Order)