module ShuffleRulesTests

open Expecto
open Types
open Shuffle

[<Tests>]
let Tests =
  let testFx = 1000000000

  testList "ShuffleRules" [
      testCase "speakerIntroers shouldn't introduce themselves" <| fun _ ->
            let test =
              let people = [{ Name = "Raph"; Order = 0 }
                            { Name = "Dave"; Order = 0 }
                            { Name = "Jane"; Order = 0 }
                            { Name = "Alex"; Order = 0 }]

              let speakersIntroers = shuffle people
              match speakersIntroers with
              | Shuffled speakersIntroers ->
                  speakersIntroers.Speakers
                  |> Seq.iter2 (fun speaker introer ->
                                    (Expect.notEqual speaker.Name introer.Name "a speaker shouldn't introduce themselves")) speakersIntroers.Introducers
              | Error(_) -> Expect.isTrue false "Shuffling didn't work"

            [testFx]
            |> Seq.iter (fun _ -> test)

      testCase "speakerIntroers shouldn't introduce after speaking" <| fun _ ->
            let test =
                let people = [{ Name = "Raph"; Order = 0 }
                              { Name = "Dave"; Order = 0 }
                              { Name = "Jane"; Order = 0 }]

                let speakersIntroers = shuffle people

                match speakersIntroers with
                | Shuffled (speakersIntroers) ->
                    speakersIntroers.Speakers
                    |> Seq.iteri2 (fun i _ speaker ->
                                      let introer = speakersIntroers.Introducers |> Seq.tryItem(i + 1)
                                      match introer with
                                      | Some (introer) ->
                                                    Expect.notEqual speaker.Name introer.Name "Shouldn't introduce after speaking"
                                      | None -> ()) speakersIntroers.Introducers
                | Error(_) -> Expect.isTrue false "Shuffling didn't work"

            [testFx]
            |> Seq.iter (fun _ -> test)
  ]