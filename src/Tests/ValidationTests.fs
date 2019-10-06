module ValidationTests

open Expecto
open Types
open Shuffle

[<Tests>]
let Tests =
  let testFx = 1000000000

  testList "ShuffleRules" [
      testCase "Shouldn't allow duplicates" <| fun _ ->
        let people = [{ Name = "Raph"; Order = 0 }
                      { Name = "RAph"; Order = 0 }
                      { Name = "Alex"; Order = 0 }]

        let speakersIntroers = shuffle people

        match speakersIntroers with
        | Shuffled _ -> Expect.isTrue false "Should not have shuffled"
        | Error errorMessage -> Expect.equal errorMessage
                                             "Speakers must have different names"
                                             "error message wrong"

      testCase "Should strip away empty and return correct error message" <| fun _ ->
        let people = [{ Name = "Raph"; Order = 0 }
                      { Name = " "; Order = 0 }
                      { Name = ""; Order = 0 }]

        let speakersIntroers = shuffle people

        match speakersIntroers with
        | Shuffled _ -> Expect.isTrue false "Should not have shuffled"
        | Error errorMessage -> Expect.equal errorMessage
                                             "More than one speaker required"
                                             "error message wrong"

      testCase "Should strip away empty items" <| fun _ ->
        let people = [{ Name = "Raph"; Order = 0 }
                      { Name = "Bob"; Order = 0 }
                      { Name = ""; Order = 0 }]

        let speakersIntroers = shuffle people

        match speakersIntroers with
        | Shuffled speakersIntroers ->
                    Expect.equal
                        (speakersIntroers.Speakers |> Seq.length)
                        2
                        "Should have stripped empty person"
        | Error errorMessage -> Expect.isTrue false "Should have shuffled"

      testCase "Shouldn't allow one person" <| fun _ ->
        let person = [{ Name = "Raph"; Order = 0 }]

        let speakersIntroers = shuffle person

        match speakersIntroers with
        | Shuffled _ -> Expect.isTrue false "Should not have shuffled"
        | Error errorMessage -> Expect.equal errorMessage
                                             "More than one speaker required"
                                             "error message wrong"
  ]