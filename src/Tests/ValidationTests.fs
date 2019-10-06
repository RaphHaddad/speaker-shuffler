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
                                             "Can't have duplicate names"
                                             "error message wrong"
  ]