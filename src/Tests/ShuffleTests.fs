module ShuffleTests

open Expecto
open Types
open Shuffle;

[<Tests>]
let ShuffleRules =
  testList "ShuffleRules" [
      testCase "speakers shouldn't introduce themselves" <| fun _ ->
            let test =
              let people = [{ Name = "Raph"; Order = 0 }
                            { Name = "Dave"; Order = 0 }
                            { Name = "Jane"; Order = 0 }
                            { Name = "Alex"; Order = 0 }]

              let shuffledPeople, introducers = shuffle people

              shuffledPeople
              |> Seq.iter2 (fun person introer ->
                                (Expect.notEqual person.Name introer.Name "a speaker shouldn't introduce themselves")) introducers

            [0..10000000]
            |> Seq.iter (fun _ -> test)
  ]

[<Tests>]
let AcceptanceTests =
  testList "AcceptanceTests" [
    testCase "four people" <| fun _ ->
            let test =
                  let isEqual compareWithResult =
                    match compareWithResult with
                    | 0 -> true
                    | _ -> false

                  let people = [{ Name = "Raph"; Order = 0 }
                                { Name = "Dave"; Order = 0 }
                                { Name = "Jane"; Order = 0 }
                                { Name = "Alex"; Order = 0 }]

                  let shuffledPeople, _ = shuffle people

                  let areEqual = people
                                        |> Seq.compareWith (fun x y ->
                                                                if x.Name = y.Name then
                                                                    0
                                                                else
                                                                    1 ) shuffledPeople
                                        |> isEqual

                  Expect.isFalse areEqual "shouldn't be the same order"

            [0..10000000]
            |> Seq.iter (fun _ -> test)

    testCase "order must be unique" <| fun _ ->
          let test =
              let isEqual compareWithResult =
                match compareWithResult with
                | 0 -> true
                | _ -> false

              let people = [{ Name = "Raph"; Order = 0 }
                            { Name = "Dave"; Order = 0 }
                            { Name = "Jane"; Order = 0 }
                            { Name = "Alex"; Order = 0 }]

              let areOrdersUnique shuffledPeople =
                              shuffledPeople
                              |> Seq.distinctBy (fun p -> p.Order)
                              |> Seq.length
                              |> (fun distinctLength -> distinctLength = people.Length)

              let shuffledPeople, shuffledIntroer = shuffle people

              let speakerOrderUnique = areOrdersUnique shuffledPeople
              let introerOrderUnique = areOrdersUnique shuffledIntroer

              Expect.isTrue (speakerOrderUnique) "speaker orders should be unique"
              Expect.isTrue (introerOrderUnique) "introducer orders should be unique"

          [0..10000000]
            |> Seq.iter (fun _ -> test)

    testCase "three people" <| fun _ ->
        Expect.isTrue false "test not written"

    testCase "two people" <| fun _ ->
        Expect.isTrue false "test not written"

    testCase "no duplicates" <| fun _ ->
        Expect.isTrue false "test not written"
  ]