module ShuffleTests

open Expecto
open Types
open Shuffle;

[<Tests>]
let ShuffleRules =
  let testFx = 100000000

  testList "ShuffleRules" [
      testCase "speakers shouldn't introduce themselves" <| fun _ ->
            let test =
              let people = [{ Name = "Raph"; Order = 0 }
                            { Name = "Dave"; Order = 0 }
                            { Name = "Jane"; Order = 0 }
                            { Name = "Alex"; Order = 0 }]

              let speakers, introducers = shuffle people

              speakers
              |> Seq.iter2 (fun speaker introer ->
                                (Expect.notEqual speaker.Name introer.Name "a speaker shouldn't introduce themselves")) introducers

            [testFx]
            |> Seq.iter (fun _ -> test)

      testCase "speakers shouldn't introduce after speaking" <| fun _ ->
            let test =
                let people = [{ Name = "Raph"; Order = 0 }
                              { Name = "Dave"; Order = 0 }
                              { Name = "Jane"; Order = 0 }
                              { Name = "Alex"; Order = 0 }]

                let speakers, introducers = shuffle people

                speakers
                |> Seq.iteri2 (fun i _ speaker ->
                                  let introer = introducers |> Seq.tryItem(i + 1)
                                  match introer with
                                  | Some (introer) ->
                                                Expect.notEqual speaker.Name introer.Name "Shouldn't introduce after speaking"
                                  | None -> ()) introducers

            [testFx]
            |> Seq.iter (fun _ -> test)
  ]

[<Tests>]
let AcceptanceTests =
  let testFx = 10000000

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

                  let speakers, _ = shuffle people

                  let areEqual = people
                                        |> Seq.compareWith (fun x y ->
                                                                if x.Name = y.Name then
                                                                    0
                                                                else
                                                                    1 ) speakers
                                        |> isEqual

                  Expect.isFalse areEqual "shouldn't be the same order"

            [testFx]
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

              let areOrdersUnique speakers =
                              speakers
                              |> Seq.distinctBy (fun p -> p.Order)
                              |> Seq.length
                              |> (fun distinctLength -> distinctLength = people.Length)

              let speakers, shuffledIntroer = shuffle people

              let speakerOrderUnique = areOrdersUnique speakers
              let introerOrderUnique = areOrdersUnique shuffledIntroer

              Expect.isTrue (speakerOrderUnique) "speaker orders should be unique"
              Expect.isTrue (introerOrderUnique) "introducer orders should be unique"

          [0..10000000]
            |> Seq.iter (fun _ -> test)

    testCase "three people" <| fun _ ->
        let test =
            let speakers, _ =
                [{ Name = "Raph"; Order = 0 }
                 { Name = "Dave"; Order = 0 }
                 { Name = "Jane"; Order = 0 }
                ]
                |> shuffle
            Expect.isNotNull speakers "should have shuffled"

        [testFx]
        |> Seq.iter (fun _ -> test)

    testCase "two people" <| fun _ ->
        let test =
            let speakers, _ =
                [{ Name = "Raph"; Order = 0 }
                 { Name = "Dave"; Order = 0 }
                ]
                |> shuffle
            Expect.isNotNull speakers "should have shuffled 2 people"

        [testFx]
        |> Seq.iter (fun _ -> test)
  ]