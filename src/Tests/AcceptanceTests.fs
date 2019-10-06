module AcceptanceTests

open Expecto
open Types
open Shuffle;

[<Tests>]
let Tests =
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

                  let speakerIntroers = shuffle people

                  match speakerIntroers with
                  | Shuffled speakerIntroers ->
                      let areEqual = people
                                            |> Seq.compareWith (fun x y ->
                                                                    if x.Name = y.Name then
                                                                        0
                                                                    else
                                                                        1 ) speakerIntroers.Speakers
                                            |> isEqual
                      Expect.isFalse areEqual "shouldn't be the same order"
                  | Error _ -> Expect.isTrue false "didn't shuffle"


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

              let areOrdersUnique speakerIntroers =
                              speakerIntroers
                              |> Seq.distinctBy (fun p -> p.Order)
                              |> Seq.length
                              |> (fun distinctLength -> distinctLength = people.Length)

              let speakersIntroers = shuffle people

              match speakersIntroers with
              | Shuffled speakersIntroers ->
                  let speakerOrderUnique = areOrdersUnique speakersIntroers.Speakers
                  let introerOrderUnique = areOrdersUnique speakersIntroers.Introducers

                  Expect.isTrue (speakerOrderUnique) "speaker orders should be unique"
                  Expect.isTrue (introerOrderUnique) "introducer orders should be unique"
              | Error _ -> Expect.isTrue false "should've shuffled"

          [testFx]
            |> Seq.iter (fun _ -> test)

    testCase "three people" <| fun _ ->
        let test =
            let speakerIntroers =
                [{ Name = "Raph"; Order = 0 }
                 { Name = "Dave"; Order = 0 }
                 { Name = "Jane"; Order = 0 }
                ]
                |> shuffle

            match speakerIntroers with
            | Shuffled speakerIntroers ->
                Expect.isNotNull speakerIntroers.Speakers "should have shuffled"
            | Error _ -> Expect.isTrue false "should've shuffled"

        [testFx]
        |> Seq.iter (fun _ -> test)

    testCase "two people" <| fun _ ->
        let test =
            let speakerIntroers =
                [{ Name = "Raph"; Order = 0 }
                 { Name = "Dave"; Order = 0 }
                ]
                |> shuffle

            match speakerIntroers with
            | Shuffled speakerIntroers ->
                Expect.isNotNull speakerIntroers.Speakers "should have shuffled 2 people"
            | Error _ -> Expect.isTrue false "should've shuffled"

        [testFx]
        |> Seq.iter (fun _ -> test)
  ]