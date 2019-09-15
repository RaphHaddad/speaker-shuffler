module ShuffleTests

open Expecto
open Types
open Shuffle;

[<Tests>]
let tests =
  testList "shuffler" [
    testCase "four people" <| fun _ ->
          let isEqual compareWithResult =
            match compareWithResult with
            | 0 -> true
            | _ -> false

          let people = [{ Name = "Raph"; Order = 0 }
                        { Name = "Dave"; Order = 0 }
                        { Name = "Jane"; Order = 0 }
                        { Name = "Alex"; Order = 0 }]

          let shuffledPeople = shuffle people

          let areEqual = people
                                |> Seq.compareWith (fun x y ->
                                                        if x.Name = y.Name then
                                                            0
                                                        else
                                                            1 ) shuffledPeople
                                |> isEqual

          Expect.isFalse areEqual "shouldn't be the same order"

    testCase "order must be unique" <| fun _ ->
          let isEqual compareWithResult =
            match compareWithResult with
            | 0 -> true
            | _ -> false

          let people = [{ Name = "Raph"; Order = 0 }
                        { Name = "Dave"; Order = 0 }
                        { Name = "Jane"; Order = 0 }
                        { Name = "Alex"; Order = 0 }]

          let shuffledPeople = shuffle people

          let ordersAreUnique =
                          shuffledPeople
                          |> Seq.distinctBy (fun p -> p.Order)
                          |> Seq.length
                          |> (fun distinctLength -> distinctLength = people.Length)

          Expect.isTrue ordersAreUnique "orders should be unique"

    testCase "three people" <| fun _ ->
        Expect.isTrue false "test not written"

    testCase "two people" <| fun _ ->
        Expect.isTrue false "test not written"

    testCase "no duplicates" <| fun _ ->
        Expect.isTrue false "test not written"
  ]