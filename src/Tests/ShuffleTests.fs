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

      let people = [{ Name = "Raph" }; { Name = "Dave" }; { Name = "Jane" }]

      let shuffledPeople = shuffle people

      let areEqual = people
                            |> Seq.compareWith (fun x y ->
                                                    if x.Name = y.Name then
                                                        0
                                                    else
                                                        1 ) shuffledPeople
                            |> isEqual

      Expect.isFalse areEqual "shouldn't be the same order"
  ]