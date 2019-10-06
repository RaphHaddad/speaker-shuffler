module Tests

open Expecto

[<EntryPoint>]
let main argv =
    let writeResults = Expecto.TestResults.writeNUnitSummary ("TestResults.xml", "Expecto.Tests")
    let config = defaultConfig.appendSummaryHandler writeResults
    Tests.runTestsInAssembly config argv
