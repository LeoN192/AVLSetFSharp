open BenchmarkDotNet.Running
open AVLSet.Benchmarks

[<EntryPoint>]
let main args =
    BenchmarkRunner.Run<SetBenchmarks>() |> ignore
    0