open BenchmarkDotNet.Running

[<EntryPoint>]
let main argv =
    let benchmarks =
        BenchmarkSwitcher
            [| typeof<AVLSet.Benchmarks.SingleOpsBenchmark>
               typeof<AVLSet.Benchmarks.SequentialSetsBenchmark>
               typeof<AVLSet.Benchmarks.ParallelSetsBenchmark> |]

    benchmarks.Run argv |> ignore
    0
