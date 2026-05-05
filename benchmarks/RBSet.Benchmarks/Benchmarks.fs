namespace AVLSet.Benchmarks

open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open RBSet

[<GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)>]
[<CategoriesColumn>]
[<HtmlExporter>]
[<MemoryDiagnoser>]
[<HardwareCounters(HardwareCounter.CacheMisses, HardwareCounter.BranchMispredictions)>]
type SetBenchmarks() =
    let rnd = System.Random(1234561)

    [<Params(100, 10000, 1000000)>]
    [<DefaultValue>]
    val mutable public A: int

    [<Params(100, 1000, 100000)>]
    [<DefaultValue>]
    val mutable public B: int

    [<Params("Random", "Sorted")>]
    [<DefaultValue>]
    val mutable public DataTypeA: string

    [<Params(1, 2, 4, 8)>]
    [<DefaultValue>]
    val mutable public Threads: int

    [<DefaultValue>]
    val mutable public rndInt: int

    [<DefaultValue>]
    val mutable public setA: Tree<int>

    [<DefaultValue>]
    val mutable public setB: Tree<int>

    [<GlobalSetup>]
    member self.Setup() =
        self.rndInt <- rnd.Next(self.A + 1, self.A + 1000)

        let dataA =
            match self.DataTypeA with
            | "Random" -> Array.init self.A (fun _ -> rnd.Next())
            | _ -> [| 1 .. self.A |]

        let dataB = Array.init self.B (fun _ -> rnd.Next())

        self.setA <- dataA |> Array.fold (fun s v -> RBSet.add v s) RBSet.empty
        self.setB <- dataB |> Array.fold (fun s v -> RBSet.add v s) RBSet.empty

    [<Benchmark>]
    [<BenchmarkCategory("Adding")>]
    member self.AddingOneElement() = RBSet.add self.rndInt self.setA

    [<Benchmark>]
    [<BenchmarkCategory("Deleting")>]
    member self.DeletingOneElement() = RBSet.delete self.rndInt self.setA

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Union")>]
    member self.SequentialUnion() = RBSet.union self.setA self.setB


    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Intersection")>]
    member self.SequentialIntersection() = RBSet.intersection self.setA self.setB


    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Difference")>]
    member self.SequentialDifference() = RBSet.difference self.setA self.setB
