# AVLSetFSharp

![.NET CI](https://github.com/LeoN192/AVLSetFSharp/actions/workflows/ci.yml/badge.svg)
![Formatter](https://img.shields.io/badge/format-Fantomas-blue?logo=fsharp&logoColor=white)
![.NET](https://img.shields.io/badge/.NET-10.0-purple?logo=dotnet&logoColor=white)
![License](https://img.shields.io/badge/license-BSD_3--Clause-blue)

## Overview

This repository contains a high-performance, purely functional **Set** data structure implemented using a self-balancing **AVL Tree** in F#. 

Unlike standard library collections, this implementation focuses on efficient set-theoretic operations (Union, Intersection, Difference, Symmetrical Difference) using the **Split/Join** algorithm, providing a solid foundation for both sequential and parallel data processing.

## Technical Details & Architecture

The project is built on the principles of immutability and persistent data structures. Every modification returns a new version of the set, while maximizing node sharing to optimize memory usage.

### Core Algorithms & Complexity
1. **Basic Operations**: `add`, `delete`, `contains` are implemented with $O(\log n)$ time complexity.
2. **Set-theoretic Operations**: `union`, `intersection`, `difference`, `symmetrical difference` utilize the **Split & Join** approach.
   * Efficiency: This reduces complexity to $O(m \log (n/m))$ where m is the size of the smaller set.
3. **Parallelism**: Recursive set operations are implemented using `Parallel.Invoke`.

### Invariants & Balancing
* Balance Factor: For every node, the height difference between left and right subtrees is at most 1.
* Rotations: Four types of rotations (LL, RR, LR, RL) are performed automatically.

---

## Quick Start

### Requirements
* .NET SDK 10.0+
* Fantomas

### 1. Setup & Build
```bash
# Restore local tools (Fantomas)
dotnet tool restore

# Build the entire solution
dotnet build -c Release
```

### 2. Run Tests
```bash
dotnet test
```

### 3. Run Benchmarks
```bash
dotnet run -c Release --project benchmarks/AVLSet.Benchmarks
```

---

## Usage Example

```fsharp
open AVLSet.Library

// 1. Create sets from sequences
let setA = [1..10] |> List.fold (fun s v -> AVLSet.add v s) AVLSet.empty
let setB = [5..15] |> List.fold (fun s v -> AVLSet.add v s) AVLSet.empty

// 2. Perform set operations
let unionSet = AVLSet.union setA setB
let interSet = AVLSet.intersection setA setB

// 3. Check membership
if AVLSet.contains 7 interSet then
    printfn "Intersection contains 7"

// 4. Parallel operations for large data
let opts = System.Threading.Tasks.ParallelOptions(MaxDegreeOfParallelism = 4)
let largeUnion = AVLSet.parallelUnion opts setA setB
```

---

## API Reference

The `AVLSet` module provides a comprehensive interface:

| Function | Signature | Description |
|:---|:---|:---|
| **add** | `'a -> AVLTree<'a> -> AVLTree<'a>` | Adds an element. |
| **delete** | `'a -> AVLTree<'a> -> AVLTree<'a>` | Removes an element. |
| **contains** | `'a -> AVLTree<'a> -> bool` | Checks membership. |
| **union** | `AVLTree<'a> -> AVLTree<'a> -> AVLTree<'a>` | Standard union ($A \cup B$). |
| **intersection** | `AVLTree<'a> -> AVLTree<'a> -> AVLTree<'a>` | Standard intersection ($A \cap B$). |
| **difference** | `AVLTree<'a> -> AVLTree<'a> -> AVLTree<'a>` | Standard difference ($A \setminus B$). |
| **symmDifference** | `AVLTree<'a> -> AVLTree<'a> -> AVLTree<'a>` | Standard symmetrical difference ($A \vartriangle B$). |
| **parallel(Union/Intersection/Difference/SymmDiff)**| `ParallelOptions -> AVLTree<'a> -> AVLTree<'a> -> AVLTree<'a>` | Multi-threaded set-theoretic operations. |
| **(union/intersection/difference/symmDiff)Traversal**| `AVLTree<'a> -> AVLTree<'a> -> AVLTree<'a>` | Set-theoretic operations via tree traversal. |

---

## Project Structure
```text
/src
└── AVLSet.Library       
    ├── AVLSet.Library.fsproj
    └── Library.fs
/tests
└── AVLSet.UnitTests     
    ├── AVLSet.UnitTests.fsproj
    └── Tests.fs
/benchmarks
└── AVLSet.Benchmarks    
    ├── AVLSet.Benchmarks.fsproj
    ├── Benchmarks.fs
    └── Program.fs
```
