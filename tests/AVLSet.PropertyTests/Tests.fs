namespace AVLSet.PropertyTests

open FsUnit.Xunit
open FsCheck.Xunit
open AVLSet.Library
open AVLSet.Library.Parallel

module SetPropertyTests =
    let rec isSetValid n mn mx =
        match n with
        | Empty -> true
        | Node(h, v, ln, rn) ->
            let isInBounds =
                mn |> Option.forall (fun mn -> v > mn) && mx |> Option.forall (fun mx -> v < mx)

            let lnHeight = Tree.height ln
            let rnHeight = Tree.height rn

            isInBounds
            && h = (max lnHeight rnHeight + 1)
            && abs (lnHeight - rnHeight) <= 1
            && isSetValid ln mn (Some v)
            && isSetValid rn (Some v) mx

    let rec advancedContains (condition: 'A -> bool -> bool) setOfValues targetSet =
        match setOfValues with
        | Empty -> true
        | Node(_, v, ln, rn) ->
            let lesser, greater, wasFound = Tree.split v targetSet

            condition v wasFound
            && advancedContains condition ln lesser
            && advancedContains condition rn greater

    [<Property>]
    let ``Adding elemements to set`` (elements: int list) =
        let set = elements |> List.fold (fun t x -> AVLSet.add x t) AVLSet.empty

        let rec setContainsList list set =
            match list with
            | [] -> true
            | head :: tail -> AVLSet.contains head set && setContainsList tail set

        isSetValid set None None |> should be True
        setContainsList elements set |> should be True

    [<Property>]
    let ``Set cloning`` (elements: int list) =
        let set = elements |> List.fold (fun t x -> AVLSet.add x t) AVLSet.empty

        AVLSet.copy set |> should equal set

    [<Property>]
    let ``Deleting elements from set`` (elements: int list) =
        let set = elements |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let emptySet = elements |> List.fold (fun t e -> AVLSet.delete e t) set
        let empty: AVLSet<int> = AVLSet.empty

        emptySet |> should equal empty

    [<Property>]
    let ``Standard set union`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let unionSet = AVLSet.union setA setB

        isSetValid unionSet None None |> should be True

        (advancedContains (fun v x -> x) setA unionSet
         && advancedContains (fun v x -> x) setB unionSet)
        |> should be True

        let unionSetSwapped = AVLSet.union setB setA

        (advancedContains (fun v x -> x) unionSet unionSetSwapped
         && advancedContains (fun v x -> x) unionSetSwapped unionSet)
        |> should be True


    [<Property>]
    let ``Standard set intersection`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let intersectionSet = AVLSet.intersection setA setB

        isSetValid intersectionSet None None |> should be True

        advancedContains (fun v x -> if AVLSet.contains v setB then x else not x) setA intersectionSet
        |> should be True

        let intersectionSetSwapped = AVLSet.intersection setB setA

        (advancedContains (fun v x -> x) intersectionSet intersectionSetSwapped
         && advancedContains (fun v x -> x) intersectionSetSwapped intersectionSet)
        |> should be True

    [<Property>]
    let ``Standard set difference`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let differenceSet = AVLSet.difference setA setB

        isSetValid differenceSet None None |> should be True

        advancedContains (fun v x -> if AVLSet.contains v setB then not x else x) setA differenceSet
        |> should be True

    [<Property>]
    let ``Standard symmetric difference`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let symmDiffSet = AVLSet.symmDifference setA setB

        isSetValid symmDiffSet None None |> should be True

        (advancedContains (fun v x -> if AVLSet.contains v setB then not x else x) setA symmDiffSet
         && advancedContains (fun v x -> if AVLSet.contains v setA then not x else x) setB symmDiffSet)
        |> should be True

        let symmDiffSetSwapped = AVLSet.symmDifference setB setA

        (advancedContains (fun v x -> x) symmDiffSet symmDiffSetSwapped
         && advancedContains (fun v x -> x) symmDiffSetSwapped symmDiffSet)
        |> should be True

    [<Property>]
    let ``Union via tree traversal`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let unionSet = AVLSet.Traversal.union setA setB

        isSetValid unionSet None None |> should be True

        (advancedContains (fun v x -> x) setA unionSet
         && advancedContains (fun v x -> x) setB unionSet)
        |> should be True

        let unionSetSwapped = AVLSet.union setB setA

        (advancedContains (fun v x -> x) unionSet unionSetSwapped
         && advancedContains (fun v x -> x) unionSetSwapped unionSet)
        |> should be True

    [<Property>]
    let ``Intersection via tree traversal`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let intersectionSet = AVLSet.Traversal.intersection setA setB

        isSetValid intersectionSet None None |> should be True

        advancedContains (fun v x -> if AVLSet.contains v setB then x else not x) setA intersectionSet
        |> should be True

        let intersectionSetSwapped = AVLSet.intersection setB setA

        (advancedContains (fun v x -> x) intersectionSet intersectionSetSwapped
         && advancedContains (fun v x -> x) intersectionSetSwapped intersectionSet)
        |> should be True

    [<Property>]
    let ``Difference via tree traversal`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let differenceSet = AVLSet.Traversal.difference setA setB

        isSetValid differenceSet None None |> should be True

        advancedContains (fun v x -> if AVLSet.contains v setB then not x else x) setA differenceSet
        |> should be True

    [<Property>]
    let ``Symmetric difference via tree traversal`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let symmDiffSet = AVLSet.Traversal.symmDifference setA setB

        isSetValid symmDiffSet None None |> should be True

        (advancedContains (fun v x -> if AVLSet.contains v setB then not x else x) setA symmDiffSet
         && advancedContains (fun v x -> if AVLSet.contains v setA then not x else x) setB symmDiffSet)
        |> should be True

        let symmDiffSetSwapped = AVLSet.symmDifference setB setA

        (advancedContains (fun v x -> x) symmDiffSet symmDiffSetSwapped
         && advancedContains (fun v x -> x) symmDiffSetSwapped symmDiffSet)
        |> should be True

    [<Property>]
    let ``Parallel set union with threads`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let unionSet = ParallelAVLSet.union None setA setB

        isSetValid unionSet None None |> should be True

        (advancedContains (fun v x -> x) setA unionSet
         && advancedContains (fun v x -> x) setB unionSet)
        |> should be True

        let unionSetSwapped = AVLSet.union setB setA

        (advancedContains (fun v x -> x) unionSet unionSetSwapped
         && advancedContains (fun v x -> x) unionSetSwapped unionSet)
        |> should be True

    [<Property>]
    let ``Parallel set intersection with threads`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let intersectionSet = ParallelAVLSet.intersection None setA setB

        isSetValid intersectionSet None None |> should be True

        advancedContains (fun v x -> if AVLSet.contains v setB then x else not x) setA intersectionSet
        |> should be True

        let intersectionSetSwapped = AVLSet.intersection setB setA

        (advancedContains (fun v x -> x) intersectionSet intersectionSetSwapped
         && advancedContains (fun v x -> x) intersectionSetSwapped intersectionSet)
        |> should be True

    [<Property>]
    let ``Parallel set difference with threads`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let differenceSet = ParallelAVLSet.difference None setA setB

        isSetValid differenceSet None None |> should be True

        advancedContains (fun v x -> if AVLSet.contains v setB then not x else x) setA differenceSet
        |> should be True

    [<Property>]
    let ``Parallel set symmetric difference with threads`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let symmDiffSet = ParallelAVLSet.symmDifference None setA setB

        isSetValid symmDiffSet None None |> should be True

        (advancedContains (fun v x -> if AVLSet.contains v setB then not x else x) setA symmDiffSet
         && advancedContains (fun v x -> if AVLSet.contains v setA then not x else x) setB symmDiffSet)
        |> should be True

        let symmDiffSetSwapped = AVLSet.symmDifference setB setA

        (advancedContains (fun v x -> x) symmDiffSet symmDiffSetSwapped
         && advancedContains (fun v x -> x) symmDiffSetSwapped symmDiffSet)
        |> should be True
