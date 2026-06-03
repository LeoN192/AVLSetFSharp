namespace AVLSet.UnitTests

open Xunit
open FsUnit.Xunit
open AVLSet.Library

module SetUnitTests =
    let rec isSetValid n mn mx =
        match n with
        | Empty -> true
        | Node(h, v, ln, rn) ->
            let isInBounds =
                mn |> Option.forall (fun mn -> v > mn) && mx |> Option.forall (fun mx -> v < mx)

            let lnHeight = Node.height ln
            let rnHeight = Node.height rn

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

    [<Fact>]
    let ``Empty tree insertion`` () =
        let resultSet = Empty |> AVLSet.add 15

        let correctSet = Node(0, 15, Empty, Empty)

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Duplicate element insertion`` () =
        let resultSet = Node(0, 15, Empty, Empty) |> AVLSet.add 15

        let correctSet = Node(0, 15, Empty, Empty)

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Insertion without rotation`` () =
        let resultSet = Node(1, 15, Node(0, 10, Empty, Empty), Empty) |> AVLSet.add 20

        let correctSet = Node(1, 15, Node(0, 10, Empty, Empty), Node(0, 20, Empty, Empty))

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Insertion with height update`` () =
        let resultSet =
            Node(2, 15, Node(1, 10, Node(0, 7, Empty, Empty), Empty), Node(0, 20, Empty, Empty))
            |> AVLSet.add 13

        let correctSet =
            Node(2, 15, Node(1, 10, Node(0, 7, Empty, Empty), Node(0, 13, Empty, Empty)), Node(0, 20, Empty, Empty))

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Left-Left rotation (RR case)`` () =
        let resultSet = Node(1, 15, Empty, Node(0, 20, Empty, Empty)) |> AVLSet.add 25

        let correctSet = Node(1, 20, Node(0, 15, Empty, Empty), Node(0, 25, Empty, Empty))

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Right-Left rotation (RL case)`` () =
        let resultSet =
            Node(2, 15, Node(0, 10, Empty, Empty), Node(1, 20, Node(0, 16, Empty, Empty), Node(0, 24, Empty, Empty)))
            |> AVLSet.add 27

        let correctSet =
            Node(
                2,
                20,
                Node(1, 15, Node(0, 10, Empty, Empty), Node(0, 16, Empty, Empty)),
                Node(1, 24, Empty, Node(0, 27, Empty, Empty))
            )

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Left-Right rotation (LR case)`` () =
        let resultSet =
            Node(2, 15, Node(1, 10, Node(0, 6, Empty, Empty), Node(0, 12, Empty, Empty)), Node(0, 20, Empty, Empty))
            |> AVLSet.add 14

        let correctSet =
            Node(
                2,
                12,
                Node(1, 10, Node(0, 6, Empty, Empty), Empty),
                Node(1, 15, Node(0, 14, Empty, Empty), Node(0, 20, Empty, Empty))
            )

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Single-node deletion`` () =
        let resultSet = Node(0, 15, Empty, Empty) |> AVLSet.delete 15

        let correctSet: AVLSet<int> = Empty

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Non-existent element deletion`` () =
        let resultSet = Node(1, 15, Node(0, 10, Empty, Empty), Empty) |> AVLSet.delete 20

        let correctSet = Node(1, 15, Node(0, 10, Empty, Empty), Empty)

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Leaf node deletion`` () =
        let resultSet =
            Node(1, 15, Node(0, 10, Empty, Empty), Node(0, 20, Empty, Empty))
            |> AVLSet.delete 10

        let correctSet = Node(1, 15, Empty, Node(0, 20, Empty, Empty))

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Deletion with single rotation`` () =
        let resultSet =
            Node(2, 15, Node(1, 10, Node(0, 6, Empty, Empty), Node(0, 12, Empty, Empty)), Node(0, 20, Empty, Empty))
            |> AVLSet.delete 20

        let correctSet =
            Node(2, 10, Node(0, 6, Empty, Empty), Node(1, 15, Node(0, 12, Empty, Empty), Empty))

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Node deletion with one child`` () =
        let resultSet =
            Node(
                2,
                15,
                Node(1, 10, Empty, Node(0, 12, Empty, Empty)),
                Node(1, 20, Node(0, 16, Empty, Empty), Node(0, 24, Empty, Empty))
            )
            |> AVLSet.delete 10

        let correctSet =
            Node(2, 15, Node(0, 12, Empty, Empty), Node(1, 20, Node(0, 16, Empty, Empty), Node(0, 24, Empty, Empty)))

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Root deletion with successor replacement`` () =
        let resultSet =
            Node(
                2,
                15,
                Node(1, 10, Node(0, 6, Empty, Empty), Node(0, 12, Empty, Empty)),
                Node(1, 20, Node(0, 16, Empty, Empty), Node(0, 24, Empty, Empty))
            )
            |> AVLSet.delete 15

        let correctSet =
            Node(
                2,
                16,
                Node(1, 10, Node(0, 6, Empty, Empty), Node(0, 12, Empty, Empty)),
                Node(1, 20, Empty, Node(0, 24, Empty, Empty))
            )

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Deletion with cascading rebalance`` () =
        let resultSet =
            Node(2, 15, Node(1, 10, Node(0, 6, Empty, Empty), Node(0, 12, Empty, Empty)), Node(0, 20, Empty, Empty))
            |> AVLSet.delete 15

        let correctSet =
            Node(2, 10, Node(0, 6, Empty, Empty), Node(1, 20, Node(0, 12, Empty, Empty), Empty))

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Complex multi-level deletion`` () =
        let resultSet =
            Node(
                3,
                15,
                Node(1, 10, Node(0, 6, Empty, Empty), Node(0, 12, Empty, Empty)),
                Node(
                    2,
                    25,
                    Node(0, 20, Empty, Empty),
                    Node(1, 30, Node(0, 27, Empty, Empty), Node(0, 33, Empty, Empty))
                )
            )
            |> AVLSet.delete 15

        let correctSet =
            Node(
                3,
                20,
                Node(1, 10, Node(0, 6, Empty, Empty), Node(0, 12, Empty, Empty)),
                Node(2, 30, Node(1, 25, Empty, Node(0, 27, Empty, Empty)), Node(0, 33, Empty, Empty))
            )

        resultSet |> should equal correctSet
