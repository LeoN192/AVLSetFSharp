namespace AVLSet.Library

type AVLTree<'value> =
    | Empty
    | Node of int * 'value * AVLTree<'value> * AVLTree<'value>

module Node = 
    let height n =
        match n with
        | Empty -> -1
        | Node(h, _, _, _) -> h

    let value n =
        match n with
        | Empty -> failwith "Empty node has no value"
        | Node(_, v, _, _) -> v

    let leftChild n =
        match n with
        | Empty -> failwith "Empty node has no left child"
        | Node(_, _, ln, _) -> ln

    let rightChild n =
        match n with
        | Empty -> failwith "Empty node has no right child"
        | Node(_, _, _, rn) -> rn
    
    let maxMinNodes n1 n2 = 
        if height n1 >= height n2 then n1, n2 else n2, n1

module Tree = 
    let LLrotate n = 
        let ln = Node.leftChild n
        let lln = Node.leftChild ln
        let rln = Node.rightChild ln
        let rn = Node.rightChild n
        let rlnNew = Node(max (Node.height rln) (Node.height rn) + 1,
            Node.value n, rln, rn)
        Node(max (Node.height lln) (Node.height rlnNew) + 1,
            Node.value ln, lln, rlnNew)

    let RRrotate n =
        let ln = Node.leftChild n
        let rn = Node.rightChild n
        let rrn = Node.rightChild rn
        let lrn = Node.leftChild rn
        let lrnNew = Node(max (Node.height ln) (Node.height lrn) + 1,
            Node.value n, ln, lrn)
        Node(max (Node.height lrnNew) (Node.height rrn) + 1,
            Node.value rn, lrnNew, rrn)

    let LRrotate n = 
        let lnNew = RRrotate (Node.leftChild n)
        let rn = Node.rightChild n
        LLrotate (Node(max (Node.height lnNew) (Node.height rn) + 1,
            Node.value n, lnNew, rn))

    let RLrotate n =
        let rnNew = LLrotate (Node.rightChild n)
        let ln = Node.leftChild n
        RRrotate (Node(max (Node.height ln) (Node.height rnNew) + 1,
            Node.value n, ln, rnNew))

    let balance ln rn v =
        let lnHeight = Node.height ln
        let rnHeight = Node.height rn
        match lnHeight - rnHeight with
            | 2 -> 
                let llnHeight = Node.height (Node.leftChild ln)
                let rlnHeight = Node.height (Node.rightChild ln)
                if llnHeight >= rlnHeight then
                    LLrotate (Node(0, v, ln, rn))
                else LRrotate (Node(0, v, ln, rn))
            | -2 ->
                let lrnHeight = Node.height (Node.leftChild rn)
                let rrnHeight = Node.height (Node.rightChild rn)
                if lrnHeight <= rrnHeight then
                    RRrotate (Node(0, v, ln, rn))
                else RLrotate (Node(0, v, ln, rn))
            | _ -> Node(max lnHeight rnHeight + 1, v, ln, rn)

    let rec minNode n = 
        match n with
        | Empty -> failwith "Empty node has no value"
        | Node(_, v, Empty, rn) -> v, rn
        | Node(_, v, ln, rn) -> 
                let value, lnNew = minNode ln
                value, balance lnNew rn v

    let rec insert value n = 
        match n with
        | Empty -> Node(0, value, Empty, Empty)
        | Node(h, v, ln, rn) -> 
            match value with
            | value when value = v -> n
            | value when value < v -> 
                let lnNew = insert value ln
                balance lnNew rn v
            | _ ->
                let rnNew = insert value rn
                balance ln rnNew v

    let rec remove value n = 
        match n with
        | Empty -> Empty
        | Node(h, v, ln, rn) -> 
            match value with
            | value when value = v -> 
                match ln, rn with
                | Empty, _ -> rn
                | _, Empty -> ln
                | _, _ -> 
                    let newValue, rnNew = minNode rn
                    balance ln rnNew newValue
            | value when value < v ->
                let lnNew = remove value ln
                balance lnNew rn v
            | _ ->
                let rnNew = remove value rn
                balance ln rnNew v

    let rec contains value n = 
        match n with
        | Empty -> false
        | Node(h, v, ln, rn) -> 
            match value with
            | value when value = v -> true
            | value when value < v -> contains value ln
            | _ -> contains value rn

    let rec traversal (func: AVLTree<'b> -> 'a -> AVLTree<'b>) nArg n = 
        match n with
        | Empty -> nArg
        | Node(_, v, ln, rn) ->
            let newNArg = traversal func nArg ln
            let newNArg2 = func newNArg v
            traversal func newNArg2 rn

    let rec copy n =
        match n with
        | Empty -> Empty
        | Node(h, v, ln, rn) -> Node(h, v, copy ln, copy rn)