type TrieNode = Node of char * TrieNode list | Leaf
type Trie = TrieNode list

//String implements Seq<char>
let (|Cons|Nil|) s = if Seq.isEmpty s then Nil else Cons(Seq.head s, Seq.skip 1 s)

let rec find str trie =
    match str, trie with
    | Nil, Leaf :: _ -> true
    | Nil, _ :: rest -> find str rest
    | Cons (c, rest), Node (c', children) :: _ when c = c' -> find rest children
    | str, _ :: rest -> find str rest
    | _ -> false
    
let rec insert str trie =
    match str, trie with
    | Nil, [] -> [ Leaf ]
    | Nil, Leaf :: _ -> trie
    | Nil, child :: rest -> child :: insert str rest
    | Cons (c, tail), [] -> [ Node(c, insert tail []) ]
    | Cons (c, tail), Node (c', children) :: rest when c = c' -> Node(c', insert tail children) :: rest
    | _, head :: rest -> head :: insert str rest

let rec delete str trie =
    match str, trie with
    | Nil, [] -> []
    | Nil, Leaf :: rest -> rest
    | Nil, node :: rest -> node :: delete str rest
    | Cons (c, tail), [] -> []
    | Cons (c, tail), Node (c', children) :: rest when c = c' ->
        match delete tail children with
        | [] -> rest
        | remaining -> Node(c', remaining) :: rest
    | _, head :: rest -> head :: delete str rest
    
let all trie =
    let rec helper acc = 
        List.collect (function 
            | Leaf -> [ new System.String (acc |> Array.ofSeq |> Array.rev) ] 
            | Node (c, children) -> helper (c :: acc) children
        )
    helper [] trie
