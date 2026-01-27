let _ = print_endline "Hello, World!"
let rec rev (l : 'abcd list): 'abcd list = match l with
    | a :: b -> (rev b) @ [a]
    | [] -> []
let my_char = 'c'
let my_char = '\n'
