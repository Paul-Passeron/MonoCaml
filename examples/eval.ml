let rec fact n =
    if n > 1 then
        n * fact (n - 1)
    else
        1

let rec foo n = print_endline "foo"; if n = 0 then bar else foo (n-1)
and bar = print_endline "bar"
