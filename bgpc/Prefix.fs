module Prefix 

(* TODO: make these 16 bits *)

type T =
    | Prefix of int * int * int * int * int option
    | True
    | False
    | Or of T * T
    | And of T * T
    | Not of T
