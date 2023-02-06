type command =
  | Hit
  | Stand
  | Double
  | Split
  | Surrender
  | Evaluate
  | Quit

exception Empty
exception Malformed

(** [remove_whitespace] pattern matches to remove the white space from a string
    list. *)
let rec remove_whitespace = function
  | [] -> []
  | h :: t -> if h = "" then remove_whitespace t else h :: remove_whitespace t

let parse str =
  match String.split_on_char ' ' str |> remove_whitespace with
  | [] -> raise Empty
  | [ "double" ] -> Double
  | [ "surrender" ] -> Surrender
  | [ "hit" ] -> Hit
  | [ "stand" ] -> Stand
  | [ "quit" ] -> Quit
  | [ "evaluate" ] -> Evaluate
  | [ "split" ] -> Split
  | _ -> raise Malformed
