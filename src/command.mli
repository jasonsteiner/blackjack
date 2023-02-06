(** Parsing of player commands. *)

(** The type [command] represents a player command that is decomposed into an
    action. *)
type command =
  | Hit
  | Stand
  | Double
  | Split
  | Surrender
  | Evaluate
  | Quit

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is parsed. *)

val parse : string -> command
(** [parse str] parses a player's input into a [command], as follows: the first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes
    the action. Examples:

    - [parse "      double"] is [Double]
    - [parse "quit"] is [Quit]
    - [parse "   surrender  "] is [Surrender]

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces.

    Raises: [Malformed] if the command is malformed. A command is malformed if
    the action is neither "hit", "stand", "double", "split", "surrender",
    "evaluate", nor "quit", or if the action is valid but there is/are non-space
    character(s) afterwards. *)
