open! Core
open! In_channel

let () =
let input_line = In_channel.input_line_exn stdin in
  (* Process the input_line as needed *)
  Printf.printf "You entered: %s\n" input_line ;
  (* Core.never_returns (Async.Scheduler.go ()) *)
;;



(* Read a line from stdin and store it in the variable "input_line"
let () =
  let input_line = In_channel.input_line_exn stdin in
  (* Process the input_line as needed *)
  Printf.printf "You entered: %s\n" input_line
;; *)
