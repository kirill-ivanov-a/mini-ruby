open Interpreter

let tester = function
  | Ok prog -> (
      let ctx = () |> make_exec_ctx in
      match eval ctx prog with
      | Ok res -> print_value ctx res
      | Error msg -> print_endline msg )
  | Error _ -> Format.printf "syntax error"
