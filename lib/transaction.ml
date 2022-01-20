type transaction = { value : float; sender : string; receiver : string }
(* should add timestamp, index and signature *)

let string_of_transaction tx =
  "{ value : " ^ string_of_float tx.value ^ ", sender: " ^ tx.sender
  ^ ", receiver: " ^ tx.receiver ^ " }"
