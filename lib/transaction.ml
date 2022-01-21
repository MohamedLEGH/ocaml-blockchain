type transaction = { value : float; sender : string; receiver : string }
(* should add timestamp, index and signature *)

let string_of_transaction tx =
  "{ value : " ^ string_of_float tx.value ^ ", sender: " ^ tx.sender
  ^ ", receiver: " ^ tx.receiver ^ " }"

let string_of_transaction_raw tx =
  string_of_float tx.value ^ tx.sender ^ tx.receiver
