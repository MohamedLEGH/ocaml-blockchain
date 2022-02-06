type transaction = { value : float; sender : string; receiver : string }
[@@deriving yojson]
(* should add timestamp, index and signature *)

let string_of_transaction tx = tx |> yojson_of_transaction |> Yojson.Safe.pretty_to_string  
let transaction_of_string string_val = string_val |> Yojson.Safe.from_string |> transaction_of_yojson

let string_of_transaction_raw tx =
  string_of_float tx.value ^ tx.sender ^ tx.receiver
