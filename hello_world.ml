type transaction = { value : float; sender : string; receiver : string }
type hash = Hash of string
type address = Address of string
type signature = Signature of string

type block = {
  index : int;
  previous_hash : hash;
  nonce : int option;
  timestamp : int option;
  transactions : transaction list;
  miner_address : address option;
  hash_val: hash option;
      (* miner_signature : signature; a ajouter plus tard *)
      (* block_reward : float; a ajouter plus tard *)
      (* reward_tx_number : int; a ajouter plus tard *)
}

exception Invalid_block of string

let string_of_transaction tx =
  "{ value : " ^ string_of_float tx.value ^ ", sender: " ^ tx.sender
  ^ ", receiver: " ^ tx.receiver ^ " }"

let add_tx_block block tx =
  { block with transactions = tx :: block.transactions }

let rec string_of_tx_list = function
  | [] -> ""
  | head::tail -> string_of_transaction head ^ string_of_tx_list tail

(*

mettre les raises *)

let hash_block block new_nonce = match block with
  | {index; previous_hash = Hash p_hash; timestamp = Some timestamp_val; transactions; miner_address = Some Address miner_add; _} -> 
  let tx_list_string = string_of_tx_list transactions in
  let block_string = (string_of_int index) ^ p_hash ^ (string_of_int new_nonce) ^ (string_of_int timestamp_val) ^ tx_list_string ^ miner_add in
  let hash_value = Digestif.SHA256.digest_string block_string in
  Digestif.SHA256.to_hex hash_value
  | {timestamp = None; _} -> raise (Invalid_block "Cannot hash the block, timestamp is missing")
  | {miner_address = None; _} -> raise (Invalid_block "Cannot hash the block, miner address is missing")
    

let tx1 = { value = 1.0; sender = "me"; receiver = "toto" }
let tx2 = { value = 2.0; sender = "me"; receiver = "toto2" }

let block1 =
  {
    index = 0;
    previous_hash = Hash "";
    nonce = None;
    timestamp = None;
    transactions = [tx1; tx2];
    miner_address = None;
    hash_val = None;
  }
;;

let block2 = {block1 with timestamp = Some 0; miner_address = Some(Address "0x4")};;
let last_hash = hash_block block2 0;;
print_endline last_hash;;
print_endline (string_of_transaction tx1);;
