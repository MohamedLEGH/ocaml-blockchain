(* open Mirage_crypto.Hash.SHA256;;*)
(* open Mirage_crypto;; 

let hashing = Hash.SHA256;;
*)

let digest = Mirage_crypto.Hash.SHA256.digest;;

digest "toto";;

type transaction = { value : float; sender : string; receiver : string }
type hash = Hash of string
type address = Address of string
type signature = Signature of string

type block = {
  index : int;
  previous_hash : hash;
  mutable nonce : int option;
  mutable timestamp : int option;
  transactions : transaction list;
  mutable miner_address : address option;
      (* miner_signature : signature; a ajouter plus tard *)
      (* block_reward : float; a ajouter plus tard *)
      (* reward_tx_number : int; a ajouter plus tard *)
}

let string_of_transaction tx =
  "{ value : " ^ string_of_float tx.value ^ ", sender: " ^ tx.sender
  ^ ", receiver: " ^ tx.receiver ^ " }"

let add_tx_block block tx =
  { block with transactions = tx :: block.transactions }

let tx1 = { value = 1.0; sender = "me"; receiver = "toto" }

let block1 =
  {
    index = 0;
    previous_hash = Hash "";
    nonce = None;
    timestamp = None;
    transactions = [];
    miner_address = None;
  }
;;

print_endline (string_of_transaction tx1);;
print_endline "Hello, world"
