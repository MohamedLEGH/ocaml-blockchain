type transaction = { value : float; sender : string; receiver : string }
(* should add timestamp, index and signature *)

type hash = Hash of string
type address = Address of string
type signature = Signature of string

type block = {
  index : int;
  previous_hash : hash;
  nonce : int option;
  timestamp : float option;
  transactions : transaction list;
  miner_address : address option;
  hash_val : hash option;
      (* miner_signature : signature; a ajouter plus tard *)
      (* block_reward : float; a ajouter plus tard *)
      (* reward_tx_number : int; a ajouter plus tard *)
}

exception Invalid_block of string

let string_of_transaction tx =
  "{ value : " ^ string_of_float tx.value ^ ", sender: " ^ tx.sender
  ^ ", receiver: " ^ tx.receiver ^ " }"

let add_tx_block block tx =
  { block with transactions = block.transactions @ [ tx ] }

let string_of_tx_list tx_list =
  let rec stringtxlist tx_list acc =
    match tx_list with
    | [] -> acc
    | head :: tail -> stringtxlist tail (acc ^ string_of_transaction head)
  in
  stringtxlist tx_list ""

let string_of_block block =
  let nonce_val =
    match block.nonce with Some n -> string_of_int n | None -> ""
  and miner_add =
    match block.miner_address with Some (Address m) -> m | None -> ""
  and p_hash = match block.previous_hash with Hash p -> p
  and timestamp_val =
    match block.timestamp with Some t -> string_of_float t | None -> ""
  and tx_list_string = string_of_tx_list block.transactions in
  let block_string =
    string_of_int block.index ^ p_hash ^ nonce_val ^ timestamp_val
    ^ tx_list_string ^ miner_add
  in
  block_string

let hash_block block nonce =
  match block with
  | { timestamp = Some _; miner_address = Some (Address _); _ } ->
      let nonce_option = Some nonce in
      let block_with_nonce = { block with nonce = nonce_option } in
      let block_string = string_of_block block_with_nonce in
      let hash_value = Digestif.SHA256.digest_string block_string in
      "0x" ^ Digestif.SHA256.to_hex hash_value
  | { timestamp = None; _ } ->
      raise (Invalid_block "Cannot hash the block, timestamp is missing")
  | { miner_address = None; _ } ->
      raise (Invalid_block "Cannot hash the block, miner address is missing")

(* will need to add the wallet to sign the block *)

let mine_block block difficulty_bits =
  let target = Z.pred (Z.shift_left Z.one (256 - difficulty_bits)) in
  let rec proof_of_work block acc =
    let hash_block_val = hash_block block acc in
    if Z.of_string hash_block_val < target then acc
    else proof_of_work block (acc + 1)
  in
  let nonce_final = proof_of_work block 0 in
  nonce_final

let tx1 = { value = 1.0; sender = "me"; receiver = "toto" }
let tx2 = { value = 2.0; sender = "me"; receiver = "toto2" }

let block1 =
  {
    index = 0;
    previous_hash = Hash "";
    nonce = None;
    timestamp = None;
    transactions = [ tx1; tx2 ];
    miner_address = None;
    hash_val = None;
  }

let block2 =
  { block1 with timestamp = Some 0.; miner_address = Some (Address "0x4") }

let tx3 = { value = 10.0; sender = "network"; receiver = "mohamed" }
let block3 = add_tx_block block2 tx3
let last_hash = hash_block block2 0;;

print_endline last_hash;;
print_endline (string_of_transaction tx1)
