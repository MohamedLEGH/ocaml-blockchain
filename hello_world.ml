type transaction = { value : float; sender : string; receiver : string }
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
  { block with transactions = tx :: block.transactions }

let rec string_of_tx_list = function
  (* need to be tail recursive ?*)
  | [] -> ""
  | head :: tail -> string_of_transaction head ^ string_of_tx_list tail

let hash_block block nonce =
  match block with
  | {
   index;
   previous_hash = Hash p_hash;
   timestamp = Some timestamp_val;
   transactions;
   miner_address = Some (Address miner_add);
   _;
  } ->
      let tx_list_string = string_of_tx_list transactions in
      let block_string =
        string_of_int index ^ p_hash ^ string_of_int nonce
        ^ string_of_float timestamp_val
        ^ tx_list_string ^ miner_add
      in
      let hash_value = Digestif.SHA256.digest_string block_string in
      "0x" ^ Digestif.SHA256.to_hex hash_value
  | { timestamp = None; _ } ->
      raise (Invalid_block "Cannot hash the block, timestamp is missing")
  | { miner_address = None; _ } ->
      raise (Invalid_block "Cannot hash the block, miner address is missing")

(* will need to add the wallet to sign the block *)
let mine_block_simple block difficulty =
  (* | {hash_val} need to do in recursive*)
  let timestamp_val = Some (Unix.time ())
  and miner_add = Some (Address "0x04") (* to replace with wallet.address *)
  and nonce_val = ref 0
  and difficulty_string = String.make difficulty '0' in
  let block_current =
    {
      block with
      nonce = Some !nonce_val;
      timestamp = timestamp_val;
      miner_address = miner_add;
    }
  in
  let hash_block_val = ref (hash_block block_current !nonce_val) in
  while not (String.starts_with ~prefix:difficulty_string !hash_block_val) do
    incr nonce_val;
    hash_block_val := hash_block block_current !nonce_val
  done;
  { block with nonce = Some !nonce_val; hash_val = Some (Hash !hash_block_val) }

let mine_block block difficulty_bits =
  let start_time = Unix.gettimeofday () in
  let target = Z.pred (Z.shift_left Z.one (256 - difficulty_bits))
  (* target = 2^(256-difficulty_bits) - 1 because the interval start at 0 *)
  and nonce_val = ref 0 in
  let hash_block_val = ref (hash_block block !nonce_val) in
  while Z.of_string !hash_block_val > target do
    incr nonce_val;
    hash_block_val := hash_block block !nonce_val;
    print_endline !hash_block_val
  done;
  let end_time = Unix.gettimeofday () in
  Printf.printf "Execution time: %fs\n%!" (end_time -. start_time);
  nonce_val

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

let last_hash = hash_block block2 0;;

print_endline last_hash;;
print_endline (string_of_transaction tx1)
