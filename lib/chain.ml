open Transaction
open Block

type blockchain = {
  blocklist : block list;
  tx_pool : transaction list;
  difficulty_bits : int;
      (* accounts ?? *)
      (* block_reward : float; a ajouter plus tard *)
}
[@@deriving yojson]

exception Non_empty_blocklist
exception Empty_blocklist

let string_of_blockchain blockchain = blockchain |> yojson_of_blockchain |> Yojson.Safe.pretty_to_string
let blockchain_of_string string_val = string_val |> Yojson.Safe.from_string |> blockchain_of_yojson

let add_block_blockchain blockchain block =
  (* need to verify the data of the block *)
  { blockchain with blocklist = blockchain.blocklist @ [ block ] }

let add_tx_blockchain blockchain tx =
  (* need to verify the signature of the tx *)
  { blockchain with tx_pool = blockchain.tx_pool @ [ tx ] }

let create_genesis_block blockchain miner_address =
  match blockchain.blocklist with
  | [] ->
      let timestamp = Unix.time () in
      let first_block =
        {
          index = 0;
          previous_hash = "";
          nonce = None;
          timestamp = Some timestamp;
          transactions = [];
          miner_address;
          hash_val = None;
        }
      in
      let nonce = mine_block first_block blockchain.difficulty_bits in
      let hash_val = hash_block first_block nonce in
      let mined_block =
        { first_block with nonce = Some nonce; hash_val = Some (hash_val) }
      in
      add_block_blockchain blockchain mined_block
  | _ -> raise Non_empty_blocklist

let mine_new_block blockchain miner_address =
  match blockchain.blocklist with
  | [] -> raise Empty_blocklist
  | _ ->
      let last_block =
        blockchain.blocklist |> List.length |> fun x ->
        x - 1 |> List.nth blockchain.blocklist
      and timestamp = Unix.time () in
      let hash_val =
        match last_block.hash_val with
        | Some h -> h
        | None ->
            raise
              (Invalid_block
                 "Cannot mine a new block, last block doesn't have a hash")
      in
      let new_block =
        {
          index = last_block.index + 1;
          previous_hash = hash_val;
          nonce = None;
          timestamp = Some timestamp;
          transactions = blockchain.tx_pool;
          miner_address;
          hash_val = None;
        }
      in
      let nonce = mine_block new_block blockchain.difficulty_bits in
      let hash_val = hash_block new_block nonce in
      let mined_block =
        { new_block with nonce = Some nonce; hash_val = Some hash_val }
      in
      let blockchain_no_pool = { blockchain with tx_pool = [] } in
      add_block_blockchain blockchain_no_pool mined_block

let mine_block blockchain miner_address =
  match blockchain.blocklist with
  | [] -> create_genesis_block blockchain miner_address
  | _ -> mine_new_block blockchain miner_address
