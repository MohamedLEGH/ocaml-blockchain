open Transaction;
open Block;

/* accounts ?? */
/* block_reward : float; a ajouter plus tard */
[@deriving yojson]
type blockchain = {
  blocklist: list(block),
  tx_pool: list(transaction),
  difficulty_bits: int,
};

exception Non_empty_blocklist;
exception Empty_blocklist;

let string_of_blockchain = blockchain =>
  blockchain |> yojson_of_blockchain |> Yojson.Safe.pretty_to_string;
let blockchain_of_string = string_val =>
  string_val |> Yojson.Safe.from_string |> blockchain_of_yojson;

let add_block_blockchain = (blockchain, block) =>
  /* need to verify the data of the block */
  {...blockchain, blocklist: blockchain.blocklist @ [block]};

let add_tx_blockchain = (blockchain, tx) =>
  /* need to verify the signature of the tx */
  {...blockchain, tx_pool: blockchain.tx_pool @ [tx]};

let create_genesis_block = (blockchain, miner_address) =>
  switch (blockchain.blocklist) {
  | [] =>
    let timestamp = Unix.time();
    let first_block = {
      index: 0,
      previous_hash: "",
      nonce: None,
      timestamp: Some(timestamp),
      transactions: [],
      miner_address,
      hash_val: None,
    };

    let nonce = mine_block(first_block, blockchain.difficulty_bits);
    let hash_val = hash_block(first_block, nonce);
    let mined_block = {
      ...first_block,
      nonce: Some(nonce),
      hash_val: Some(hash_val),
    };

    add_block_blockchain(blockchain, mined_block);
  | _ => raise(Non_empty_blocklist)
  };

let mine_new_block = (blockchain, miner_address) =>
  switch (blockchain.blocklist) {
  | [] => raise(Empty_blocklist)
  | _ =>
    let last_block =
      blockchain.blocklist
      |> List.length
      |> (x => x - 1 |> List.nth(blockchain.blocklist))
    and timestamp = Unix.time();
    let hash_val =
      switch (last_block.hash_val) {
      | Some(h) => h
      | None =>
        raise(
          Invalid_block(
            "Cannot mine a new block, last block doesn't have a hash",
          ),
        )
      };

    let new_block = {
      index: last_block.index + 1,
      previous_hash: hash_val,
      nonce: None,
      timestamp: Some(timestamp),
      transactions: blockchain.tx_pool,
      miner_address,
      hash_val: None,
    };

    let nonce = mine_block(new_block, blockchain.difficulty_bits);
    let hash_val = hash_block(new_block, nonce);
    let mined_block = {
      ...new_block,
      nonce: Some(nonce),
      hash_val: Some(hash_val),
    };

    let blockchain_no_pool = {...blockchain, tx_pool: []};
    add_block_blockchain(blockchain_no_pool, mined_block);
  };

let mine_block = (blockchain, miner_address) =>
  switch (blockchain.blocklist) {
  | [] => create_genesis_block(blockchain, miner_address)
  | _ => mine_new_block(blockchain, miner_address)
  };
