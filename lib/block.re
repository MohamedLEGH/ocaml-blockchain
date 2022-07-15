open Transaction;

/* miner_signature : signature; a ajouter plus tard */
/* block_reward : float; a ajouter plus tard */
/* reward_tx_number : int; a ajouter plus tard */
[@deriving yojson]
type block = {
  index: int,
  previous_hash: string,
  nonce: option(int),
  timestamp: option(float),
  transactions: list(transaction),
  miner_address: option(string),
  hash_val: option(string),
};

exception Invalid_block(string);

let add_tx_block = (block, tx) => {
  ...block,
  transactions: block.transactions @ [tx],
};

let string_of_tx_list_raw = tx_list => {
  let rec stringtxlist = (tx_list, acc) =>
    switch (tx_list) {
    | [] => acc
    | [head, ...tail] =>
      stringtxlist(tail, acc ++ string_of_transaction_raw(head))
    };

  stringtxlist(tx_list, "");
};

let string_of_block = block =>
  block |> yojson_of_block |> Yojson.Safe.pretty_to_string;
let block_of_string = string_val =>
  string_val |> Yojson.Safe.from_string |> block_of_yojson;

let string_of_block_raw = block => {
  let nonce_val =
    switch (block.nonce) {
    | Some(n) => string_of_int(n)
    | None => ""
    }
  and miner_add =
    switch (block.miner_address) {
    | Some(m) => m
    | None => ""
    }
  and p_hash =
    switch (block.previous_hash) {
    | p => p
    }
  and timestamp_val =
    switch (block.timestamp) {
    | Some(t) => string_of_float(t)
    | None => ""
    }
  and tx_list_string = string_of_tx_list_raw(block.transactions);
  let block_string =
    string_of_int(block.index)
    ++ p_hash
    ++ nonce_val
    ++ timestamp_val
    ++ tx_list_string
    ++ miner_add;

  block_string;
};

let hash_block = (block, nonce) =>
  switch (block) {
  | {timestamp: Some(_), miner_address: Some(_), _} =>
    let nonce_option = Some(nonce);
    let block_with_nonce = {...block, nonce: nonce_option};
    let block_string = string_of_block_raw(block_with_nonce);
    let hash_value = Digestif.SHA256.digest_string(block_string);
    "0x" ++ Digestif.SHA256.to_hex(hash_value);
  | {timestamp: None, _} =>
    raise(Invalid_block("Cannot hash the block, timestamp is missing"))
  | {miner_address: None, _} =>
    raise(Invalid_block("Cannot hash the block, miner address is missing"))
  };

/* will need to add the wallet to sign the block */

let mine_block = (block, difficulty_bits) => {
  let target = Z.pred(Z.shift_left(Z.one, 256 - difficulty_bits));
  let rec proof_of_work = (block, acc) => {
    let hash_block_val = hash_block(block, acc);
    if (Z.of_string(hash_block_val) < target) {
      acc;
    } else {
      proof_of_work(block, acc + 1);
    };
  };

  let nonce_final = proof_of_work(block, 0);
  nonce_final;
};
