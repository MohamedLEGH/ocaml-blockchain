open Lib.Transaction
open Lib.Block
open Lib.Chain

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
let last_hash = hash_block block2 0
let blockchain_new = { blocklist = []; tx_pool = []; difficulty_bits = 4 }
let blockchain1 = create_genesis_block blockchain_new (Some (Address "0x4"));;

print_endline last_hash;;
print_endline (string_of_transaction tx1);;
print_endline (string_of_blockchain blockchain1)
