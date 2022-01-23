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

let miner_address = Some (Address "0x4")

let block2 =
  { block1 with timestamp = Some 0.; miner_address = miner_address }

let tx3 = { value = 10.0; sender = "network"; receiver = "mohamed" }
let block3 = add_tx_block block2 tx3
let last_hash = hash_block block2 0
let blockchain_new = { blocklist = []; tx_pool = []; difficulty_bits = 10 }

let blockchain1 = create_genesis_block blockchain_new miner_address
let blockchain12 = add_tx_blockchain blockchain1 tx1
let blockchain13 = add_tx_blockchain blockchain12 tx2
let blockchain2 = mine_new_block blockchain13 miner_address;;

print_endline last_hash;;
print_endline (string_of_transaction tx1);;
print_endline (string_of_blockchain blockchain1);;
print_endline (string_of_blockchain blockchain2);;

let zmq_context = Zmq.Context.create() in
let socket = Zmq.Socket.create zmq_context Zmq.Socket.rep in
let _ = Zmq.Socket.bind socket "tcp://*:5000" in
let msg = Zmq.Socket.recv socket in
print_endline msg
