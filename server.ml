(* should allow to define server parameter(socket binding, log file, sqlite file, ... *)

let ctx = Zmq.Context.create () in
let socket = Zmq.Socket.create ctx Zmq.Socket.rep in
let _ = Zmq.Socket.bind socket "tcp://127.0.0.1:5000"
(* at the beggining, read the sqlite file, if there is no file, create it *)
(* better to use graph database but will look at it in the future *)

(* and miner_adress = "0x4" (* should use wallet address *) *)

(* Chain.mine_block *)

and msg_encoded = Zmq.Socket.recv_msg socket in
let msg = Base_bigstring.to_string (Zmq.Msg.copy_data msg_encoded) in
print_endline msg;

Zmq.Socket.close socket;
Zmq.Context.terminate ctx
