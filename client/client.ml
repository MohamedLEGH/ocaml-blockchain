(* proposer yojson to bigstring pour que ça soit plus simple pour l'envoyer sur le réseau *)
(* client should send command *)
s
(* liste des commandes :
help : affiche l'aide
mine : create chain or add new block
create_account : new wallet
shell : create a utop shell, connected to the server
list_block
last_block
show_block block number
sendtx amount receiver

*)
let ctx = Zmq.Context.create () in
let socket = Zmq.Socket.create ctx Zmq.Socket.req in
let _ = Zmq.Socket.connect socket "tcp://127.0.0.1:5000" in
let topic = "" 
and data_tx1 = Zmq.Msg.init_data (Base_bigstring.of_string ) in
Zmq.Socket.send_msg_all socket [];
(* Zmq.Socket.send socket "toto"; 
let msg = Zmq.Socket.recv socket in
print_endline msg;
*)
Zmq.Socket.close socket;
Zmq.Context.terminate ctx
