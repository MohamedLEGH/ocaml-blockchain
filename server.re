open Yojson.Safe.Util;
open Lib.Chain;
/* should allow to define server parameter(socket binding, log file, sqlite file, ... */
/* infinite loop to wait for client call*/
exception Rpc_Non_Valid(string);

let ctx = Zmq.Context.create();
let socket = Zmq.Socket.create(ctx, Zmq.Socket.rep);
Zmq.Socket.bind(socket, "tcp://127.0.0.1:5000");
/* at the beggining, read the sqlite file, if there is no file, create it */
/* better to use graph database but will look at it in the future */

/* let miner_adress = "0x4" /* should use wallet address */ */

let bc = {blocklist: [], tx_pool: [], difficulty_bits: 10};
let rec main = () => {
  print_endline("Wainting for next command");
  let json = Zmq.Socket.recv(socket) |> Yojson.Safe.from_string;
  let topic = json |> member("topic") |> to_string;
  let msg = json |> member("msg") |> to_string;
  switch (topic) {
  | "mining" =>
    let miner_address = Some(msg);
    let bc1 = mine_block(bc, miner_address);
    Zmq.Socket.send(socket, string_of_blockchain(bc1));
  | _ => raise(Rpc_Non_Valid("Non valid method"))
  };
  main();
};

print_endline("Starting blockchain node");
main();
Zmq.Socket.close(socket);
Zmq.Context.terminate(ctx);
