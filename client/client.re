/* peut envoyer directement un json as string sur le réseau */
/* client should send command : CLI , Args*/

/* liste des commandes :
      help : affiche l'aide
      mine : create chain or add new block > mining address
      create_account : new wallet
      shell : create a utop shell, connected to the server
      tui : should give a tui with all commands
      list_block
      last_block
      show_block block number
      sendtx amount receiver
   */

{
  let ctx = Zmq.Context.create();
  let socket = Zmq.Socket.create(ctx, Zmq.Socket.req);
  let _ = Zmq.Socket.connect(socket, "tcp://127.0.0.1:5000");
  let topic = "mining";
  let msg = "0x34";
  Zmq.Socket.send_all(socket, [topic, msg]);

  /* change to send only one message : stringified json rpc payload */

  /* Zmq.Socket.send socket "toto";
        let msg = Zmq.Socket.recv socket in
        print_endline msg;
     */
  Zmq.Socket.close(socket);
  Zmq.Context.terminate(ctx);
};
