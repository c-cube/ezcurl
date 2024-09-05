(* inspired from curl_lwt *)

include Ezcurl_core
module Mutex = Picos_std_sync.Mutex
module Stream = Picos_std_sync.Stream
module C = Picos.Computation
module M = Curl.Multi
module Fd = Picos_io_fd

type bg_state = {
  bt: Printexc.raw_backtrace;
  mt: Curl.Multi.mt;
  fds: (Unix.file_descr, Fd.t) Hashtbl.t;
  comps: (Curl.t, Curl.curlCode C.t) Hashtbl.t;
}

type task =
  | T_add of Curl.t * Curl.curlCode C.t
  | T_timeout of { ms: int }
  | T_socket of Unix.file_descr * M.poll

type st = {
  lock: Mutex.t;
  mutable bg: (unit C.t * Picos.Fiber.t * unit Picos_std_sync.Ivar.t) option;
      (** Runs tasks from [tasks] *)
  tasks: task Stream.t;
}

let on_cancel_ _tr (self : bg_state) h = Curl.Multi.remove self.mt h

let perform_ (self : st) (h : Curl.t) : Curl.curlCode =
  let comp = C.create () in
  Stream.push self.tasks (T_add (h, comp));
  C.await comp

exception Timeout

let spawn_ (f : Picos.Fiber.t -> 'a) : 'a C.t * Picos.Fiber.t =
  let comp = C.create () in
  let fiber = Picos.Fiber.create ~forbid:false comp in
  Picos.Fiber.spawn fiber f;
  comp, fiber

let[@inline] spawn_ignore_ f : unit = ignore (spawn_ f : _ * _)

let process_task_ (self : bg_state) (t : task) =
  match t with
  | T_timeout { ms } ->
    Printf.eprintf "TIMEOUT %d\n%!" ms;
    let comp = C.create () in
    Picos_io_select.cancel_after comp
      ~seconds:(float ms *. 1000.)
      Timeout self.bt;
    spawn_ignore_ @@ fun _ ->
    (match C.await comp with
    | () -> ()
    | exception Timeout -> M.action_timeout self.mt)
  | T_add (h, comp) ->
    Printf.eprintf "ADD\n%!";
    let trigger =
      (* no IO inside handler *)
      (Picos.Trigger.from_action self h on_cancel_ [@alert "-handler"])
    in

    let attached_ok = C.try_attach comp trigger in
    assert attached_ok;

    (* register to curl *)
    Hashtbl.add self.comps h comp;
    Curl.Multi.add self.mt h
  | T_socket (u_fd, what) ->
    Printf.eprintf "POLL fd=%d\n%!" (Obj.magic u_fd : int);
    let get_fd self =
      try Hashtbl.find self.fds u_fd
      with Not_found ->
        Unix.set_nonblock u_fd;
        let fd = Fd.create u_fd in
        Hashtbl.add self.fds u_fd fd;
        fd
    in

    (match what with
    | M.POLL_REMOVE ->
      let fd = Hashtbl.find_opt self.fds u_fd in
      Hashtbl.remove self.fds u_fd;
      Option.iter Fd.decr fd
    | M.POLL_NONE -> ()
    | M.POLL_IN ->
      let fd = get_fd self in
      spawn_ignore_ (fun _ ->
          ignore (Picos_io_select.await_on fd `R : Fd.t);
          ignore (M.action self.mt u_fd M.EV_IN : int))
    | M.POLL_OUT ->
      let fd = get_fd self in
      spawn_ignore_ (fun _ ->
          ignore (Picos_io_select.await_on fd `W : Fd.t);
          ignore (M.action self.mt u_fd M.EV_OUT : int))
    | M.POLL_INOUT ->
      let fd = get_fd self in
      spawn_ignore_ (fun _ ->
          let ev =
            Picos_std_event.Event.(
              select
                [
                  wrap (Picos_io_select.on fd `R) (fun _ -> M.EV_IN);
                  wrap (Picos_io_select.on fd `W) (fun _ -> M.EV_OUT);
                ])
          in
          ignore (M.action self.mt u_fd ev : int)))

(** Process handles that are finished *)
let process_finished_ (self : bg_state) =
  let continue = ref true in
  while !continue do
    match M.remove_finished self.mt with
    | None -> continue := false
    | Some (h, code) ->
      Printf.eprintf "HANDLE DONE\n%!";
      (match Hashtbl.find_opt self.comps h with
      | None -> Printf.eprintf "curl_picos: orphan handle, how come?\n%!"
      | Some comp ->
        (* resolve computation *)
        Hashtbl.remove self.comps h;
        C.return comp code)
  done

let create () : st =
  let bt = Printexc.get_callstack 10 in
  let self = { lock = Mutex.create (); tasks = Stream.create (); bg = None } in

  (* background fiber that performs tasks submitted by curl *)
  let bg_ready = Picos_std_sync.Ivar.create () in
  (let comp, fiber =
     spawn_ @@ fun _ ->
     let mt = M.create () in
     let bg_state =
       { bt; mt; fds = Hashtbl.create 16; comps = Hashtbl.create 32 }
     in

     M.set_timer_function mt (fun ms ->
         Stream.push self.tasks (T_timeout { ms }));

     M.set_socket_function mt (fun fd what ->
         Stream.push self.tasks (T_socket (fd, what)));

     let cursor = ref (Stream.tap self.tasks) in
     Picos_std_sync.Ivar.fill bg_ready ();
     try
       while true do
         let task, new_cursor = Stream.read !cursor in
         cursor := new_cursor;
         process_task_ bg_state task;
         process_finished_ bg_state
       done
     with
     | Exit -> ()
     | exn ->
       let bt = Printexc.get_raw_backtrace () in
       Printf.eprintf "background fiber failed: %s\n%s" (Printexc.to_string exn)
         (Printexc.raw_backtrace_to_string bt)
   in
   self.bg <- Some (comp, fiber, bg_ready));
  self

(** Global state for the multi handle *)
let global = Picos_std_sync.Lazy.from_fun create

let perform h =
  Printf.eprintf "PERFORM\n%!";
  let (self : st) = Picos_std_sync.Lazy.force global in
  (match self.bg with
  | None -> assert false
  | Some (_, _, ready) -> Picos_std_sync.Ivar.read ready);
  Printf.eprintf "PERFORM_\n%!";
  perform_ self h

include Ezcurl_core.Make (struct
  type 'a t = 'a

  let return x = x
  let ( >>= ) = ( |> )
  let ( >|= ) = ( |> )
  let fail = raise
  let perform = perform
end)
