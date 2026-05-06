module J = Yojson.Basic

type t =
  { pid : int
  ; ic : in_channel
  ; oc : out_channel
  ; mutable next_id : int
  }

let read_line_no_cr ic =
  let line = input_line ic in
  let n = String.length line in
  if n > 0 && line.[n - 1] = '\r'
  then String.sub line 0 (n - 1)
  else line

let read_message ic =
  let rec read_headers acc =
    let line = read_line_no_cr ic in
    if line = ""
    then acc
    else
      match String.index_opt line ':' with
      | Some i ->
        let k =
          String.lowercase_ascii (String.trim (String.sub line 0 i))
        in
        let v =
          String.trim
            (String.sub line (i + 1) (String.length line - i - 1))
        in
        read_headers ((k, v) :: acc)
      | None -> read_headers acc
  in
  let headers = read_headers [] in
  match List.assoc_opt "content-length" headers with
  | None -> failwith "lsp: missing Content-Length"
  | Some l ->
    let len = int_of_string l in
    let buf = Bytes.create len in
    really_input ic buf 0 len;
    Bytes.unsafe_to_string buf

let write_message oc body =
  let len = String.length body in
  Printf.fprintf oc "Content-Length: %d\r\n\r\n" len;
  output_string oc body;
  flush oc

let spawn ?(cmd = "typescript-language-server") ?(args = [| "--stdio" |]) () =
  let in_read, in_write = Unix.pipe () in
  let out_read, out_write = Unix.pipe () in
  let argv = Array.append [| cmd |] args in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0o644 in
  let pid =
    Unix.create_process cmd argv in_read out_write dev_null
  in
  Unix.close in_read;
  Unix.close out_write;
  Unix.close dev_null;
  let ic = Unix.in_channel_of_descr out_read in
  let oc = Unix.out_channel_of_descr in_write in
  { pid; ic; oc; next_id = 1 }

let next_id t =
  let id = t.next_id in
  t.next_id <- id + 1;
  id

let send_request t method_ params =
  let id = next_id t in
  let msg =
    `Assoc
      [ "jsonrpc", `String "2.0"
      ; "id", `Int id
      ; "method", `String method_
      ; "params", params
      ]
  in
  write_message t.oc (J.to_string msg);
  id

let send_notification t method_ params =
  let msg =
    `Assoc
      [ "jsonrpc", `String "2.0"
      ; "method", `String method_
      ; "params", params
      ]
  in
  write_message t.oc (J.to_string msg)

let rec await_response t id =
  let body = read_message t.ic in
  let msg = J.from_string body in
  match msg with
  | `Assoc kvs ->
    let resp_id = List.assoc_opt "id" kvs in
    (match resp_id with
     | Some (`Int n) when n = id -> msg
     | _ -> await_response t id)
  | _ -> await_response t id

let initialize t ~root_uri =
  let params =
    `Assoc
      [ "processId", `Int (Unix.getpid ())
      ; "rootUri", `String root_uri
      ; "capabilities",
        `Assoc
          [ ( "textDocument"
            , `Assoc
                [ ( "hover"
                  , `Assoc
                      [ ( "contentFormat"
                        , `List
                            [ `String "markdown"; `String "plaintext" ]
                        )
                      ] )
                ] )
          ]
      ; "initializationOptions", `Assoc []
      ]
  in
  let id = send_request t "initialize" params in
  let _ = await_response t id in
  send_notification t "initialized" (`Assoc [])

let did_open t ~uri ~lang_id ~version ~text =
  let params =
    `Assoc
      [ ( "textDocument"
        , `Assoc
            [ "uri", `String uri
            ; "languageId", `String lang_id
            ; "version", `Int version
            ; "text", `String text
            ] )
      ]
  in
  send_notification t "textDocument/didOpen" params

let hover t ~uri ~line ~character =
  let params =
    `Assoc
      [ ( "textDocument"
        , `Assoc [ "uri", `String uri ] )
      ; ( "position"
        , `Assoc
            [ "line", `Int line
            ; "character", `Int character
            ] )
      ]
  in
  let id = send_request t "textDocument/hover" params in
  await_response t id

let shutdown t =
  let id = send_request t "shutdown" `Null in
  let _ = await_response t id in
  send_notification t "exit" `Null;
  (try close_out t.oc with _ -> ());
  (try close_in t.ic with _ -> ());
  (try ignore (Unix.waitpid [] t.pid) with _ -> ())

let path_to_uri path =
  let abs =
    if Filename.is_relative path
    then Filename.concat (Sys.getcwd ()) path
    else path
  in
  "file://" ^ abs
