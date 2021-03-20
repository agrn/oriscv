open Js_of_ocaml

let handler input result =
  let value = Js.to_string input##.value in
  match Int32.of_string_opt ("0x" ^ value) with
  | None -> Dom_html.window##alert (Js.string "bad format")
  | Some instr ->
    let decoded =
      Decoder.decode_instr instr
      |> Decoder.print_instr in
    result##.innerHTML := Js.string decoded

let onload () =
  let input = Dom_html.getElementById_coerce "hex" Dom_html.CoerceTo.input in
  let button = Dom_html.getElementById_opt "decode" in
  let result = Dom_html.getElementById_opt "result" in
  match input, button, result with
  | Some input, Some button, Some result ->
    button##.onclick := Dom_html.handler (fun _ ->
      handler input result;
      Js._true)
  | _, _, _ -> Dom_html.window##alert (Js.string "Failed to get elements")

let () =
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
    onload ();
    Js._false)
