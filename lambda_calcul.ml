type term = 
  | Var of string 
  | Lam of string * term 
  | App of term * term

let colors = [|
  (fun s -> "\027[38;2;255;85;85m" ^ s ^ "\027[0m");     (* red *)
  (fun s -> "\027[38;2;255;153;85m" ^ s ^ "\027[0m");    (* orange *)
  (fun s -> "\027[38;2;255;221;85m" ^ s ^ "\027[0m");    (* yellow *)
  (fun s -> "\027[38;2;170;255;85m" ^ s ^ "\027[0m");    (* yellow-green *)
  (fun s -> "\027[38;2;85;255;85m" ^ s ^ "\027[0m");     (* green *)
  (fun s -> "\027[38;2;85;255;170m" ^ s ^ "\027[0m");    (* aqua-green *)
  (fun s -> "\027[38;2;85;221;255m" ^ s ^ "\027[0m");    (* sky blue *)
  (fun s -> "\027[38;2;85;153;255m" ^ s ^ "\027[0m");    (* blue *)
  (fun s -> "\027[38;2;170;85;255m" ^ s ^ "\027[0m");    (* violet *)
  (fun s -> "\027[38;2;255;85;255m" ^ s ^ "\027[0m");    (* magenta *)
|]

let var_color s = "\027[1;37m" ^ s ^ "\027[0m"  (* bright white *)



let string_of_term t =
  let rec aux depth t =
    let color = colors.(depth mod Array.length colors) in
    match t with
    | Var s -> var_color s
    | Lam (s, t') -> color ("(λ" ^ s ^ ".") ^ aux (depth + 1) t' ^ color ")"
    | App (t1, t2) -> aux (depth + 1) t1 ^ " " ^ aux (depth + 1) t2
  in aux 0 t

let rec free_name t =
  match t with
  | Var x -> [x]
  | Lam (x, t1) -> List.filter (fun y -> y <> x) (free_name t1)
  | App (t1, t2) -> free_name t1 @ free_name t2
  
let rec rename_if_needed x y t =
  match t with
  | Var z when z = y -> Var (y ^ "_renamed") 
  | Lam (z, t') when z = x -> Lam (z, t')
  | Lam (z, t') -> Lam (z, rename_if_needed x y t')
  | App (t1, t2) -> App (rename_if_needed x y t1, rename_if_needed x y t2)
  
let rec alpha_conv x y t =
  match t with
  | Var z when z = y -> Var (y ^ "_renamed")
  | Var z -> Var z
  | Lam (z, t') when z = x -> Lam (z, t')
  | Lam (z, t') ->
      if List.mem y (free_name t') then
        Lam (y ^ "_renamed", alpha_conv x y (rename_if_needed y (y ^ "_renamed") t'))
      else
        Lam (y, alpha_conv x y t')
  | App (t1, t2) -> App (alpha_conv x y t1, alpha_conv x y t2)  

let rec subst x s t =
  match t with
  | Var z -> if z = x then s else Var z

  | Lam (z, t') ->
      if z = x then
        Lam (z, t')
      else if List.mem z (free_name s) then
        let z' = z ^ "_renamed" in
        let t_renamed = alpha_conv z z' t' in
        Lam (z', subst x s t_renamed)
      else
        Lam (z, subst x s t')

  | App (t1, t2) ->
      App (subst x s t1, subst x s t2)

let reduce_once t =
  match t with
  | App (Lam(x, t1), t2) -> subst x t2 t1
  | _ -> t

let expr = App(
  Lam("x", App(Var "x", Var "x")),
  Lam("y", Var "y")
)

let () = print_endline (string_of_term expr)
let expr' = reduce_once expr
let () = print_endline (string_of_term expr')
