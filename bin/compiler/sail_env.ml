open Llvm

module type Env = sig
  type t
  type variable
  type in_variable

  val make_array_var : variable -> int -> in_variable
  val make_ground_var : variable -> in_variable

  type composite_type

  type global_defs

  val get_globals : t -> global_defs

  
  val empty : unit -> t

  val get_var : t -> string -> in_variable
  val declare_var : t -> string -> in_variable -> t

  val get_value : in_variable -> variable

  val array_length : in_variable -> int

  val declare_struct : t -> string -> string list -> t

  val get_struct_field : t -> (string * string) -> int
  val print_env : t -> unit

end

module type StackEnv = 
sig
  include Env

  type stack
  type frame
  val push_frame :  t -> frame -> t
  val pop_frame : t -> t

  val new_frame : t -> t
  val current_frame : t -> frame * t
end

module SailEnv  : (StackEnv with type variable = llvalue)  = 
struct
  module M = Map.Make(String)

  type variable = llvalue
  type composite_type = (string * int) List.t
  type in_variable = Ground of variable | Array of (variable * int) | Field of ((int * in_variable) list)

  type global_defs =  composite_type M.t
  type frame = in_variable M.t
  type stack = frame List.t 
  type t = stack * global_defs


  let get_globals e = snd e
  let make_array_var v n = Array (v,n)
  let make_ground_var  v = Ground v 

  let empty () = let c = M.empty in ([c],c)

  let push_frame (s,g) f = 
    f :: s , g

  let pop_frame (s,g) = 
    List.tl s , g

  let new_frame env =
    let c = M.empty in
    push_frame env c

  let current_frame = function [],_ -> failwith "environnement is empty !" | (h::t),g ->  h,(t,g)

  let get_var env name  = 
    let rec aux e = 
      let current,env = current_frame e in
      match M.find_opt name current with 
      | Some v -> v
      | None when fst env = [] ->  Printf.sprintf "variable %s doesn't exist !" name |> failwith
      | _ -> aux env
      in aux env


  let get_value = function Array (v,_) -> v | Ground v -> v | Field _ -> failwith "not done yet"

  let array_length = function Array (_,n) -> n | _ -> failwith "not an array"


  let declare_var (env:t) (name:string) (variable:in_variable) =
    let current,env = current_frame env in
    match M.find_opt name current with 
    | Some _ -> Printf.sprintf "variable %s already exists in the current frame !" name |> failwith
    | None -> 
      let upd_frame = M.add name variable current in
      push_frame env upd_frame


    let declare_struct (stack,globals : t) s_name (fields : string list ) =
      match M.find_opt s_name globals with 
      | Some _ -> Printf.sprintf "type %s already exists" s_name |> failwith
      | None -> let fields = List.mapi ( fun i f -> (f,i) ) fields in
        let g = M.add s_name fields globals in
        stack,g

  
  

    let get_struct_field (_,globals : t) (t,f) =
      match M.find_opt t globals with
      | Some s -> 
          begin
            match List.assoc_opt f s with
            | Some f -> f
            | None -> Printf.sprintf "unkown field %s for struct %s" f t |> failwith
          end
      | None -> Printf.sprintf "unknown struct %s" t |> failwith
      

    (* let get_struct_field env s_name f_name = 
      let strct = Printf.sprintf "_struct_%s" s_name in
      let f_name =  strct ^ "_" ^ f_name in
      let rec aux env = 
      let current,env = current_frame env in
      match M.find_opt strct current with 
      | Some Field f -> 
        begin
          match List.assoc_opt f_name f with
          | Some i -> i
          | None -> Printf.sprintf "field %s of %s doesn't exist !" f_name s_name |> failwith
        end
      | None  when env = [] ->  Printf.sprintf "struct %s doesn't exist ! " s_name |> failwith
      | _ -> aux env
      in aux env *)

      let print_env env =
        let rec aux env : string = 
          let c,env = current_frame env in
          let p =
            M.fold 
            (
              let rec aux name = function
                | Ground _ ->  name ^ "; "
                | Array (_,n) -> Printf.sprintf "%s[%i] ; " name n
                | Field _ -> failwith "notyet"
                (* | Field fields -> fun n -> let s,_ = List.split fields in Printf.sprintf "{%s} %s" (String.concat "," s) n *)
              in fun name v -> (^) (aux name v)
            ) c "}"
          in let c = "\t{ " ^ p  in
          match env with
          | [],_ -> c ^ "\n"
          | _ -> c ^ "\n"  ^ aux env
        in try
        Logs.debug (fun m -> m "env : \n{\n %s }" (aux env) )
        with _ -> ()
end
