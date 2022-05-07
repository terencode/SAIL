open Llvm

module type Env = sig
  type t
  type value
  val empty : unit -> t
  val get_var : t -> string -> value
  val declare_var : t -> string -> value -> t

  val declare_struct_fields : t -> string -> string list -> t

  val get_struct_field : t -> string -> string -> int
  val print_env : t -> unit

end

module type StackEnv = 
sig
  include Env

  type frame
  val push_frame :  t -> frame -> t
  val pop_frame : t -> t

  val new_frame : t -> t
  val current_frame : t -> frame * t
end

module SailEnv : (StackEnv with type value = llvalue) = 
struct
  module M = Map.Make(String)

  type value = llvalue

  type value_or_fields = Value of value | Field of (string * int) list

  type frame = value_or_fields M.t

  type t = frame List.t

  let empty () = let c = M.empty in [c]

  let push_frame env s = 
    s :: env

  let pop_frame env = 
    List.tl env

  let new_frame env =
    let c = M.empty in
    push_frame env c

  let current_frame = function [] -> failwith "environnement is empty !" | h::t ->  h,t

  
  let print_env env =
    let rec aux env : string = 
      let c,env = current_frame env in
      let p =
        M.fold 
                    (fun _ v -> match v with 
          | Value v -> let s = Printf.sprintf "%s " (value_name v) in fun n  ->  s ^ n
          | Field fields -> fun n -> let s,_ = List.split fields in Printf.sprintf "{%s} %s" (String.concat "," s) n
          ) c "]"
      in let c = "\t[ " ^ p  in
      match env with
      | [] -> c ^ "\n"
      | _ -> c ^ "\n"  ^ aux env
    in try
    Logs.debug (fun m -> m "env : \n{\n %s }" (aux env) )
    with _ -> ()
  let get_var env name = 
    let rec aux env = 
      let current,env = current_frame env in
      match M.find_opt name current with 
      | Some Value v -> v
      | None  when env = [] ->  Printf.sprintf "variable %s doesn't exist !" name |> failwith
      | _ -> aux env
      in aux env

  let declare_var env name value =
    let current,env = current_frame env in
    match M.find_opt name current with 
    | Some _ -> Printf.sprintf "variable %s already exists in the current frame !" name |> failwith
    | None -> 
      let upd_frame = M.add name (Value value) current in
      push_frame env upd_frame


    let declare_struct_fields env s_name fields =
      let strct = Printf.sprintf "_struct_%s" s_name in
      let current,env = current_frame env in
      match M.find_opt strct current with 
      | Some _ -> Printf.sprintf "structure %s already exists in the current frame !" strct |> failwith
      | None -> let fields = List.mapi (
          fun i f -> let f_name =  strct ^ "_" ^ f in
          (f_name,i)
        ) fields  in
        let upd_frame = M.add strct (Field fields) current in
        push_frame env upd_frame
  
  
    let get_struct_field env s_name f_name = 
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
      in aux env
end
