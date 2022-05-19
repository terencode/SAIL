open Llvm
open Common
open Sail_env

type llvm_args = { c:llcontext; b:llbuilder;m:llmodule; }

type statement_return = SailEnv.t * ( SailEnv.value option )



let getLLVMType (t : sailtype) (llc: llcontext) (llm: llmodule) (env:SailEnv.t) : lltype = 
  let get_custom_type name = 
    Some (i1_type llc)
  in 
  let rec aux = function
  | Bool -> i1_type llc
  | Int -> i32_type llc 
  | Float -> double_type llc
  | Char -> i8_type llc
  | String -> i8_type llc |> pointer_type
  | ArrayType t -> aux t (* we just return the type of the elements *)
  | CompoundType (_, [t])-> aux t
  | CompoundType (name, []) ->
    begin
      match get_custom_type name with
      | Some t -> t
      | None -> 
        begin
          match type_by_name llm name with
          | Some t -> t
          | None -> "type " ^ name ^ " doesn't exist!" |> failwith
        end
    end
  | CompoundType _ -> failwith "unimplemented12"
  | Box _ -> failwith "boxing unimplemented" 
  | RefType (t,_) -> aux t |> pointer_type
  | GenericType _ -> failwith "generic types unimplemented"
  in
  aux t


let getLLVMValue (l:literal) (llvm:llvm_args) : llvalue =
  match l with
  | LBool b -> const_int (i1_type llvm.c) (Bool.to_int b)
  | LInt i -> const_int (i32_type llvm.c) i
  | LFloat f -> const_float (double_type llvm.c) f
  | LChar c -> const_int (i8_type llvm.c) (Char.code c)
  | LString s -> build_global_stringptr  s ".str" llvm.b