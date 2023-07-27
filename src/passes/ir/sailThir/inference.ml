open Common
open TypesCommon
open IrHir

module HM = struct
  type ty = T of sailtype | Unknown of string

  type ('a,'b) constrnt = Eq of (ty,'b) AstHir.expression * (ty,'b) AstHir.expression
  type ('a,'b) constrnts = ('a,'b) constrnt list
  type env = ty FieldMap.t

  let mk_cstrnt t1 t2 = Eq (t1,t2)

  
  let reduce (Eq (e1,e2) : ('a,'b) constrnt) sol : ('a,'b) constrnts = 
    match (e1.info,e2.info) with
    | Unknown s1, Unknown s2 -> []
    | T t,Unknown s | Unknown s,T t -> []
    | T t1, T t2 -> []
    
  let solve constrnts =
    let rec aux c s =
      match c with
      | [] ->  ()
      | eq::t -> let c',s' = reduce eq s in 
    in 
    aux constrnts []

end




module Test = HM(
  struct
    type exp = LInt of int | LFloat of float | Plus of exp * exp 
    type ty = Int | Float 
    type id = string
    let compare_id = String.compare

    let rec eq e1 e2 = match (e1,e2) with 
      | LInt _, LInt _ | LFloat _, LFloat _ -> true
      | Plus (l,r),Plus (l',r') -> eq l r && eq l' r'
      | _ -> false

    let rec ty_of = function
    | LInt _ -> Some Int 
    | LFloat _ -> Some Float 
    | Plus (e1,_e2) -> ty_of e1


  end
)

(*
  env : string -> type
  t : literal -> type
  c : literal
  e : exp

  env |- c: t(c) -| {}
  env |- id : env(id) -| {}
  env |- e1 (+,-,*,/) e2 : t(e1) -| {t(e1) = t(e2)} 
  env |- e1 % e2 : int -| {t(e1) = t(e2), t(e1) = int, t(e2) = int}
  env |- e1 (==,!=,<,>,<=,>=) e2 : bool -| {t(e1) = t(e2)}
  env |- if e1 then e2 else e3 : ? -| 

*)



