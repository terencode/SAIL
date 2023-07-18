open Error
open Monad
open TypesCommon

open MonadFunctions(Logger)
open MonadSyntax(Logger)
open MonadOperator(Logger)


module type Pass = sig
  val name : string
  type input
  type output

  val transform : input -> output
end


module type ModulePass = sig
  type in_body
  type out_body  

  include Pass with type input := in_body SailModule.t and type output := out_body SailModule.t Logger.t
end



module type S = sig
  type in_body
  type out_body
  include Pass with type input := in_body SailModule.t Logger.t and type output := out_body SailModule.t Logger.t
end



module Progression = struct 
  type (_,_) t = 
  | Transform : ('a -> 'b) * ('b,'c) t -> ('a, 'c) t
  | Done : ('a,'a) t

  let run p i =
    let rec aux : type a b. (a,b) t -> a -> b = fun pipeline input ->
    match pipeline with
    | Done -> input
    | Transform (f,tail) -> aux tail (f input)
    in aux p i

  let finish = Done
  
  let (@>) f pipeline = Transform (f,pipeline) 
end




module Make (T: ModulePass) : S with type in_body = T.in_body and type out_body = T.out_body  = 
struct
  let name = T.name
  type in_body = T.in_body
  type out_body = T.out_body
  let transform (sm: T.in_body SailModule.t Logger.t) : T.out_body SailModule.t Logger.t = 
    let* sm = sm |> Logger.fail in 
    Logs.info (fun m -> m "Lowering module '%s' to '%s'" sm.md.name name);
    T.transform sm
end


type body_type = BMethod | BProcess


type 'a function_type = 
{
  name : string;
  body : 'a;
  ret : sailtype option;
  generics : string list;
  bt : body_type;
  pos : loc;
}


let function_type_of_process p = 
  {
      name=p.p_name;
      body=snd p.p_body.init;
      pos=p.p_pos;
      ret=None;
      bt=BProcess;
      generics=p.p_generics
  }

let function_type_of_method m  = 
  Either.map_right (fun b -> {
    name=m.m_proto.name;
    body=b;
    pos=m.m_proto.pos;
    ret=m.m_proto.rtype;
    bt=BMethod;
    generics=m.m_proto.generics
  }) m.m_body


module MakeFunctionPass 
  (V : Env.Variable) 
  (T: 
    sig 
    val name : string 
    type in_body 
    type out_body 
    val lower_function : in_body function_type -> SailModule.SailEnv(V).t -> in_body SailModule.methods_processes SailModule.t -> (out_body * SailModule.SailEnv(V).D.t) Logger.t 
    val preprocess : in_body SailModule.methods_processes SailModule.t -> in_body SailModule.methods_processes SailModule.t Logger.t
    end)  
  : S with type in_body := T.in_body SailModule.methods_processes and type out_body = T.out_body SailModule.methods_processes = 
struct
  let name = T.name
  type out_body = T.out_body SailModule.methods_processes

  module VEnv = SailModule.SailEnv(V)

  let lower_method (m:T.in_body method_defn) (sm : T.in_body SailModule.methods_processes SailModule.t) : (VEnv.D.t  * T.out_body method_defn) Logger.t = 
    let start_env = VEnv.get_start_env sm.declEnv m.m_proto.params in
    let decl = function_type_of_method m in
    match decl with
    | Right f -> 
      let* ve = start_env in
      let+ b,d = T.lower_function f ve sm in 
      d,{ m with m_body=Either.right b }
    | Left x ->  Logger.pure (sm.declEnv,{ m with m_body = Left x})


  let lower_process (p: T.in_body process_defn) (sm : T.in_body SailModule.methods_processes SailModule.t) : (VEnv.D.t * T.out_body process_defn ) Logger.t  = 
    let start_env = VEnv.get_start_env sm.declEnv (fst p.p_interface) in
    let decl = function_type_of_process p in
    let* ve = start_env in
    let+ init,d = T.lower_function decl ve sm in
    let init = fst p.p_body.init,init in
    d,{ p with p_body={p.p_body with init}}



  let transform (sm :T.in_body SailModule.methods_processes SailModule.t Logger.t)  : T.out_body SailModule.methods_processes SailModule.t Logger.t =
    let* sm = sm >>= T.preprocess in
    Logs.info (fun m -> m "Lowering module '%s' to '%s'" sm.md.name name);
    (
    let* declEnv,methods = ListM.fold_left_map 
                          (fun declEnv methd -> lower_method methd {sm with declEnv}) 
                          sm.declEnv sm.body.methods |> Logger.recover (sm.declEnv,[]) 
    in
    let+ declEnv,processes = ListM.fold_left_map 
                              (fun declEnv proccess -> lower_process proccess {sm with declEnv}) 
                              declEnv sm.body.processes |> Logger.recover (sm.declEnv,[]) in

    { sm with body=SailModule.{processes; methods} ; declEnv }

    ) |> Logger.fail
end