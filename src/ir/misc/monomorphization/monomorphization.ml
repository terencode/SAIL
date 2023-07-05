open Common
open TypesCommon
module E = Common.Error.Logger
open Monad.UseMonad(E)
open IrMir.AstMir

type mono_body = {monomorphics : mir_function method_defn list; polymorphics : mir_function method_defn list;}

module Pass = Pass.Make (struct
  let name = "Monomorphization"

  type in_body = mir_function SailModule.methods_processes
  type out_body = mono_body

  module Env = SailModule.DeclEnv

  let transform (smdl : in_body SailModule.t) : out_body SailModule.t E.t =
    let polymorphics,monomorphics = List.partition (fun m -> m.m_proto.generics <> []) smdl.body.methods in
    let monomorphics = List.fold_left (fun acc p -> if p.p_name = "Main" then SailModule.method_of_process p::acc else acc) monomorphics smdl.body.processes in 
    return {smdl with body={monomorphics;polymorphics}}
end)