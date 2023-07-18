open Common
open SailModule
open TypesCommon
module E = Common.Error.Logger
open Monad.UseMonad(E)
open Mono
open IrMir

(* temporary pass, converts Main process into a method, throws error if not found or other processes exist *)
module Pass = Pass.Make( struct
  let name = "Main Process to Method"
  type in_body = Monomorphization.Pass.out_body
  type out_body  = in_body

  let add_return (m:  AstMir.mir_function method_defn) : AstMir.mir_function method_defn = 
    let m_proto = {m.m_proto with rtype=Some (Int 32); name= "main"} in
    let m_body = match m.m_body with
    | Right (decls,cfg) ->
      let b = AstMir.BlockMap.find cfg.output cfg.blocks in
      (* hardcode "return 0" at the end *)
      let blocks = AstMir.BlockMap.add cfg.output 
        {b with terminator=Some (Return (Some {info=(dummy_pos,Int 32); exp=(Literal (LInt {l=Z.zero;size=32}))}))} cfg.blocks in
      Either.right (decls,{cfg with blocks})
    | Left _ ->  m.m_body
    in
    {m_proto;m_body}

  let transform (m : in_body SailModule.t)  : out_body SailModule.t E.t =
  let monomorphics = List.map (fun m -> if m.m_proto.name = "main" then add_return m else m) m.body.monomorphics in
  return { m with body = Monomorphization.{monomorphics; polymorphics = m.body.polymorphics} }
end
)