open Common
open TypesCommon
open IrHir
open SailParser
open ProcessUtils
module E = Error.Logger
open ProcessMonad
open Monad.UseMonad(M)

module Pass = Pass.Make(struct
  let name = "Process to Methods"
  type in_body = (HirUtils.statement,(HirUtils.statement,HirUtils.expression) AstParser.process_body) SailModule.methods_processes
  type out_body = in_body

  let transform (sm:in_body SailModule.t) : out_body SailModule.t E.t = 
    let lower_processes (procs : _ AstParser.process_body process_defn list) : _ method_defn E.t =

      let rec browse args closed (p : (HirUtils.statement,HirUtils.expression) AstParser.process_body process_defn)  : 'a M.t = 
        let closed = p.p_name::closed in (* no cycles *)
    
        (* add process local var decls *)
        ListM.iter (fun (id,ty) -> M.write_decls M.HirSyntax.(var (dummy_pos,id,ty))) p.p_body.decls >> fun () -> 

        ListM.iter2 (fun (p:param) arg -> 
          
          (* add process parameters to the decls *)
          M.write_decls M.HirSyntax.(var (p.loc,p.id,p.ty)) >> fun () ->

          (* assign them *)
          M.write_init M.HirSyntax.(!@(p.id) = arg)

        ) (fst p.p_interface) args >> fun () -> 

        

          
        (* add var for choosing which block to execute *)
        let id = Fmt.str "_%s__i" p.p_name in 
        M.(write_decls HirSyntax.(var (dummy_pos,id,Int 32))) >> fun () ->


        (* add process init *)
        let _,init = p.p_body.init in 
        M.write_init init >> fun () -> 
    

        (* inline process calls *)
        let* children,closed,nproc = ListM.fold_left ( fun (ch,cl,n) b -> 
          let+ p,cl' = lower_body M.HirSyntax.(!@id,n) cl b in 
          (match p with None -> ch | Some p -> p::ch), cl@cl', n+1
        ) ([],closed,0) p.p_body.loop in
    

        (* incr var modulo nproc *)
        let+ () = M.(write_loop HirSyntax.(!@id = (!@id + !1) % !nproc)) in
    
        P (p,children),closed
    
      and lower_body ((v,nproc) : _ * int) (closed:string list) =
        let open AstParser in function
        | _,Statement s -> 
          (* *)
          M.(write_loop HirSyntax.(_if (!nproc == v) s skip)) >> fun () -> 
          return (None,[])
        | _,Run ((l,id),args,_sv) -> 
          (* *)
          M.throw_if Error.(make l "not allowed to call Main process explicitely") (id = Constants.main_process) >> fun () ->
          M.throw_if Error.(make l "not allowed to have recursive process") (List.mem id closed) >> fun () ->
          M.throw_if_none Error.(make l @@ Fmt.str "unknown process %s" id) (List.find_opt (fun p -> p.p_name = id) procs) >>=
          browse args closed >>| fun (x,cl) -> Some x,cl
        
      in
    
      let open Monad.MonadOperator(E) in
      (
        let open Monad.MonadSyntax(M) in
        let* m = 
        M.throw_if_none (Error.make dummy_pos "need main process") 
              (List.find_opt (fun p -> p.p_name = Constants.main_process) procs) 
        in 
        let+ _t = browse [] [] m in m
      ) |> M.run >>| finalize
  
  in
  let open Monad.UseMonad(E) in
  let+ main = lower_processes sm.body.processes in
  let body : in_body =  { methods = main::sm.body.methods; processes = []} in 
  {sm with body}
end)