open Common
open TypesCommon
open ProcessMonad
open Monad.UseMonad(M)
open IrHir
module E = Error.Logger


let method_of_process (p : 'a process_defn): 'a method_defn = 
  let m_proto = {pos=p.p_pos; name=String.lowercase_ascii p.p_name; generics = p.p_generics; params = fst p.p_interface; variadic=false; rtype=None} 
  and m_body = Either.right p.p_body in
  {m_proto;m_body}



let finalize (proc_def,(new_body: M.elt)) = 
  let open AstHir in
  let (++) = M.SeqMonoid.concat in 

  let main = method_of_process proc_def in 
  let m_body = 
    new_body.decls ++ 
    new_body.init ++ 
    buildStmt dummy_pos (Loop new_body.loop) 
    |> Either.right  
  in {main with m_body}

  

type 'a proc_tree = P of 'a process_defn * 'a proc_tree list
  