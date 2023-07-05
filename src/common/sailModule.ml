open TypesCommon
module E = Error.Logger

module Declarations = struct
  type process_decl = loc * function_proto
  type method_decl = l_str * function_proto 
  type struct_decl = loc * struct_proto
  type enum_decl = loc * enum_proto
  type type_decl = ty_defn
end

module DeclEnv = Env.DeclarationsEnv(Declarations)

module SailEnv = Env.VariableDeclEnv(Declarations) 


type 'a methods_processes  = {methods : 'a method_defn list ; processes : 'a process_defn list; }

type 'a t =
{
  declEnv: DeclEnv.t;
  builtins : method_sig list ;
  body : 'a;
  imports : ImportSet.t;
  md : metadata;
}

let emptyModule empty_content = 
{
  declEnv = DeclEnv.empty;
  builtins = [];
  body = empty_content;
  imports = ImportSet.empty;
  md = {
    name = String.empty; 
    version = String.empty;
    hash = String.empty; 
    libs = FieldSet.empty
  };
}

let method_decl_of_defn (d : 'a method_defn) : Declarations.method_decl = 
  let pos = d.m_proto.pos
  and name = (match d.m_body with Left (sname,_) -> sname | Right _ -> d.m_proto.name)
  and ret = d.m_proto.rtype 
  and args = d.m_proto.params 
  and generics = d.m_proto.generics 
  and variadic = d.m_proto.variadic in
  ((pos,name),{ret;args;generics;variadic})
  

let method_of_process (p : 'a process_defn): 'a method_defn = 
  let open Monad.MonadSyntax(E) in
  let m_proto = {pos=p.p_pos; name=String.lowercase_ascii p.p_name; generics = p.p_generics; params = fst p.p_interface; variadic=false; rtype=None} 
  and m_body = Either.right p.p_body in
  {m_proto;m_body}