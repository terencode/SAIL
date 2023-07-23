open Common
open TypesCommon
open IrHir

module M = struct
  open AstHir


  module E =  Error.Logger

  module SeqMonoid = struct

    type t = {decls : HirUtils.statement; init : HirUtils.statement ; loop : HirUtils.statement}

    let empty = {info=dummy_pos ; stmt=Skip}
    let concat s1 s2 = match s1.stmt,s2.stmt with
    | Skip, Skip -> empty
    | Skip,stmt | stmt,Skip -> {info=dummy_pos ; stmt}
    | _ -> {info=dummy_pos ; stmt=Seq (s1,s2)}

    let mempty = {decls=empty; init=empty ; loop=empty}
    let mconcat s1 s2 = {decls=concat s1.decls s2.decls; init = concat s1.init s2.init ; loop = concat s1.loop s2.loop}

  end

  module EW =  MonadWriter.MakeTransformer(E)(SeqMonoid)
  
  open Monad.UseMonad(EW)

  include EW

  let throw_if_none b e = E.throw_if_none b e |> lift

  let write_decls decls = EW.write SeqMonoid.{decls; init=empty ; loop=empty}
  let write_init init = EW.write SeqMonoid.{decls=empty; init; loop=empty}
  let write_loop loop = EW.write SeqMonoid.{decls=empty; loop; init=empty}

  module HirSyntax = struct
    let skip = buildStmt dummy_pos Skip

    let (=) = fun l r -> buildStmt dummy_pos (Assign (l,r))

    let var (loc,id,ty) = buildStmt loc (DeclVar (true,id,Some ty,None))


    let (+) = fun l r -> buildExp dummy_pos (BinOp(Plus,l,r))
    let (%) = fun l r -> buildExp dummy_pos (BinOp(Rem,l,r))
    let (==) = fun l r -> buildExp dummy_pos (BinOp(Eq, l,r))
    
    let (&&) = fun s1 s2 -> buildStmt dummy_pos (Seq (s1,s2))


    let (!@) = fun id -> buildExp dummy_pos (Variable id)

    let (!) = fun n -> buildExp dummy_pos (Literal (LInt {l=Z.of_int n; size=32}))

    let _if cond _then _else = buildStmt dummy_pos (If (cond,_then,Some _else))


  end

  let throw_if b e = E.throw_if b e |> lift 


  let run : 'a t -> ('a * 'b) E.t = fun x -> E.bind x E.pure

end