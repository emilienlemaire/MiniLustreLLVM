open Ast_lib.Imp_ast

module MexprOp = struct
    let make_expr expr desc =
        { expr with
            mexpr_desc = desc
        }

    let (>>=) = make_expr
end
