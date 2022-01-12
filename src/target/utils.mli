open Ast_lib.Imp_ast

module MexprOp : sig
    val make_expr: m_expr -> m_expr_desc -> m_expr

    val (>>=): m_expr -> m_expr_desc -> m_expr
end
