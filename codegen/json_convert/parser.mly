/*
 * Copyright (C) 2009      Citrix Ltd.
 * Author Prashanth Mundkur <firstname.lastname@citrix.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License (version
 * 2.1 only) as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 */

%{

open Syntax

let raise_syntax_error e pos =
  raise (Syntax_error (e, pos))

%}

/* keywords */
%token TYPE AND MUTABLE OF

%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK
%token EQUAL STAR SEMI SEMISEMI COLON BAR
%token EOF

%token <string> UIDENT LIDENT QIDENT
%token <char list> PRAGMA

%start defn_list

%type <Syntax.defn list> defn_list

%%

defn_list:
| defns EOF                     { List.rev $1 }
;

defns:
| defns top_defn_term           { $2 :: $1 }
| defns pragma                  { $2 :: $1 }
| /* epsilon */                 { [] }
;

pragma:
| PRAGMA                        { Pragma $1 }
top_defn_term:
| defn semi                     { Type_defn $1 }

defn:
| TYPE LIDENT                   { T_abstract $2 }
| TYPE eqn                      { T_manifest [ $2 ] }
| TYPE eqn AND defn_parts       { T_manifest ($2 :: (List.rev $4)) }

defn_parts:
| defn_parts AND eqn            { $3 :: $1 }
| eqn                           { [ $1 ] }

eqn:
| LIDENT EQUAL repn             { ($1, $3) }
;

semi:
| SEMISEMI                      {}
| /* epsilon */                 {}

repn:
| expr_or_tuple                 { $1 }
| record                        { C_record (List.rev $1) }
| variant                       { C_variant (List.rev $1) }

expr_or_tuple:
| expr                          { $1 }
| expr STAR tuple               { C_tuple ($1 :: (List.rev $3)) }

tuple:
| tuple STAR expr               { $3 :: $1 }
| expr                          { [ $1 ] }

expr:
| LPAREN expr_or_tuple RPAREN   { $2 }

| expr LIDENT
        { match $2 with
          | "option" -> C_option $1
          | "list"   -> C_list $1
          | "array"  -> C_array $1
          | s        -> (raise_syntax_error
                           (Unsupported_type_constructor s)
                           (Parsing.rhs_start_pos 2))
        }
| base                          { C_base $1 }

base:
| LIDENT        { match $1 with
                  | "string" -> B_string
                  | "int"    -> B_int
                  | "int64"  -> B_int64
                  | "float"  -> B_float
                  | "bool"   -> B_bool
                  | s        -> B_ident s
                }
| QIDENT        { B_ident $1 }

record:
| LBRACE field_decls opt_semi RBRACE    { $2 }

field_decls:
| field_decls SEMI field_decl   { $3 :: $1 }
| field_decl                    { [ $1 ] }

opt_semi:
| SEMI                          {}
| /* epsilon */                 {}

field_decl:
| LIDENT COLON expr_or_tuple            { ($1, $3) }
| MUTABLE LIDENT COLON expr_or_tuple    { ($2, $4) }

variant:
| variant BAR constr            { $3 :: $1 }
| constr                        { [ $1 ] }
| /* epsilon */                 { [] }

constr:
| UIDENT                        { CD_tuple ($1, []) }
| UIDENT OF expr                { CD_tuple ($1, [ $3 ]) }

| UIDENT OF expr STAR tuple     { CD_tuple ($1, ($3 :: (List.rev $5))) }

