(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

(*We added a string here*)
type typ = Int | Bool | Void | String | File | Float | Matrix1DType of typ * int | Matrix1DPointer of typ



type bind = typ * string

type expr =
    Literal of int
  | FloatLiteral of float
  | BoolLit of bool
  | StringSeq of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | PointerIncrement of string
  | Len of string
  | MatrixLiteral of expr list
  | Matrix1DAccess of string * expr
  | Call of string * expr list
  | Matrix1DReference of string
  | Dereference of string
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

type program = bind list * func_decl list

(* Pretty-printing functions *)
(*Olessya: this is not the part of the AST itself, but the function to print it out!!!!!!*)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

     (*Need to change this!!!!*)
let string_of_matrix m =
  let rec string_of_matrix_lit = function
      [] -> "]"
    | [hd] -> (match hd with
                Literal(i) -> string_of_int i
              | FloatLiteral(i) -> string_of_float i
              | BoolLit(i) -> string_of_bool i
              | Id(s) -> s
              | _ -> raise( Failure("Illegal expression in matrix literal") )) ^ string_of_matrix_lit []
    | hd::tl -> (match hd with
                    Literal(i) -> string_of_int i ^ ", "
                  | FloatLiteral(i) -> string_of_float i ^ ", "
                  | BoolLit(i) -> string_of_bool i ^ ", "
                  | Id(s) -> s
                  | _ -> raise( Failure("Illegal expression in matrix literal") )) ^ string_of_matrix_lit tl
  in
  "[" ^ string_of_matrix_lit m

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | FloatLiteral(i) -> string_of_float i
  | BoolLit(true) -> "true"
  | StringSeq(s) -> s
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  (* | Assign(v, e) -> v ^ " = " ^ string_of_expr e *)
  | Assign(r1, r2) -> (string_of_expr r1) ^ " =  " ^ (string_of_expr r2)
  | PointerIncrement(s) -> "++" ^ s
  | MatrixLiteral(m) -> string_of_matrix m
  | Matrix1DAccess(s, r1) -> s ^ "[" ^ (string_of_expr r1) ^ "]"
  | Noexpr -> ""
  | Len(s) -> "len(" ^ s ^ ")"
  | Matrix1DReference(s) -> "%" ^ s
  | Dereference(s) -> "#" ^ s
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
  | Float -> "float"
  | String -> "string"
  | File -> "file"
  | Matrix1DType(t, i1) -> string_of_typ t ^ "[" ^ string_of_int i1 ^ "]"
  | Matrix1DPointer(t) -> string_of_typ t ^ "[]"


let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
