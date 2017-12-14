(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "MicroC"
  and i8_t   = L.i8_type   context
  and i32_t  = L.i32_type  context
  and str_t  = L.pointer_type (L.i8_type context)
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context in
  let void_ptr =  L.pointer_type (L.i8_type context) in
(*
  let func_ptr = L.pointer_type (L.function_type (L.void_type context) [| L.i32_type  context; L.i32_type  context |] in
*)

  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Bool -> i1_t
    | A.String -> str_t
    | A.Void -> void_t
    | A.File -> void_ptr in


  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Declare the built-in printbig() function *)
  let printbig_t = L.function_type i32_t [| i32_t |] in
  let printbig_func = L.declare_function "printbig" printbig_t the_module in

  let open_t = L.function_type void_ptr [| str_t; str_t|] in
  let open_func = L.declare_function "open" open_t the_module in

  let readFile_t = L.function_type str_t [| void_ptr; i32_t|] in
  let readFile_func = L.declare_function "readFile" readFile_t the_module in

  let isFileEnd_t = L.function_type i1_t [| void_ptr |] in
  let isFileEnd_func = L.declare_function "isFileEnd" isFileEnd_t the_module in

  let close_t = L.function_type i32_t [| void_ptr; void_ptr|] in
  let close_func = L.declare_function "close" close_t the_module in

  let strstr_t = L.function_type str_t [| str_t; str_t |] in
  let strstr_func = L.declare_function "strstr" strstr_t the_module in

  let miniMap_t = L.function_type i32_t [| void_ptr; L.pointer_type (L.function_type (i32_t) [| L.i32_type  context; L.i32_type  context |] )|] in
  let miniMap_func = L.declare_function "miniMap" miniMap_t the_module in




  (* let printstring_t = L.var_arg_function_type str_t [| L.pointer_type i8_t |] in
  let printstring_func = L.declare_function "printstring" printstring_t the_module in *)

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types =
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let str_format_str = L.build_global_stringptr "%s" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = L.set_value_name n p;
	let local = L.build_alloca (ltype_of_typ t) n builder in
	ignore (L.build_store p local builder);
	StringMap.add n local m in

      let add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.A.locals in

    (* Return the value for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found ->
                   StringMap.find n global_vars
    in
(*
                   with Not_found -> match
                   StringMap.find n function_decls with
                   |(y,_)-> ignore(build_alloca    builder);
(*build_alloca ty name b creates a %name = alloca %ty instruction at the position specified by the instruction builder b.*)
(*build_store v p b creates a store %v, %p instruction at the position specified by the instruction builder b.*)

*)


    (* Construct code for an expression; return its value *)
    let rec expr builder = function
        A.Literal i -> L.const_int i32_t i
      | A.StringSeq str -> L.build_global_stringptr str "tmp" builder
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id s -> L.build_load (lookup s) s builder
      | A.Binop (e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
	  (match op with
	    A.Add     -> L.build_add
	  | A.Sub     -> L.build_sub
	  | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
	  | A.And     -> L.build_and
	  | A.Or      -> L.build_or
	  | A.Equal   -> L.build_icmp L.Icmp.Eq
	  | A.Neq     -> L.build_icmp L.Icmp.Ne
	  | A.Less    -> L.build_icmp L.Icmp.Slt
	  | A.Leq     -> L.build_icmp L.Icmp.Sle
	  | A.Greater -> L.build_icmp L.Icmp.Sgt
	  | A.Geq     -> L.build_icmp L.Icmp.Sge
	  ) e1' e2' "tmp" builder
      | A.Unop(op, e) ->
	  let e' = expr builder e in
	  (match op with
	    A.Neg     -> L.build_neg
          | A.Not     -> L.build_not) e' "tmp" builder
      | A.Assign (s, e) -> let e' = expr builder e in
	                   ignore (L.build_store e' (lookup s) builder); e'
      | A.Call ("print", [e]) | A.Call ("printb", [e]) ->
	  L.build_call printf_func [| int_format_str ; (expr builder e) |]
	    "printf" builder
      | A.Call ("printbig", [e]) ->
	  L.build_call printbig_func [| (expr builder e) |] "printbig" builder
      |  A.Call ("printstring", [e]) ->
      L.build_call printf_func [| str_format_str; (expr builder e) |]
        "printf" builder
      |  A.Call ("strstr", [e1;e2]) ->
        L.build_call strstr_func [| (expr builder e1); (expr builder e2)|]
          "strstr" builder
    | A.Call ("open", [e1;e2]) ->
    L.build_call open_func [| (expr builder e1);(expr builder e2)|] "open" builder
    | A.Call ("readFile", [e1;e2]) ->
    L.build_call readFile_func [| (expr builder e1); (expr builder e2)|] "readFile" builder
    | A.Call ("isFileEnd", [e1]) ->
    L.build_call isFileEnd_func [| (expr builder e1)|] "isFileEnd" builder
    | A.Call ("close", [e1;e2]) ->
    L.build_call close_func [| (expr builder e1); (expr builder e2)|] "close" builder
    | A.Call ("miniMap", [e1; A.Id(e2)]) ->
      let fileptr = expr builder e1 in


    let (fdef,_) = StringMap.find e2 function_decls in

     L.build_call miniMap_func [|fileptr; fdef |] "miniMap" builder

    | A.Call (f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	 let actuals = List.rev (List.map (expr builder) (List.rev act)) in
	 let result = (match fdecl.A.typ with A.Void -> ""
                                            | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (f builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
	    A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> ignore (match fdecl.A.typ with
	                A.Void -> L.build_ret_void builder
	                | _ -> L.build_ret (expr builder e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
	 let merge_bb = L.append_block context "merge" the_function in

	 let then_bb = L.append_block context "then" the_function in
	 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
	   (L.build_br merge_bb);

	 let else_bb = L.append_block context "else" the_function in
	 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
	   (L.build_br merge_bb);

	 ignore (L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb

      | A.While (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore (L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

	  let merge_bb = L.append_block context "merge" the_function in
	  ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb

      | A.For (e1, e2, e3, body) -> stmt builder
	    ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
