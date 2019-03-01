open Base
open Common
open Ir

module A = Assem

(* $v0-$v1, $t0-$t9, $a0-$a3, $at *)
let calldefs = Array.map [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;24;25|]
  ~f:Temp.temp_of_int

let stmt_gen stmt temp_store =
  let instrs = ref [] in
  let emit instr =
    instrs := instr :: !instrs
  in

  let nop () =
    emit (A.Oper {
      assem = A.make_mnemonic "nop";
      src = [||];
      dst = [||];
      jumps = [||]
    })
  in

  let is_unsigned = function
    | Ult | Ule | Ugt | Uge -> true
    | _ -> false
  in

  let rec munch_stmt = function
    | Seq _ -> Utils.Exn.unreachable ()
    | Move (Mem (Binop (Add, e1, Const n)), e2) | Move (Mem (Binop (Add, Const n, e1)), e2) ->
      emit (A.Oper {
        assem = A.make_mnemonic "sw 's0, %ld('s1)" n;
        src = [|munch_expr e2; munch_expr e1|];
        dst = [||];
        jumps = [||]
      });
      nop ()
    | Move (Mem e1, e2) ->
      emit (A.Oper {
        assem = A.make_mnemonic "sw 's0, 0('s1)";
        src = [|munch_expr e2; munch_expr e1|];
        dst = [||];
        jumps = [||]
      })
    | Move (Temp t, Const n) ->
      const_expr n t
    | Move (Temp t, Call (f, args)) ->
      call_stmt f args;
      emit (A.Move {
        assem = A.make_mnemonic "move 'd0, 's0";
        src = Mips_platf.rv;
        dst = t
      })
    | Move (Temp t, e1) ->
      emit (A.Oper {
        assem = A.make_mnemonic "move 'd0, 's0";
        src = [|munch_expr e1|];
        dst = [|t|];
        jumps = [||]
      })
    | Label label ->
      emit (A.Label {
        assem = A.make_mnemonic "        %s:" (Temp.show_label label);
        label
      })
    | Jump (Name label, _labels) ->
      emit (A.Oper {
        assem = A.make_mnemonic "j 'j0";
        src = [||];
        dst = [||];
        jumps = [|label|];
      });
      nop ()
    | Jump _ ->
      Utils.Exn.unreachable ()
    | Cjump (op, Const n1, Const n2, t, _) ->
      if (match op with
          | Eq -> Int32.(n1 = n2)
          | Ne -> Int32.(n1 <> n2)
          | Lt -> Int32.(n1 < n2)
          | Le -> Int32.(n1 <= n2)
          | Gt -> Int32.(n1 > n2)
          | Ge -> Int32.(n1 >= n2)
          | Ult -> Unsigned.UInt32.(compare (of_int32 n1) (of_int32 n2) < 0)
          | Ule -> Unsigned.UInt32.(compare (of_int32 n1) (of_int32 n2) <= 0)
          | Ugt -> Unsigned.UInt32.(compare (of_int32 n1) (of_int32 n2) > 0)
          | Uge -> Unsigned.UInt32.(compare (of_int32 n1) (of_int32 n2) >= 0))
        then
          (emit (A.Oper {
            assem = A.make_mnemonic "j 'j0";
            src = [||];
            dst = [||];
            jumps = [|t|];
          });
          nop ())
    | Cjump (Gt | Ge | Lt | Le as op, e1, Const 0l, t, f) ->
      let op =
        match op with
        | Gt -> "bgtz"
        | Ge -> "bgez"
        | Lt -> "bltz"
        | Le -> "blez"
        | _ -> Utils.Exn.unreachable ()
      in
      emit (A.Oper {
        assem = A.make_mnemonic "%s 's0, 'j0" op;
        src = [|munch_expr e1|];
        dst = [||];
        jumps = [|t; f|]
      });
      nop ()
    | Cjump (Gt | Ge | Lt | Le as op, Const 0l, e1, t, f) ->
      let op =
        match op with
        | Gt -> "bltz"
        | Ge -> "blez"
        | Lt -> "bgtz"
        | Le -> "bgez"
        | _ -> Utils.Exn.unreachable ()
      in
      emit (A.Oper {
        assem = A.make_mnemonic "%s 's0, 'j0" op;
        src = [|munch_expr e1|];
        dst = [||];
        jumps = [|t; f|]
      });
      nop ()
    | Cjump (Eq | Ne as op, e1, e2, t, f) ->
      let op =
        match op with
        | Eq -> "beq"
        | Ne -> "bne"
        | _ -> Utils.Exn.unreachable ()
      in
      emit (A.Oper {
        assem = A.make_mnemonic "%s 's0, 's1, 'j0" op;
        src = [|munch_expr e1; munch_expr e2|];
        dst = [||];
        jumps = [|t; f|];
      });
      nop ()
    | Cjump (Lt | Ult as op, e1, Const n, t, f) | Cjump (Gt | Ugt as op, Const n, e1, t, f) ->
      let is_unsigned = is_unsigned op in
      if Int32.(n >= -32768l && n <= 32767l) then
        begin
          let temp = Temp.new_temp temp_store in
          emit (A.Oper {
            assem = A.make_mnemonic "%s 'd0, 's0, %ld"
              (if is_unsigned then "sltiu" else "slti")
              n;
            src = [|munch_expr e1|];
            dst = [|temp|];
            jumps = [||]
          });
          beq false temp t f
        end
      else
        cjump_lt_or_le_stmt e1 (Const n) t f ~is_le:false ~is_unsigned
    | Cjump (Lt | Ult as op, e1, e2, t, f) | Cjump (Gt | Ugt as op, e2, e1, t, f) ->
      let is_unsigned = is_unsigned op in
      cjump_lt_or_le_stmt e1 e2 t f ~is_le:false ~is_unsigned
    | Cjump (Ge | Uge as op, e1, Const n, t, f) | Cjump (Le | Ule as op, Const n, e1, t, f) ->
      let is_unsigned = is_unsigned op in
      if Int32.(n >= -32768l && n <= 32767l) then
        begin
          let temp = Temp.new_temp temp_store in
          emit (A.Oper {
            assem = A.make_mnemonic "%s 'd0, 's0, %ld"
              (if is_unsigned then "sltiu" else "slti")
              n;
            src = [|munch_expr e1|];
            dst = [|temp|];
            jumps = [||]
          });
          beq true temp t f
        end
      else
        cjump_lt_or_le_stmt e1 (Const n) t f ~is_le:true ~is_unsigned
    | Cjump (Ge | Uge as op, e1, e2, t, f) | Cjump (Le | Ule as op, e2, e1, t, f) ->
      let is_unsigned = is_unsigned op in
      cjump_lt_or_le_stmt e1 e2 t f ~is_le:true ~is_unsigned
    | Expr (Call (f, args)) ->
      call_stmt f args
    | Expr _ -> ()

  and call_stmt f args =
    call_args 0 args;
    let arity = List.length args in
    let temps =
      Array.sub Mips_platf.reg_params ~pos:0 ~len:(Int.min arity 4)
    in
    (match f with
     | Name label ->
       emit (A.Oper {
         assem = A.make_mnemonic "jal %s" (Temp.show_label label);
         src = temps;
         dst = calldefs;
         jumps = [||]
       })
     | e ->
       emit (A.Oper {
         assem = A.make_mnemonic "jalr 's0, $ra";
         src = Array.concat [[|munch_expr e|]; temps];
         dst = calldefs;
         jumps = [||]
       }));
    nop ();
    if arity > 4 then
      emit (A.Oper {
        assem = A.make_mnemonic "addiu 's0, 's0, %d" ((arity - 4) * 4);
        src = [|Mips_platf.sp|];
        dst = [|Mips_platf.sp|];
        jumps = [||]
      })

  and call_args i = function
    | [] -> ()
    | arg::args ->
      begin
        let src = munch_expr arg in
        if i < 4 then
          emit (A.Move {
            assem = A.make_mnemonic "move 'd0, 's0";
            src = src;
            dst = Mips_platf.reg_params.(i)
          })
        else
          begin
            emit (A.Oper {
              assem = A.make_mnemonic "addiu 's0, 's0, -4";
              src = [|Mips_platf.sp|];
              dst = [|Mips_platf.sp|];
              jumps = [||]
            });
            emit (A.Oper {
              assem = A.make_mnemonic "sw 's0, 0('s1)";
              src = [|src; Mips_platf.sp|];
              dst = [||];
              jumps = [||]
            })
          end;
        call_args (i + 1) args
      end

  and cjump_lt_or_le_stmt e1 e2 t f ~is_le ~is_unsigned =
    let temp = Temp.new_temp temp_store in
    emit (A.Oper {
      assem = A.make_mnemonic "%s 'd0, 's0, 's1" (if is_unsigned then "sltu" else "slt");
      src = [|munch_expr e1; munch_expr e2|];
      dst = [|temp|];
      jumps = [||]
    });
    beq is_le temp t f

  and beq is_beq s tlabel flabel =
    emit (A.Oper {
      assem = A.make_mnemonic "%s 's0, $0, 'j0" (if is_beq then "beq" else "bne");
      src = [|s|];
      dst = [||];
      jumps = [|tlabel; flabel|]
    });
    nop ()

  and result f =
    let dst = Temp.new_temp temp_store in
    f dst;
    dst

  and munch_expr = function
    | Binop (Add, Const n1, Const n2) ->
      munch_expr (Const Int32.(n1 + n2))
    | Binop (Add, e1, Const n) | Binop (Add, Const n, e1) ->
      if Int32.(n >= -32768l && n <= 32767l) then
        result (fun dst ->
          emit (A.Oper {
            assem = A.make_mnemonic "addiu 'd0, 's0, %ld" n;
            src = [|munch_expr e1|];
            dst = [|dst|];
            jumps = [||]
          }))
      else
        add_expr e1 (Const n)
    | Binop (Add, e1, e2) ->
      add_expr e1 e2
    | Binop (Sub, Const n1, Const n2) ->
      munch_expr (Const Int32.(n1 - n2))
    | Binop (Sub, e1, Const n) ->
      if Int32.(n >= -32767l && n <= 32768l) then
        result (fun dst ->
          emit (A.Oper {
            assem = A.make_mnemonic "addiu 'd0, 's0, %ld" Int32.(-n);
            src = [|munch_expr e1|];
            dst = [|dst|];
            jumps = [||];
          }))
      else
        sub_expr e1 (Const n)
    | Binop (Sub, e1, e2) ->
      sub_expr e1 e2
    | Binop (Mul, Const n1, Const n2) ->
      munch_expr (Const Int32.(n1 * n2))
    | Binop (Mul, e1, e2) ->
      result (fun dst ->
        emit (A.Oper {
          assem = A.make_mnemonic "mult 's0, 's1";
          src = [|munch_expr e1; munch_expr e2|];
          dst = [||];
          jumps = [||]
        });
        emit (A.Oper {
          (* TODO:
            Cannot use mult/div in two instructions after mflo (on real MIPS machine),
            fill NOPs later to handle this
           *)
          assem = A.make_mnemonic "mflo 'd0";
          src = [||];
          dst = [|dst|];
          jumps = [||]
        }))
    | Binop (Div, Const n1, Const n2) ->
      if Int32.(n2 <> 0l) then
        munch_expr (Const Int32.(n1 / n2))
      else
        div_expr (Const n1) (Const n2)
    | Binop (Div, e1, e2) ->
      div_expr e1 e2
    | Binop (And, Const n1, Const n2) ->
      munch_expr (Const Int32.(n1 land n2))
    | Binop (And, e1, Const n) | Binop (And, Const n, e1) ->
      if Int32.(n = 0l) then
        Mips_platf.zero
      else if Int32.(n > 0l && n <= 65535l) then
        result (fun dst ->
          emit (A.Oper {
            assem = A.make_mnemonic "andi 'd0, 's0, %ld" n;
            src = [|munch_expr e1|];
            dst = [|dst|];
            jumps = [||];
          }))
      else
        and_expr e1 (Const n)
    | Binop (And, e1, e2) ->
      and_expr e1 e2
    | Binop (Or, Const n1, Const n2) ->
      munch_expr (Const Int32.(n1 lor n2))
    | Binop (Or, e1, Const n) | Binop (Or, Const n, e1) ->
      if Int32.(n >= 0l && n <= 65535l) then
        result (fun dst ->
          emit (A.Oper {
            assem = A.make_mnemonic "ori 'd0, 's0, %ld" n;
            src = [|munch_expr e1|];
            dst = [|dst|];
            jumps = [||];
          }))
      else
        or_expr e1 (Const n)
    | Binop (Or, e1, e2) ->
      or_expr e1 e2
    | Binop (Xor, Const n1, Const n2) ->
      munch_expr (Const Int32.(n1 lxor n2))
    | Binop (Xor, e1, Const n) | Binop (Xor, Const n, e1) ->
      if Int32.(n >= 0l && n <= 65535l) then
        result (fun dst ->
          emit (A.Oper {
            assem = A.make_mnemonic "xori 'd0, 's0, %ld" n;
            src = [|munch_expr e1|];
            dst = [|dst|];
            jumps = [||];
          }))
      else
        xor_expr e1 (Const n)
    | Binop (Xor, e1, e2) ->
      xor_expr e1 e2
    | Binop (Lshift, Const n1, Const n2) ->
      munch_expr (Const Int32.(n1 lsl (Int32.to_int_exn (n2 land 31l))))
    | Binop (Lshift, e1, Const n) ->
      result (fun dst ->
        emit (A.Oper {
          assem = A.make_mnemonic "sll 'd0, 's0, %ld" Int32.(n land 31l);
          src = [|munch_expr e1|];
          dst = [|dst|];
          jumps = [||]
        }))
    | Binop (Lshift, e1, e2) ->
      result (fun dst ->
        emit (A.Oper {
          assem = A.make_mnemonic "sllv 'd0, 's0, 's1";
          src = [|munch_expr e1; munch_expr e2|];
          dst = [|dst|];
          jumps = [||]
        }))
    | Binop (Rshift, Const n1, Const n2) ->
      munch_expr (Const Int32.(n1 lsr (Int32.to_int_exn (n2 land 31l))))
    | Binop (Rshift, e1, Const n) ->
      result (fun dst ->
        emit (A.Oper {
          assem = A.make_mnemonic "srl 'd0, 's0, %ld" Int32.(n land 31l);
          src = [|munch_expr e1|];
          dst = [|dst|];
          jumps = [||]
        }))
    | Binop (Rshift, e1, e2) ->
      result (fun dst ->
        emit (A.Oper {
          assem = A.make_mnemonic "srlv 'd0, 's0, 's1";
          src = [|munch_expr e1; munch_expr e2|];
          dst = [|dst|];
          jumps = [||]
        }))
    | Binop (Arshift, Const n1, Const n2) ->
      munch_expr (Const Int32.(n1 asr (Int32.to_int_exn (n2 land 31l))))
    | Binop (Arshift, e1, Const n) ->
      result (fun dst ->
        emit (A.Oper {
          assem = A.make_mnemonic "sra 'd0, 's0, %ld" Int32.(n land 31l);
          src = [|munch_expr e1|];
          dst = [|dst|];
          jumps = [||]
        }))
    | Binop (Arshift, e1, e2) ->
      result (fun dst ->
        emit (A.Oper {
          assem = A.make_mnemonic "srav 'd0, 's0, 's1";
          src = [|munch_expr e1; munch_expr e2|];
          dst = [|dst|];
          jumps = [||]
        }))
    | Unop (Neg, Const n) ->
      munch_expr (Const Int32.(-n))
    | Unop (Neg, e1) ->
      result (fun dst ->
        emit (A.Oper {
          assem = A.make_mnemonic "subu 'd0, $0, 's0";
          src = [|munch_expr e1|];
          dst = [|dst|];
          jumps = [||]
        }))
    | Unop (Not, Const n) ->
      munch_expr (Const Int32.(lnot n))
    | Unop (Not, e1) ->
      result (fun dst ->
        emit (A.Oper {
          assem = A.make_mnemonic "nor 'd0, $0, 's0";
          src = [|munch_expr e1|];
          dst = [|dst|];
          jumps = [||]
        }))
    | Lval (Temp r) -> r
    | Lval (Mem (Binop (Add, e1, Const n))) | Lval (Mem (Binop (Add, Const n, e1))) ->
      if Int32.(n >= -32768l && n <= 32767l) then
        result (fun dst ->
          emit (A.Oper {
            assem = A.make_mnemonic "lw 'd0, %ld('s0)" n;
            src = [|munch_expr e1|];
            dst = [|dst|];
            jumps = [||]
          });
          emit (A.Oper {
            assem = A.make_mnemonic "nop";
            src = [||];
            dst = [||];
            jumps = [||]
          }))
      else
        mem_expr (Binop (Add, e1, Const n))
    | Lval (Mem e) ->
      mem_expr e
    | Call _ | Eseq _ -> Utils.Exn.unreachable ()
    | Name label ->
      result (fun dst ->
        emit (A.Oper {
          assem = A.make_mnemonic "la 'd0, %s" (Temp.show_label label);
          src = [||];
          dst = [|dst|];
          jumps = [||]
        }))
    | Const n ->
      if Int32.(n = 0l) then
        Mips_platf.zero
      else
        result (const_expr n)

  and add_expr e1 e2 =
    result (fun dst ->
      emit (A.Oper {
        assem = A.make_mnemonic "addu 'd0, 's0, 's1";
        src = [|munch_expr e1; munch_expr e2|];
        dst = [|dst|];
        jumps = [||]
      }))

  and sub_expr e1 e2 =
    result (fun dst ->
      emit (A.Oper {
        assem = A.make_mnemonic "subu 'd0, 's0, 's1";
        src = [|munch_expr e1; munch_expr e2|];
        dst = [|dst|];
        jumps = [||]
      }))

  and div_expr e1 e2 =
    result (fun dst ->
      emit (A.Oper {
        assem = A.make_mnemonic "divu 's0, 's1";
        src = [|munch_expr e1; munch_expr e2|];
        dst = [||];
        jumps = [||]
      });
      emit (A.Oper {
        (* TODO:
          Cannot use mult/div in two instructions after mflo (on real MIPS machine),
          fill NOPs later to handle this
          *)
        assem = A.make_mnemonic "mflo 'd0";
        src = [||];
        dst = [|dst|];
        jumps = [||]
      }))

  and and_expr e1 e2 =
    result (fun dst ->
      emit (A.Oper {
        assem = A.make_mnemonic "and 'd0, 's0, 's1";
        src = [|munch_expr e1; munch_expr e2|];
        dst = [|dst|];
        jumps = [||];
      }))

  and or_expr e1 e2 =
    result (fun dst ->
      emit (A.Oper {
        assem = A.make_mnemonic "or 'd0, 's0, 's1";
        src = [|munch_expr e1; munch_expr e2|];
        dst = [|dst|];
        jumps = [||];
      }))

  and xor_expr e1 e2 =
    result (fun dst ->
      emit (A.Oper {
        assem = A.make_mnemonic "xor 'd0, 's0, 's1";
        src = [|munch_expr e1; munch_expr e2|];
        dst = [|dst|];
        jumps = [||];
      }))

  and mem_expr e =
    result (fun dst ->
      emit (A.Oper {
        assem = A.make_mnemonic "lw 'd0, 0('s0)";
        src = [|munch_expr e|];
        dst = [|dst|];
        jumps = [||]
      });
      nop ())

  and const_expr n dst =
    if Int32.(n > 0l && n <= 65535l) then
      emit (A.Oper {
        assem = A.make_mnemonic "ori 'd0, $0, %ld" n;
        src = [||];
        dst = [|dst|];
        jumps = [||]
      })
    else if Int32.(n >= -32768l && n <= 32767l) then
      emit (A.Oper {
        assem = A.make_mnemonic "addiu 'd0, $0, %ld" n;
        src = [||];
        dst = [|dst|];
        jumps = [||]
      })
    else
      begin
        emit (A.Oper {
          assem = A.make_mnemonic "lui 'd0, %ld" Int32.(n lsr 16 land 0xffffl);
          src = [||];
          dst = [|dst|];
          jumps = [||]
        });
        emit (A.Oper {
          assem = A.make_mnemonic "ori 'd0, 'd0, %ld" Int32.(n land 0xffffl);
          src = [|dst|];
          dst = [|dst|];
          jumps = [||]
        })
      end

  in
  munch_stmt stmt;
  List.rev !instrs

let gen stmts temp_store =
  List.map stmts ~f:(fun stmt -> stmt_gen stmt temp_store)
    |> List.concat