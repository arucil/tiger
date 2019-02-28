open Base
open Common
open Ir

module A = Assem

let stmt_gen stmt temp_store =
  let instrs = ref [] in
  let emit instr =
    instrs := instr :: !instrs
  in

  let rec munch_stmt = function
    | Seq (s1, s2) -> (munch_stmt s1; munch_stmt s2)

  and result f =
    let dst = Temp.new_temp temp_store in
    f dst;
    dst

  and munch_expr = function
    | Binop (Add, Const n1, Const n2) ->
      munch_expr (Const (n1 + n2))
    | Binop (Add, e1, Const n) | Binop (Add, Const n, e1) ->
      result (fun dst ->
        if n >= -32768 && n <= 32768 then
          let src = munch_expr e1 in
          emit (A.Oper {
            assem = A.make_mnemonic "addiu 'd0, 's0, %d" n;
            src = [|src|];
            dst = [|dst|];
            jumps = [||]
          })
        else
          begin
            let src1 = munch_expr e1 in
            let src2 = munch_expr (Const n) in
            emit (A.Oper {
              assem = A.make_mnemonic "addu 'd0, 's0, 's1";
              src = [|src1; src2|];
              dst = [|dst|];
              jumps = [||]
            })
          end)
    | Binop (Add, e1, e2) ->
      result (fun dst ->
        let src1 = munch_expr e1 in
        let src2 = munch_expr e2 in
        emit (A.Oper {
          assem = A.make_mnemonic "addu 'd0, 's0, 's1";
          src = [|src1; src2|];
          dst = [|dst|];
          jumps = [||]
        }))
    | Binop (Sub, Const n1, Const n2) ->
      munch_expr (Const (n1 - n2))
    | Binop (Sub, e1, Const n) ->
      result (fun dst ->
        let src1 = munch_expr e1 in
        if n >= -32767 && n <= 32768 then
          emit (A.Oper {
            assem = A.make_mnemonic "addiu 'd0, 's0, %d" (-n);
            src = [|src1|];
            dst = [|dst|];
            jumps = [||];
          })
        else
          let src2 = munch_expr (Const n) in
          emit (A.Oper {
            assem = A.make_mnemonic "subu 'd0, 's0, 's1";
            src = [|src1; src2|];
            dst = [|dst|];
            jumps = [||]
          }))
    | Binop (Sub, e1, e2) ->
      result (fun dst ->
        let src1 = munch_expr e1 in
        let src2 = munch_expr e2 in
        emit (A.Oper {
          assem = A.make_mnemonic "subu 'd0, 's0, 's1";
          src = [|src1; src2|];
          dst = [|dst|];
          jumps = [||]
        }))
    | Binop (Mul, Const n1, Const n2) ->
      munch_expr (Const (n1 * n2))
    | Binop (Mul, e1, e2) ->
      result (fun dst ->
        let src1 = munch_expr e1 in
        let src2 = munch_expr e2 in
        emit (A.Oper {
          assem = A.make_mnemonic "mult 's0, 's1";
          src = [|src1; src2|];
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
      if n2 <> 0 then
        munch_expr (Const (n1 / n2))
      else
        div_expr (Const n1) (Const n2)
    | Binop (Div, e1, e2) ->
      div_expr e1 e2
    | Binop (And, e1, Const n) | Binop (And, Const n, e1) ->
      if n = 0 then
        Temp.temp_of_int 0
      else
        result (fun dst ->
          let src1 = munch_expr e1 in
          if n >= 0 && n <= 65535 then
            emit (A.Oper {
              assem = A.make_mnemonic "andi 'd0, 's0, %d" n;
              src = [|src1|];
              dst = [|dst|];
              jumps = [||];
            })
          else
            let src2 = munch_expr (Const n) in
            emit (A.Oper {
              assem = A.make_mnemonic "and 'd0, 's0, 's1";
              src = [|src1; src2|];
              dst = [|dst|];
              jumps = [||];
            }))
    | Binop (And, e1, e2) ->
      result (fun dst ->
        let src1 = munch_expr e1 in
        let src2 = munch_expr e2 in
        emit (A.Oper {
          assem = A.make_mnemonic "and 'd0, 's0, 's1";
          src = [|src1; src2|];
          dst = [|dst|];
          jumps = [||];
        }))
    | Binop (Or, e1, Const n) | Binop (Or, Const n, e1) ->
      result (fun dst ->
        let src1 = munch_expr e1 in
        if n >= 0 && n <= 65535 then
          emit (A.Oper {
            assem = A.make_mnemonic "ori 'd0, 's0, %d" n;
            src = [|src1|];
            dst = [|dst|];
            jumps = [||];
          })
        else
          let src2 = munch_expr (Const n) in
          emit (A.Oper {
            assem = A.make_mnemonic "or 'd0, 's0, 's1";
            src = [|src1; src2|];
            dst = [|dst|];
            jumps = [||];
          }))
    | Binop (Or, e1, e2) ->
      result (fun dst ->
        let src1 = munch_expr e1 in
        let src2 = munch_expr e2 in
        emit (A.Oper {
          assem = A.make_mnemonic "or 'd0, 's0, 's1";
          src = [|src1; src2|];
          dst = [|dst|];
          jumps = [||];
        }))
    | Binop (Lshift, e1, Const n) ->
      result (fun dst ->
        let src1 = munch_expr e1 in
        emit (A.Oper {
          assem = A.make_mnemonic "sll 'd0, 's0, %d" (n land 31);
          src = [|src1|];
          dst = [|dst|];
          jumps = [||]
        }))
    | Binop (Lshift, e1, e2) ->
      result (fun dst ->
        let src1 = munch_expr e1 in
        let src2 = munch_expr e2 in
        emit (A.Oper {
          assem = A.make_mnemonic "sllv 'd0, 's0, 's1";
          src = [|src1; src2|];
          dst = [|dst|];
          jumps = [||]
        }))
    | Binop (Rshift, e1, Const n) ->
      result (fun dst ->
        let src1 = munch_expr e1 in
        emit (A.Oper {
          assem = A.make_mnemonic "srl 'd0, 's0, %d" (n land 31);
          src = [|src1|];
          dst = [|dst|];
          jumps = [||]
        }))
    | Binop (Rshift, e1, e2) ->
      result (fun dst ->
        let src1 = munch_expr e1 in
        let src2 = munch_expr e2 in
        emit (A.Oper {
          assem = A.make_mnemonic "srlv 'd0, 's0, 's1";
          src = [|src1; src2|];
          dst = [|dst|];
          jumps = [||]
        }))
    | Binop (Arshift, e1, Const n) ->
      result (fun dst ->
        let src1 = munch_expr e1 in
        emit (A.Oper {
          assem = A.make_mnemonic "sra 'd0, 's0, %d" (n land 31);
          src = [|src1|];
          dst = [|dst|];
          jumps = [||]
        }))
    | Binop (Arshift, e1, e2) ->
      result (fun dst ->
        let src1 = munch_expr e1 in
        let src2 = munch_expr e2 in
        emit (A.Oper {
          assem = A.make_mnemonic "srav 'd0, 's0, 's1";
          src = [|src1; src2|];
          dst = [|dst|];
          jumps = [||]
        }))
    | Name label ->
      result (fun dst ->
        emit (A.Oper {
          assem = A.make_mnemonic "la 'd0, %s" (Temp.show_label label);
          src = [||];
          dst = [||];
          jumps = [||]
        }))
    | Unop (Neg, Const n) ->
      munch_expr (Const (-n))
    | Unop (Neg, e1) ->
      result (fun dst ->
        let src1 = munch_expr e1 in
        emit (A.Oper {
          assem = A.make_mnemonic "subu 'd0, $0, 's0";
          src = [|src1|];
          dst = [|dst|];
          jumps = [||]
        }))
    | Unop (Not, Const n) ->
      munch_expr (Const (lnot n))
    | Unop (Not, e1) ->
      result (fun dst ->
        let src1 = munch_expr e1 in
        emit (A.Oper {
          assem = A.make_mnemonic "nor 'd0, $0, 's0";
          src = [|src1|];
          dst = [|dst|];
          jumps = [||]
        }))
    | Lval (Temp r) -> r
    | Call _ | Eseq _ -> Utils.Exn.unreachable ()
    | Const n ->
      if n = 0 then
        Temp.temp_of_int 0
      else
        result (fun dst ->
          if n > 0 && n <= 65535 then
            emit (A.Oper {
              assem = A.make_mnemonic "ori 'd0, 'd0, %d" n;
              src = [|dst|];
              dst = [|dst|];
              jumps = [||]
            })
          else if n >= -32768 && n <= 32767 then
            emit (A.Oper {
              assem = A.make_mnemonic "addiu 'd0, 'd0, %d" n;
              src = [|dst|];
              dst = [|dst|];
              jumps = [||]
            })
          else
            begin
              emit (A.Oper {
                assem = A.make_mnemonic "lui 'd0, 'd0, %d" (n lsr 16 land 0xffff);
                src = [|dst|];
                dst = [|dst|];
                jumps = [||]
              });
              emit (A.Oper {
                assem = A.make_mnemonic "ori 'd0, 'd0, %d" (n land 0xffff);
                src = [|dst|];
                dst = [|dst|];
                jumps = [||]
              })
            end)

  and div_expr e1 e2 =
    result (fun dst ->
      let src1 = munch_expr e1 in
      let src2 = munch_expr e2 in
      emit (A.Oper {
        assem = A.make_mnemonic "divu 's0, 's1";
        src = [|src1; src2|];
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

  in
  munch_stmt stmt;
  List.rev !instrs

let gen stmts temp_store =
  List.map stmts ~f:(fun stmt -> stmt_gen stmt temp_store)
    |> List.concat