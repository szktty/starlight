type t =
  | Nop
  | Ref of t ref (* placeholder *)

  | Load_local of int
  | Load_const of int
  | Load_int of int
  | Load_true
  | Load_false
  | Load_undef
  | Load_ok
  | Load_error
  | Load_empty of Block_tag.t
  | Load_pid
  | Load_context
  | Load_self_fun
  | Load_bitstr of int * int (* size, value *)
  | Load_native_bitstr of int * int (* size, value *)
  | Store_pop_local of int

  | Get_field of int (* index *)
  | Get_prop
  | Get_global
  | Get_bitstr of Bitstr.spec
  | Set_field of int
  | Set_global

  | Return
  | Return_true
  | Return_false
  | Return_ok
  | Return_error
  | Return_undef
  | Pop

  | No_match
  | Loophead
  | Jump of int ref
  | Branch_true of int ref
  | Branch_false of int ref
  | Throw
  | Make_block of Block_tag.t * int (* tag, size *)
  | Make_bitstr of Bitstr.spec
  | Make_fun of int * int (* function at constant *)
  | Make_ok of int
  | Make_error of int

  | Apply of int
  | Spawn
  | Not
  | Eq
  | Ne
  | Lt
  | Add
  | Add1
  | Sub
  | Mul
  | Rem
  | Block_size
  | List_len
  | List_cons
  | List_concat
  | List_sub

  | Test_tuple
  | Test_nonnil
