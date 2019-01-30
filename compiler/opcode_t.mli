type t =
  | Nop
  | Ref of t ref (* placeholder *)

  | Load_local of int
  | Load_const of int
  | Load_int of int
  | Load_bool of bool
  | Load_atom of [`Undef | `Nil]
  | Load_ok of int
  | Load_error of int
  | Load_pid
  | Load_context
  | Load_self_fun
  | Store_pop_local of int

  | Get_global
  | Set_global

  | Return
  | Pop
  | No_match
  | Loophead
  | Jump of int ref
  | Branch of bool * int ref
  | Throw

  | Create_block of Block_tag.t * int (* tag, size *)
  | Get_block_field of int option (* index *)
  | Set_block_field of int option
  | Get_block_size
  | Test_block of Block_tag.t 

  | Create_bitstr of Bitstr.spec
  | Load_bitstr of int * int (* size, value *)
  | Load_native_bitstr of int * int (* size, value *)
  | Get_bitstr of Bitstr.spec

  | Clos of int (* function at constant *)

  | Create_rec of int * int (* record name, field count *)
  | Update_rec of int (* field count *)
  | Get_rec_field of int (* field name index *)

  | List_len
  | List_cons
  | List_concat
  | List_sub

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

  | Test_nonnil
