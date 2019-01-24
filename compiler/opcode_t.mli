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
  | Load_global
  | Store_pop_local of int
  | Store_pop_global

  | Return
  | Pop
  | No_match
  | Loophead
  | Jump of int ref
  | Branch of bool * int ref
  | Throw

  | Create_block of Block_tag.t * int (* tag, size *)
  | Get_field of Block_tag.t * int (* index *)
  | Set_field of Block_tag.t * int
  | Get_block_size of Block_tag.t 
  | Test_block of Block_tag.t 

  | Create_bitstr of Bitstr.spec
  | Load_bitstr of int * int (* size, value *)
  | Load_native_bitstr of int * int (* size, value *)
  | Get_bitstr of Bitstr.spec

  | Create_clos of int * int (* function at constant *)

  | Create_rec of int * int (* record name, field count *)
  | Update_rec of int (* field count *)

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
