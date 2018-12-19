open Core
open Common
open Located
open Lambda_t

let event loc kind exp =
  Event {
    ev_loc = loc;
    ev_exp = exp;
    ev_kind = kind;
    ev_repr = None;
  }

let ev_before loc exp =
  event loc Ev_before exp

let ev_after loc after exp =
  event loc (Ev_after after) exp

let ev_fun loc exp =
  event loc Ev_fun exp
