open Core

module List = struct
  include Core.List
  include Xlist 
end

module Sexp = struct
  include Core.Sexp
  include Xsexp
end

let extract = Seplist.values
let printf = Printf.printf
let sprintf = Printf.sprintf
