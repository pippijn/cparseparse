open Camlp4

module Id : Sig.Id = struct
  let name = "pa_unpack"
  let version = "0.1"
end

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig
  module Expander = Expander.Make(Syntax)
  include Syntax

  EXTEND Gram
    expr: LEVEL "top" [[
      "unpack"; template = STRING -> Expander.unpack _loc template
      | "pack"; template = STRING -> Expander.  pack _loc template
    ]];
  END
end

module M = Register.OCamlSyntaxExtension(Id)(Make)
