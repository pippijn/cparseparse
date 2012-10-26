

module Loc = struct

  module Map = Map.Make(struct
    type t = Ident.uident * Ident.uident
    let compare = Pervasives.compare
  end)

end
