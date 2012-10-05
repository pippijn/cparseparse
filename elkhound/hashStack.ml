module Make(T : Hashtbl.S) = struct
  type 'a t = {
    table : 'a T.t;
    mutable stack : T.key list;
  }


  let create size = {
    table = T.create size;
    stack = [];
  }


  let is_empty stack =
    stack.stack = []


  let push stack item =
    T.add stack.table item item;
    stack.stack <- item :: stack.stack


  let pop stack =
    let item = List.hd stack.stack in
    stack.stack <- List.tl stack.stack;
    T.remove stack.table item;
    item


  let find stack item =
    T.find stack.table item


end
