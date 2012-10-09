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


  let push key ?(value=key) stack =
    T.add stack.table key value;
    stack.stack <- key :: stack.stack


  let pop stack =
    let key = List.hd stack.stack in
    stack.stack <- List.tl stack.stack;
    T.remove stack.table key;
    key


  let mem stack key =
    T.mem stack.table key


  let find stack key =
    T.find stack.table key


end
