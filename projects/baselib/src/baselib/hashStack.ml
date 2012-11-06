module Make(T : Hashtbl.S) = struct
  type 'a t = {
    table : 'a T.t;
    mutable stack : (T.key * 'a) list;
  }


  let create size = {
    table = T.create size;
    stack = [];
  }


  let is_empty stack =
    stack.stack = []


  let push key value stack =
    T.add stack.table key value;
    stack.stack <- (key, value) :: stack.stack


  let pop stack =
    let key, value = List.hd stack.stack in
    stack.stack <- List.tl stack.stack;
    T.remove stack.table key;
    value


  let mem stack key =
    T.mem stack.table key


  let find stack key =
    T.find stack.table key


end
