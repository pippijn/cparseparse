ast ListA {
List: Cons Integer List
      | Nil
Integer: int
}

ast ListB {
List: Cons Integer List
      | Nil
Integer: int
}

map double : ListA => ListB {
 List: Cons x xs => Cons x (Cons x xs)
}
