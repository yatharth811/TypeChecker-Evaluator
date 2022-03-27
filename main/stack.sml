signature STACK = sig

type 'a Stack
exception EmptyStack
exception Error of string
val create: unit -> 'a Stack
val push:  'a * 'a Stack -> 'a Stack
val pop : 'a Stack -> 'a Stack
val top : 'a Stack -> 'a
val empty: 'a Stack -> bool
val poptop : 'a Stack -> ('a * 'a Stack) option
val nth : 'a Stack * int -> 'a
val drop : 'a Stack * int -> 'a Stack
val depth : 'a Stack -> int
val app : ('a -> unit) -> 'a Stack -> unit
val map : ('a -> 'b) -> 'a Stack -> 'b Stack
val mapPartial : ('a -> 'b option) -> 'a Stack -> 'b Stack
val find : ('a -> bool) -> 'a Stack -> 'a option
val filter : ('a -> bool) -> 'a Stack -> 'a Stack
val foldr : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
val foldl : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
val exists : ('a -> bool) -> 'a Stack -> bool
val all : ('a -> bool) -> 'a Stack -> bool
val list2stack : 'a list -> 'a Stack 
val stack2list: 'a Stack -> 'a list 
val toString: ('a -> string) -> 'a Stack -> string

end