structure FunStack :> STACK =

struct
type 'a Stack = 'a list
exception EmptyStack
exception Error of string

fun create () = [];

fun push (x : 'a , l : 'a Stack ) = x :: l
 
fun pop (nil) =  raise EmptyStack 
    | pop ((hd :: tl) : 'a Stack) = tl

fun top (l : 'a Stack): 'a = (case l of
           [] => raise EmptyStack
        |  hd :: _ => hd)

fun empty (l : 'a Stack): bool = (case l of
           [] => true
        |   _ => false)

fun poptop(nil) = NONE
    | poptop((hd :: tl) : 'a Stack) = SOME (hd, tl) 

fun nth (l : 'a Stack, x : int) = List.nth(l, x)

fun drop (l : 'a Stack, x : int) = List.drop(l, x);

fun depth (l: 'a Stack) : int = (case l of
            [] => 0       
        |   (hd :: tl) => 1 + depth(tl))

fun app f (l : 'a Stack) = List.app f l;

fun map f (l : 'a Stack) = List.map f l;

fun mapPartial f (l : 'a Stack) = List.mapPartial f l;

fun find f (l : 'a Stack) = List.find f l;

fun filter f (l : 'a Stack) = List.filter f l;

fun foldr f x l = List.foldr f x l; 

fun foldl f x l = List.foldl f x l; 

fun exists f (l : 'a Stack) = List.exists f l;

fun all f (l : 'a Stack) = List.all f l;

fun list2stack (l : 'a list) : 'a Stack = l; 

fun stack2list (l : 'a Stack) : 'a list = l; 

fun toString f ([]) = ""
    | toString f (x::y) = (f x) ^ toString f y;

end