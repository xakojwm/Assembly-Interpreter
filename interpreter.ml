(*
Honor code comes here:

First Name: Jacob
Last Name: Morris
BU ID: U28939360

I pledge that this program represents my own
program code and that I have coded on my own. I received
help from no one in designing and debugging my program.
I have read the course syllabus of CS 320 and have read the sections on Collaboration
and Academic Misconduct. I also understand that I may be asked to meet the instructor
or the TF for a follow up interview on Zoom. I may be asked to explain my solution in person and
may also ask you to solve a related problem.
*)
open Printf



type 'a parser = Parser of (string -> ('a * string) list)

type const =
    Integer of int
  |
    String of string
  |
    Boolean of bool
  |
    Name of char list
  |
    Unit of string



type command = 
    Push of const
  |
    Pop
  |
    Swap
  |
    Log
  |
    Add 
  |
    Sub 
  |
    Mul 
  |
    Div 
  |
    Rem
  |
    Neg
  |
    Cat
  | 
    And
  | 
    Or 
  | 
    Not
  | 
    Eq
  | 
    Lte 
  | 
    Lt 
  | 
    Gte 
  | 
    Gt
  |  
    Let
  | 
    Ask

  | Begin_End of command list 

  | If_Else_End of (command list * command list)

  | Call

  | Throw

  | DefFun of ( const * const * command list)

  | TryCatch of ( command list * command list)


type value = 
    Constant of const
  |
    Function of ( char list * char list * command list * env)

and env = (char list * value ) list


let reverse (l: 'a list): 'a list = 
  let rec aux curr acc = 
    match curr with
      [] -> acc
    |
      h::t -> aux t (h::acc)
  in aux l []


let explode s =
  List.of_seq (String.to_seq s)

let implode ls =
  String.of_seq (List.to_seq ls)

let parse p inp=
  match p with 
    Parser f -> f inp

let returnP a = 
  Parser(fun inp -> [a,inp])

let failP = 
  Parser(fun inp -> [])

let charP = 
  Parser( fun inp -> match (explode inp) with
      |[] -> []
      | ch::rest -> [ch, implode(rest)]
    )

let (>>=) p1 p2 =
  Parser( fun inp -> match (parse p1 inp ) with
      | [] -> []
      | (a1, rest1) :: _ -> let parser2 = p2 a1 in 
        match (parse parser2 rest1) with
        | [] -> []
        | (a2,rest2) :: _ -> [a2,rest2]

    )

let (<|>) p1 p2 = 
  Parser (
    fun s->  match (parse p1 s) with 
        []-> parse p2 s 
      |
        r->r
  )

let satcP (c:char) = 
  charP >>= (fun a -> if a = c then (returnP c) else failP)

let not_satcP c = 
  charP >>= (fun a -> if a != c then (returnP a) else failP)

let satsP s = 
  if s="" then failP else
    let rec asats (s:string)= 
      match (explode s) with 
        h::rest->satcP h >>=fun _->asats (implode rest)
      |
        []-> returnP([])
    in 
    asats (s:string)

let rec many0 p =
  (p >>= fun a -> 
   many0 p >>= fun b-> 
   returnP (a::b))
  <|>
  returnP []


let rec many1 p =
  p >>= fun a -> 
  many0 p >>= fun b-> 
  returnP (a::b)

let whitespaceP = 
  satcP ' ' <|> satcP '\t' <|> satcP '\n' <|> satcP '\t' <|> satcP '\012'

let digitP = 
  satcP '0' <|> satcP '1' <|> satcP '2' <|> satcP '3' <|> satcP '4' <|> satcP '5' <|> satcP '6'<|> satcP '7' <|> satcP '8' <|> satcP '9'

let natP = 
  (many1 digitP >>= fun a-> 
   returnP (int_of_string (implode a)))

let integerP = 
  natP
  <|>
  (satcP '-' >>= fun _->
   natP >>= fun v -> 
   returnP ((-1)*v)) 


let letterP =
  Parser(fun inp -> 
      match explode inp with
        h::t -> if (((Char.code h) >= 97 && (Char.code h) <= 122) || ((Char.code h) >= 65 && (Char.code h) <= 90))
        then [(h, implode t)] else []
      |
        _ -> []
    )

let aposP =
  satcP '\''

let underscoreP = 
  satcP '_'

let falseP = 
  satsP "<false>" >>= (fun _ -> returnP false)

let trueP = 
  satsP "<true>" >>= (fun _ -> returnP true)

let unitP =
  satsP "<unit>" >>= (fun _ -> returnP "<unit>")

let asciiP = 
  many0 (not_satcP '\"') 

let stringP =
  satcP '\"' >>= fun _ -> asciiP >>= fun a -> satcP '\"' >>= fun _ -> returnP ("\"" ^ implode a ^ "\"")

let boolP = 
  falseP 
  <|>
  trueP

let name_disjP = 
  letterP
  <|>
  underscoreP
  <|>
  aposP
  <|>
  digitP

let nameP = 
  letterP >>= fun a -> many0 name_disjP >>= fun b -> returnP (a::b)

let push_genericP = 
  many0 whitespaceP >>= fun _-> satsP "Push" >>= fun _ -> many1 whitespaceP

let push_intP = 
  push_genericP >>= fun _->
  integerP >>= fun i -> 
  returnP (Push (Integer i))

let push_boolP = 
  push_genericP >>= fun _->
  boolP >>= fun i -> 
  returnP (Push (Boolean i))

let push_stringP =
  push_genericP >>= fun _->
  stringP >>= fun i -> 
  returnP (Push (String i))

let push_nameP = 
  push_genericP >>= fun _->
  nameP >>= fun i -> 
  returnP (Push (Name i))

let push_unitP = 
  push_genericP >>= fun _->
  unitP >>= fun i -> 
  returnP (Push (Unit i))

let pushP = 
  push_boolP
  <|>
  push_intP
  <|>
  push_nameP
  <|>
  push_unitP
  <|>
  push_stringP

let popP = 
  many0 whitespaceP >>= fun _->
  satsP "Pop" >>= fun _ -> (returnP Pop)

let swapP = 
  many0 whitespaceP >>= fun _->
  satsP "Swap" >>= fun _ -> (returnP Swap)

let logP = 
  many0 whitespaceP >>= fun _->
  satsP "Log" >>= fun _ -> (returnP Log)

let addP = 
  many0 whitespaceP >>= fun _->
  satsP "Add" >>= fun _ -> (returnP Add)

let subP = 
  many0 whitespaceP >>= fun _->
  satsP "Sub" >>= fun _ -> (returnP Sub)

let mulP = 
  many0 whitespaceP >>= fun _->
  satsP "Mul" >>= fun _ -> (returnP Mul)

let divP = 
  many0 whitespaceP >>= fun _->
  satsP "Div" >>= fun _ -> (returnP Div)

let remP = 
  many0 whitespaceP >>= fun _->
  satsP "Rem" >>= fun _ -> (returnP Rem)

let negP = 
  many0 whitespaceP >>= fun _->
  satsP "Neg" >>= fun _ -> (returnP Neg)

(* Part 2 Command Parsers *)

let catP = 
  many0 whitespaceP >>= fun _->
  satsP "Cat" >>= fun _ -> (returnP Cat)


let andP = 
  many0 whitespaceP >>= fun _->
  satsP "And" >>= fun _ -> (returnP And)

let orP = 
  many0 whitespaceP >>= fun _->
  satsP "Or" >>= fun _ -> (returnP Or)

let notP = 
  many0 whitespaceP >>= fun _->
  satsP "Not" >>= fun _ -> (returnP Not)

let eqP = 
  many0 whitespaceP >>= fun _->
  satsP "Eq" >>= fun _ -> (returnP Eq)

let lteP = 
  many0 whitespaceP >>= fun _->
  satsP "Lte" >>= fun _ -> (returnP Lte)

let ltP = 
  many0 whitespaceP >>= fun _->
  satsP "Lt" >>= fun _ -> (returnP Lt)

let gteP = 
  many0 whitespaceP >>= fun _->
  satsP "Gte" >>= fun _ -> (returnP Gte)

let gtP =
  many0 whitespaceP >>= fun _->
  satsP "Gt" >>= fun _ -> (returnP Gt)

let letP = 
  many0 whitespaceP >>= fun _->
  satsP "Let" >>= fun _ -> (returnP Let)

let askP = 
  many0 whitespaceP >>= fun _->
  satsP "Ask" >>= fun _ -> (returnP Ask)


(* part 3 parsers *)

let callP = 
  many0 whitespaceP >>= fun _->
  satsP "Call" >>= fun _ -> (returnP Call)

let throwP =
  many0 whitespaceP >>= fun _->
  satsP "Throw" >>= fun _ -> (returnP Throw)


let rec command_listP() = 
  many0 (commandP() >>= fun a -> satcP ';' >>= fun _ -> returnP a)

and commandP() =
  pushP <|> popP <|> swapP <|> logP <|>
  addP <|> mulP <|> divP <|> remP <|>
  negP <|> subP <|> catP <|> andP <|>
  orP <|> notP <|> eqP <|> lteP <|>
  ltP <|> gteP <|> gtP <|> letP <|>
  askP <|> begin_endP() <|> if_else_endP() <|> 
  throwP <|> callP  <|> deffunP() <|> trycatchP()

and begin_endP() = 
  many0 whitespaceP >>= fun _->
  satsP "Begin" >>= fun _ -> command_listP() >>= fun c -> many0 whitespaceP
  >>= fun _-> satsP "End" >>= fun _ -> (returnP (Begin_End (c) ))

and if_else_endP() = 
  many0 whitespaceP >>= fun _->
  satsP "If" >>= fun _ -> command_listP() >>= fun c -> many0 whitespaceP
  >>= fun _-> satsP "Else" >>= fun _ -> command_listP() >>= fun d -> many0 whitespaceP
  >>= fun _-> satsP "End" >>= fun _ ->(returnP (If_Else_End (c,d) ))

and deffunP() = 
  many0 whitespaceP >>= fun _ -> satsP "DefFun" >>= fun _ ->
  many0 whitespaceP >>= fun _ -> nameP >>= fun fname -> many0 whitespaceP >>= 
  fun _ -> nameP >>= fun arg -> many0 whitespaceP >>= 
  fun _ -> command_listP() >>= fun comms ->  many0 whitespaceP >>= 
  fun _ -> satsP "End" >>= fun _ -> ( returnP (DefFun (Name fname, Name arg, comms ) ))

and trycatchP() = 
  many0 whitespaceP >>= fun _->
  satsP "Try" >>= fun _ -> command_listP() >>= fun comm1 -> many0 whitespaceP
  >>= fun _-> satsP "Catch" >>= fun _ -> command_listP() >>= fun comm2 -> many0 whitespaceP
  >>= fun _-> satsP "End" >>= fun _ ->(returnP (TryCatch (comm1,comm2) ))

let const_to_string (a:const): string = 
  match a with
    Integer i -> string_of_int i
  |
    String j -> j
  |
    Boolean k -> "<" ^ (string_of_bool k) ^ ">"
  |
    Name l -> implode l
  |
    Unit m -> m

let rec compare_char_lists (a: char list) (b: char list) : bool = 
  match a,b with
    [],[] -> true
  |
    [], _ -> false
  |
    _, [] -> false
  |
    h1::t1, h2::t2 -> if (h1 == h2) then compare_char_lists t1 t2 else false

let fetch_value (v:char list) (variables:(char list * value) list ) =
  match List.assoc_opt v variables with
    None -> (Constant(Unit ""), 4)
  |
    Some vv -> (vv, 0)

(*
let rec fetch_value (v:char list) (variables:(char list * value) list ) =
  match variables with 
    [] -> (Constant (Unit ""),4)
  |
    (name, vall)::rest -> if compare_char_lists v name then
      (match vall with Function a -> (Function a, 0) | Constant a -> ((Constant a), 0) ) else fetch_value v rest
*)


let push (a: value)(b: value list): (value list * int) = 
  (a :: b, 0)

let pop (b: value list): (value list * int) = 
  match b with
    []-> ([], 2)
  |
    h::t -> (t, 0)

let log (b: value list): (value list * string * int) = 
  match b with
    []-> ([], "", 2)
  |
    (Constant h)::t -> (t, const_to_string h, 0)
  |
    Function (fname,arg,comms,env)::t -> (t, (implode fname) , 0)


let swap (b: value list): (value list * int) = 
  match b with
    []-> ([], 2)
  |
    h::[] -> ([], 2)
  | h::n::rest -> (n::h::rest, 0)

let math_no_div (f:int->int->int)(b: value list): (value list * int) = 
  match b with
    []-> ([], 2)
  |
    [x] -> ([], 2)
  |
    (Constant(Integer h)) :: (Constant(Integer n) )::rest -> push (Constant (Integer (f h n))) rest
  |  _ -> ([], 1)



let add (b: value list): (value list * int) = 
  math_no_div (+) b

let sub (b: value list): (value list * int) = 
  math_no_div (-) b

let mul (b: value list): (value list * int) = 
  math_no_div ( * ) b

let div (b: value list): (value list * int) = 
  match b with
    []-> ([], 2)
  |
    [x] -> ([], 2)
  | 
    Constant (Integer h):: Constant (Integer n)::rest -> if n == 0 then ([], 3) else (push  (Constant (Integer (h / n))) rest)  
  |
    _ -> ([], 1)

let rem (b: value list): (value list * int) = 
  match b with
    []-> ([], 2)
  |
    [x] -> ([], 2)
  | 
    Constant (Integer h):: Constant (Integer n)::rest -> if n == 0 then ([], 3) else (push (Constant(Integer (h mod n))) rest)  
  |
    _ -> ([], 1)

let neg (b: value list): (value list * int) = 
  match b with
    [] -> ([], 2)
  |
    Constant (Integer h)::rest -> push (Constant(Integer (-1*h))) rest
  | _ -> ([], 1)

(* part 2 command execution stuff *)

let rec remove_char_list_end (a:char list) = 
  match a with
    [] -> []
  |
    [x] -> []
  |
    h::t -> h:: remove_char_list_end t


let string_cat (a: string) (b:string) = 
  match explode a, explode b with
    ( ['"'; '"'], c) -> b
  |
    (c, ['"'; '"']) -> a
  |
    x,y1::y2 -> implode (remove_char_list_end x) ^ (implode y2)
  |
    _ -> ""

let cat (b:value list): (value list * int) = 
  match b with
    [] -> ([], 2)
  |
    [x] -> ([], 2)
  |
    Constant(String h) :: Constant(String n) :: rest -> (push (Constant(String (string_cat h  n))) rest)
  |
    _ -> ([], 1)

let and_stack (b: value list): (value list * int) = 
  match b with
    [] -> ([], 2)
  |
    [x] -> ([], 2)
  |
    Constant(Boolean h) :: Constant(Boolean n) :: rest -> if (h && n) then push (Constant(Boolean true)) rest else push (Constant (Boolean false)) rest
  |
    _ -> ([], 1)

let or_stack (b: value list): (value list * int) = 
  match b with
    [] -> ([], 2)
  |
    [x] -> ([], 2)
  |
    Constant (Boolean h) :: Constant (Boolean n) :: rest -> if (h || n) then push (Constant (Boolean true)) rest else push (Constant (Boolean false)) rest
  |
    _ -> ([], 1)

let not_stack (b: value list): (value list * int) = 
  match b with
    [] -> ([], 2)
  |
    Constant (Boolean h)::rest -> push (Constant(Boolean (not h))) rest
  |
    _ -> ([], 1)

let generic_int_comparison (f: int -> int -> bool)(b: value list): (value list * int) = 
  match b with
    [] -> ([], 2)
  |
    [x] -> ([], 2)
  |
    Constant(Integer h) :: Constant(Integer n) :: rest -> if f h n then push (Constant(Boolean true)) rest else push (Constant(Boolean false)) rest
  |
    _ -> ([], 1)

let eq (b:value list): (value list * int) = 
  generic_int_comparison (==) b

let lte (b:value list): (value list * int) = 
  generic_int_comparison (<=) b

let lt (b:value list): (value list * int) = 
  generic_int_comparison (<) b

let gte (b:value list): (value list * int) = 
  generic_int_comparison (>=) b

let gt (b:value list): (value list * int) = 
  generic_int_comparison (>) b


let let_stack (b: value list)(variables: (char list * value) list): ((char list * value) list * value list * int) = 
  match b with
    [] -> ( variables , [] , 2)
  |
    [x] -> (variables , [], 2)
  |
    Constant (Name n) :: v :: rest ->  (match v with
        Constant h -> ( (n, Constant h) :: variables, rest, 0)  
      | Function f -> ((n, Function f) :: variables, rest, 0)

    )
  |
    _ -> (variables, [], 1)



let ask (b:value list)(variables:(char list * value) list): (value list * int) = 
  match b with
    [] -> ([], 2)
  |
    Constant (Name h) :: rest -> (match fetch_value h variables with 
        (_, 4) -> ([],4)
      | 
        (value, _) ->  (value :: rest, 0)
    )
  |
    _ -> ([], 1)


(* part 3 execution stuff *)

let throw (b:value list): (value list * int) = 
  match b with 
    [] -> (b, 2)
  |
    Constant(Integer code) :: rest -> (rest, code)
  |
    _ -> (b, 1)

let deffun (funn:  (char list * char list * command list))(variables: (char list * value) list): ((char list * value) list) =
  match funn with
    (f, arg, comms) -> (f, Function (f,arg,comms, (f, Function(f,arg,comms,variables))::variables)     ) :: variables
(*I really think all the nonsense is coming from here maybe oh, the inner function is missing the access to itself i think. *)

let rec merge_lists (a: 'a list) (b: 'a list) = 
  match a with 
    [] -> b
  |
    h::t -> h :: merge_lists t b

let rec comm1 (b: command list) (stack: value list) (output: string list)(variables:( char list * value) list):
  (command list * value list * string list * int * ( char list * value) list) =
  match b with
    []-> ([], stack, output, 0, variables)
  |
    h::t -> match h with 
      Push i -> (match push (Constant i) stack with
          (stack_rest,code) -> (t, stack_rest, output, code, variables) )
    |
      Pop -> (match pop stack with
          (stack_rest,code) -> (t, stack_rest, output, code, variables) )
    |
      Swap -> (match swap stack with
          (stack_rest,code) -> (t, stack_rest, output, code, variables) )
    |
      Log -> (match log stack with
          (new_stack1, "", code) -> (t, new_stack1, output, code, variables)
        | (new_stack2, out, code) -> (t, new_stack2, out::output, code, variables)
      ) 
    |
      Add -> (match add stack  with
          (stack_rest,code) -> (t, stack_rest, output, code, variables) )
    |
      Sub -> (match sub stack  with
          (stack_rest,code) -> (t, stack_rest, output, code, variables) )
    |
      Mul -> (match mul stack  with
          (stack_rest,code) -> (t, stack_rest, output, code, variables) )
    |
      Div -> (match div stack with
          (stack_rest,code) -> (t, stack_rest, output, code, variables) )
    |
      Rem -> (match rem stack with
          (stack_rest,code) -> (t, stack_rest, output, code, variables) )
    |
      Neg -> (match neg stack with
          (stack_rest,code) -> (t, stack_rest, output, code, variables) )
    |
      Cat -> (match cat stack with
          (stack_rest,code) -> (t, stack_rest, output, code, variables) )
    | 
      And -> (match and_stack stack with
          (stack_rest,code) -> (t, stack_rest, output, code, variables) )
    | 
      Or -> (match or_stack stack with
          (stack_rest,code) -> (t, stack_rest, output, code, variables) )
    | 
      Not -> (match not_stack stack with
          (stack_rest,code) -> (t, stack_rest, output, code, variables) )
    | 
      Eq -> (match eq stack with
          (stack_rest,code) -> (t, stack_rest, output, code, variables) )
    | 
      Lte -> (match lte stack with
          (stack_rest,code) -> (t, stack_rest, output, code, variables) )
    | 
      Lt -> (match lt stack with
          (stack_rest,code) -> (t, stack_rest, output, code, variables) )
    | 
      Gte -> (match gte stack with
          (stack_rest,code) -> (t, stack_rest, output, code, variables) )
    | 
      Gt -> (match gt stack with
          (stack_rest,code) -> (t, stack_rest, output, code, variables) )

    | Let -> (match (let_stack stack variables) with
          (vars, stack_rest,code) -> (t, stack_rest, output, code, vars))

    | Ask -> (match (ask stack variables) with
          (stack_rest,code) -> (t, stack_rest, output, code, variables))

    | Begin_End comms -> (match (eval comms output [] variables) with
          (out, [], 0, _) -> (t, [], out, 2, variables)
        |
          (out, top::curr_stack, 0, _) -> (t, top::stack, out, 0, variables)

        |
          (out, new_stack, code, vars) -> (t,[], out, code, variables)
      )
    |
      If_Else_End (if_branch, else_branch) -> (match stack with 
          [] -> (t, [], output,  2, variables)
        | h::tail -> (match h with 
              Constant(Boolean value) -> 
              (if value then 
                 (match eval if_branch output tail variables with
                    (out, new_stack, 0, vars) -> (t, new_stack, out, 0, vars)
                  |
                    (out, new_stack, code, vars) -> (t, new_stack, out, code, vars)
                 )
               else (match eval else_branch output tail variables  with
                     (out, new_stack, 0, vars) -> (t, new_stack, out, 0, vars)
                   |
                     (out, new_stack, code, vars) -> (t, new_stack, out, code, vars)
                 )   )
            |
              _ -> (t, [], output,  1, variables)
          )

      )
    |
      DefFun (Name f, Name arg, value) -> (t, stack, output, 0, deffun (f, arg, value) variables)
    |
      Throw -> (match throw stack with
          (stack_rest,code) -> (t, stack_rest, output, code, variables) )
    |
      Call -> ( match stack with 
          [] -> (t, [], output, 2, variables)
        |
          [x] -> (t, [], output, 2, variables)
        |
          (* stack overflow as a result of adding the variables in here whyyyyyyyyyyyy
          (fname, Function(fname, arg,fun_comms,env))
          *)
          constant:: Function(fname, arg, fun_comms, env):: rest1  -> ( match  eval fun_comms output [] ((fname, Function(fname, arg,fun_comms,env))::((arg, constant)::env) ) with
(out, top::rest2 , 0 , _ ) -> (t, top::rest1, out, 0, variables)
|
(out, small_stack, 0, _) -> (t,  rest1, out, 2, variables)
|
(out, _ , code, _) -> (t, rest1, out, code, variables)   )     

|
  _ -> (t, [], output, 1, variables) )

|
  TryCatch (comms1, comms2) -> ( match eval comms1 output stack variables with
        (out1, new_stack, 0, vars) -> (t, new_stack, out1, 0, vars)
      |
        (out1, _, code, _ ) -> ( match eval comms2 out1 (Constant(Integer(code))::stack) variables with 
            (out2, new_stack, 0, vars) -> (t, new_stack, out2, 0, vars)
          |
            (out2, _ , code, _ ) -> (t, [], out2, code, [])
        )
    )
|
  _ -> (t,[],output,0, [])

and eval (coms: command list) (output: string list)(stack: value list) (variables:(char list * value) list) =
    let rec aux comms curr_stack outgoing vars =
      match (comm1 comms curr_stack outgoing vars) with
        ([], new_stack, out, 0, vars) -> (out, new_stack, 0, vars)
      |
        (com_list, new_stack, out, 0, vars) -> aux com_list new_stack out vars
      |
        (_ , _ , out, code , _) -> (out, [], code, [])

    in aux coms stack output variables

let exec (coms: command list): (string list*int) = 
  let rec aux coms stack output variables =
    match (comm1 coms stack output variables) with
      ([], _, out, 0, _) -> (reverse out, 0)
    |
      (com_list, curr_stack, out, 0, vars) -> aux com_list curr_stack out vars
    |
      (_ , _ , out , code , _) -> (reverse out, code)
  in aux coms [] [] []


let interpreter (s : string) : string list * int = 
  match parse (command_listP()) s with 
    [(a,_)] -> exec a
  | _ -> ([], 0)


let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ (loop ())
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

let runfile (file : string) : string list * int =
  let s = readlines file in
  interpreter s

let testing (file:string)  =
  let s = readlines file in parse (command_listP()) s

let matching_push (st: value list) (v: value) = 
  match push v st with
    (s, _) -> s


let matching_add (st: value list) = 
  match add st with
    (s, _) -> s

let matching_let (st:value list) (vars: (char list * value) list) =
  match let_stack st vars with
    (v, s, c) -> v,s

let comm_list (file: string): command list =
  match (testing file) with
  | [(c , s)] -> c
  | _ -> []


let call (st:value list) (vars: (char list * value) list) = 
  match st with 
    [] -> (st,vars)
  |
    [x] -> (st, vars)
  |
    constant:: Function(fname, arg, fun_comms, env):: rest ->  ( st, ((arg, constant)::vars) )

  |
    _ -> ([],[])

let ask_me (st:value list) (vars: (char list * value) list) = 
  (match (ask st vars) with
     (stack_rest,code) -> stack_rest)

let exec2 (coms: command list) = 
  let rec aux coms stack output variables =
    match (comm1 coms stack output variables) with
      ([], st, out, 0, vars) -> (reverse out, 0, vars, st)
    |
      (com_list, curr_stack, out, 0, vars) -> aux com_list curr_stack out vars
    |
      (_ , st , out , code , vars) -> (reverse out, code, vars, st)
  in aux coms [] [] []

let interpreter2 (s : string) = 
  match parse (command_listP()) s with 
    [(a,_)] -> exec2 a
  |
    _ -> ([], 10, [],[])

let abc (file : string) =
  let s = readlines file in
  interpreter2 s

