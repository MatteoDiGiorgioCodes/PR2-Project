type ide = string;;

(* etichette denotanti i nomi dei tipi *)
type typename = 
  | BooleanType 
  | IntegerType 
  | StringType

(*tipi algebrici*)
type exp = 
  | Eint of int 
  | Ebool of bool 
  | Estring of string
  | Den of ide 
  | Prod of exp * exp 
  | Sum of exp * exp 
  | Diff of exp * exp 
  | Eq of exp * exp 
  | Minus of exp 
  | IsZero of exp 
  | Or of exp * exp 
  | And of exp * exp 
  | Not of exp
  | Mod of exp * exp 
  | Ifthenelse of exp * exp * exp 
  | Let of ide * exp * exp 
  | Fun of ide * exp 
  | Apply of exp * exp 
  | Letrec of ide * exp * exp
  | EmptySet of string
  | Singleton of exp * string
  | Of of string * (exp list)
  | BelongsTo of exp * exp
  | Insert of exp * exp
  | Remove of exp * exp
  | Is_empty of exp
  | Is_subset of exp * exp
  | Get_min of exp 
  | Get_max of exp
  | Union  of exp * exp
  | Intersection of exp * exp
  | Difference of exp * exp
  | ForAll of exp * exp
  | Exists of exp * exp
  | Filter of exp * exp
  | Map of exp * exp;;

(* environment *)
type 't env = ide -> 't;;
let emptyenv (v : 't) = function x -> v;;
let applyenv (r : 't env) (i : ide) = r i;;
let bind (r : 't env) (i : ide) (v : 't) = function x -> if x = i then v else applyenv r x;;

(*tipi esprimibili*)
type set = 
  | Empty of typename 
  | Set of evT list * typename
and evT =
  | Int of int 
  | Bool of bool 
  | String of string
  | Unbound 
  | FunVal of evFun 
  | RecFunVal of ide * evFun
  | Setval of set
and evFun = 
  ide * exp * evT env;;

(* restituisce il tipo del parametro se esso è un tipo esprimibile consumabile dalle funzioni di set *)
let typeof (v: evT) : typename = match v with
    Int(_) -> IntegerType |
    Bool(_) -> BooleanType |
    String(_) -> StringType |
    _ -> failwith("not an expressable value")

(*type checking*)
let typecheck (s : string) (v : evT) : bool = match s with
    "int" -> (match v with
        Int(_) -> true |
        _ -> false) |
    "bool" -> (match v with
        Bool(_) -> true |
        _ -> false) |
	(* estensione *)
    "string" -> (match v with
        String(_) -> true |
        _ -> false) |
    _ -> failwith("not a valid type");;

(*operazioni di base*)
let prod x y = if (typecheck "int" x) && (typecheck "int" y)
  then (match (x,y) with
        (Int(n),Int(u)) -> Int(n*u))
  else failwith("Type error");;

let sum x y = if (typecheck "int" x) && (typecheck "int" y)
  then (match (x,y) with
        (Int(n),Int(u)) -> Int(n+u))
  else failwith("Type error");;

let diff x y = if (typecheck "int" x) && (typecheck "int" y)
  then (match (x,y) with
        (Int(n),Int(u)) -> Int(n-u))
  else failwith("Type error");;

let eq x y = if (typecheck "int" x) && (typecheck "int" y)
  then (match (x,y) with
        (Int(n),Int(u)) -> Bool(n=u))
  else failwith("Type error");;

let minus x = if (typecheck "int" x) 
  then (match x with
        Int(n) -> Int(-n))
  else failwith("Type error");;

let iszero x = if (typecheck "int" x)
  then (match x with
        Int(n) -> Bool(n=0))
  else failwith("Type error");;

let isEmptyString s = if (typecheck "string" s)
  then (match s with
        String(s) -> Bool(s="")
    ) else failwith("Type error");;

let vel x y = if (typecheck "bool" x) && (typecheck "bool" y)
  then (match (x,y) with
        (Bool(b),Bool(e)) -> (Bool(b||e)))
  else failwith("Type error");;

let et x y = if (typecheck "bool" x) && (typecheck "bool" y)
  then (match (x,y) with
        (Bool(b),Bool(e)) -> Bool(b&&e))
  else failwith("Type error");;

let non x = if (typecheck "bool" x)
  then (match x with
        Bool(true) -> Bool(false) |
        Bool(false) -> Bool(true))
  else failwith("Type error");;

let int_mod(x, y) = 
  match((typecheck "int" x), (typecheck "int" y), x, y) with
  | (true, true, Int(v), Int(w)) -> Int(v mod w)
  | (_,_,_,_) -> failwith("Type error ");;

(* Operazione di conversione di tipo evT -> exp *)
let evTToExp e =
  match e with
    (Bool b) -> (Ebool b) |
    (Int i) -> (Eint i) |
    (String s) -> (Estring s) |
    _ -> failwith("unintended use")
;;

(*operazioni su set*)
let belongsto (e : evT)(s : evT)=
  match s with
		|Setval(set)->
      (let rec f l el = match l with
          |[]-> false
          |h::b->if(h = e)
              then  true
              else f b el
       in (match set with
           |Empty(_)->false
           |Set(lst, typ)->if(typ = typeof e)
               then f lst e
               else failwith("Type error")))
  |_->failwith("Type error");;


let insert (e : evT)(s : evT)=
  match s with
  |Setval(set)->(match set with
      |Empty(t)->if(typeof e = t)
          then Set([e], t)
          else failwith("Type error")
      |Set(lst, t)->if(typeof e = t)
          then (if(belongsto e s)
                then set
                else Set(e::lst, t))
          else failwith("Type error")
      |_->failwith("Type error"))
  |_->failwith("Type error");;

let remove (e : evT)(s : evT)=
  match s with
  |Setval(set)->
      (let rec f l el = match l with
          |[]->[]
          |h::b->if(h = e)
              then b
              else h::(f b el)
       in (match set with
           |Empty(t)->set
           |Set(lst, typ)->if(typ = typeof e)
               then Set((f lst e), typ)
               else failwith("Type error")))
  |_->failwith("Type error");;

let is_empty (s : evT)=
  match s with
  |Setval(set)->(match set with
      |Empty(_)->true
      |Set(lst, typ)->(match lst with
          |[]-> true
          |_-> false)
      |_->failwith("Type error"))
  |_->failwith("Type error");;

let is_subset (s1 : evT)(s2 : evT)=
  let rec f l s=
    match (l,s) with 
    |([],_)-> true 
    |(h::t,Setval(_))->if(belongsto h s)
        then f t s
        else false
    |_->failwith("Type error")
  in
  match (s1, s2) with
  |(Setval(Set(_,_)),Setval(Empty(_)))->false
  |(Setval(Empty(t1)),Setval(Set(_,t2)))-> t1=t2
  |(Setval(Empty(t1)),Setval(Empty(t2)))-> t1=t2
  |(Setval(Set(lst1,t1)),Setval(Set(lst2,t2)))->if (t1=t2)
      then f lst1 s2
      else false
  |_->failwith("Type error");;

let get_min (s : evT)=
  let rec f list min=
    match list with
    |[]->min
    |h::t->if (h<min)
        then f t h
        else f t min
    |_->failwith("Type error")
  in
  match s with
  |Setval(Empty(_))->failwith("get_min doesn't work on empty sets")
  |Setval(Set([],_))->failwith("get_min doesn't work on empty sets")
  |Setval(Set(h::t,typ))->f t h
  |_->failwith("Type error");;

let get_max (s : evT)=
  let rec f list max=
    match list with
    |[]->max
    |h::t->if (h>max)
        then f t h
        else f t max
    |_->failwith("Type error")
  in
  match s with
  |Setval(Empty(_))->failwith("get_max doesn't work on empty sets")
  |Setval(Set([],_))->failwith("get_max doesn't work on empty sets")
  |Setval(Set(h::t,typ))->f t h
  |_->failwith("Type error");;

let union (s1 : evT)(s2 : evT)=
  let rec f lis set=
    match (lis, set) with
    |([], Setval(Empty(ty)))->Setval(Empty(ty))
    |([], Setval(Set(lis,ty)))->Setval(Set(lis,ty))
    |(h::t, Setval(Empty(ty)))->Setval(Set(lis,ty))
    |(h::t, Setval(Set([],ty)))->Setval(Set(lis,ty)) 
    |(h::t, Setval(Set(res,ty)))->if(belongsto h set)
        then f t set
        else (if (typeof h = ty)
              then f t (Setval(insert h set))
              else failwith("Type error"))
    |_->failwith("Type error")
  in
  match (s1,s2) with
  |(Setval(Empty(t1)),Setval(Empty(t2)))->if (t1 = t2)
      then Setval(Empty(t1))
      else failwith("Incompatible types")
  |(Setval(Set([],t1)),Setval(Set([],t2)))->if (t1 = t2)
      then Setval(Empty(t1))
      else failwith("Incompatible types")
  |(Setval(Empty(t1)),Setval(Set(list,t2)))->if (t1 = t2)
      then Setval(Set(list,t2))
      else failwith("Incompatible types")
  |(Setval(Set(list,t1)),Setval(Empty(t2)))->if (t1 = t2)
      then Setval(Set(list,t1))
      else failwith("Incompatible types")
  |(Setval(Set(l1,t1)),Setval(Set(l2,t2)))->f l1 s2
  |_->failwith("Not a set");;
             
let intersection (s1 : evT)(s2 : evT)=
  let rec f lis set result=
    match (lis, set) with
    |([], Setval(Empty(_)))->result
    |([], Setval(Set(_,_)))->result
    |(h::t, Setval(Empty(ty)))->set
    |(h::t, Setval(Set([],ty)))->set
    |(h::t, Setval(Set(res,ty)))->if(belongsto h set)
        then f t set (Setval(insert h result))
        else f t set result
    |_->failwith("Type error")
  in
  match (s1,s2) with
  |(Setval(Empty(t1)),Setval(Empty(t2)))->if (t1 = t2)
      then Setval(Empty(t1))
      else failwith("Incompatible types")
  |(Setval(Set([],t1)),Setval(Set([],t2)))->if (t1 = t2)
      then Setval(Empty(t1))
      else failwith("Incompatible types")
  |(Setval(Empty(t1)),Setval(Set(_,t2)))->if (t1 = t2)
      then (Setval(Empty(t1)))
      else failwith("Incompatible types")
  |(Setval(Set(_,t1)),Setval(Empty(t2)))->if (t1 = t2)
      then Setval(Empty(t2))
      else failwith("Incompatible types")
  |(Setval(Set(l1,t1)),Setval(Set(l2,t2)))->f l1 s2 (Setval(Empty(t1)))
  |_->failwith("Not a set");;
             
let difference (s1 : evT)(s2 : evT)=
  let rec f lis set =
    match (lis, set) with
    |([], Setval(Empty(_)))->set
    |([], Setval(Set(_,_)))->set
    |(h::t, Setval(Empty(ty)))->set
    |(h::t, Setval(Set([],ty)))->set
    |(h::t, Setval(Set(res,ty)))->if(belongsto h set)
        then f t (Setval(remove h set))
        else f t set
    |_->failwith("Type error")
  in
  (match (s1,s2) with
   |(Setval(Empty(t1)),Setval(Empty(t2)))->if (t1 = t2)
       then Setval(Empty(t1))
       else failwith("Incompatible types")
   |(Setval(Set([],t1)),Setval(Set([],t2)))->if (t1 = t2)
       then Setval(Empty(t1))
       else failwith("Incompatible types")
   |(Setval(Empty(t1)),Setval(Set(_,t2)))->if (t1 = t2)
       then s1
       else failwith("Incompatible types")
   |(Setval(Set(_,t1)),Setval(Empty(t2)))->if (t1 = t2)
       then s1
       else failwith("Incompatible types")
   |(Setval(Set(l1,t1)),Setval(Set(l2,t2)))->f l2 s1
   |_->failwith("Not a set"));;

(*interprete*)
let rec eval (e : exp) (r : evT env) : evT = 
  match e with
  |Eint n -> Int n 
  |Ebool b -> Bool b 
  |Estring s -> String s
  |IsZero a -> iszero (eval a r) 
  |Den i -> applyenv r i 
  |Eq(a, b) -> eq (eval a r) (eval b r) 
  |Prod(a, b) -> prod (eval a r) (eval b r) 
  |Sum(a, b) -> sum (eval a r) (eval b r) 
  |Diff(a, b) -> diff (eval a r) (eval b r) 
  |Minus a -> minus (eval a r) 
  |And(a, b) -> et (eval a r) (eval b r) 
  |Or(a, b) -> vel (eval a r) (eval b r) 
  |Not a -> non (eval a r) 
  |Mod(e1, e2) -> int_mod((eval e1 r), (eval e2 r))
  |Ifthenelse(a, b, c) -> 
      let g = (eval a r) in
      if (typecheck "bool" g) 
      then (if g = Bool(true) then (eval b r) else (eval c r))
      else failwith ("nonboolean guard") 
  |Let(i, e1, e2) -> eval e2 (bind r i (eval e1 r)) 
  |Fun(i, a) -> FunVal(i, a, r) 
  |Apply(f, eArg) -> 
      let fClosure = (eval f r) in
      (match fClosure with
       |FunVal(arg, fBody, fDecEnv) -> 
           eval fBody (bind fDecEnv arg (eval eArg r)) 
       |RecFunVal(g, (arg, fBody, fDecEnv)) -> 
           let aVal = (eval eArg r) in
           let rEnv = (bind fDecEnv g fClosure) in
           let aEnv = (bind rEnv arg aVal) in
           eval fBody aEnv 
       |_ -> failwith("non functional value"))
  |Letrec(f, funDef, letBody) ->
      (match funDef with
       |Fun(i, fBody) -> let r1 = (bind r f (RecFunVal(f, (i, fBody, r)))) 
           in
           eval letBody r1
       |  _ -> failwith("non functional def"))				
  | EmptySet (t)->let f q=
                    match q with
                    |"bool"->BooleanType
                    |"int"->IntegerType
                    |"string"->StringType
                    |_->failwith("not valid type")
      in Setval(Empty(f t))
  | Singleton (e,t)-> let f q=
                        match q with
                        |"bool"->BooleanType
                        |"int"->IntegerType
                        |"string"->StringType
                        |_->failwith("not valid type")
      in
      (let elem = eval e r in
       (if not (typecheck t elem) then failwith("Singleton: element has mismatching type.")
        else Setval(Set([elem], f t))))
                         
  | Of (t,l)-> let f q=
                 match q with
                 |"bool"->BooleanType
                 |"int"->IntegerType
                 |"string"->StringType
                 |_->failwith("not valid type")	
      in
      (let rec aux lis s=
         match lis with
         |[]->s
         |h::tail->aux tail (Setval(insert (eval h r) s)) 
       in
       aux l (Setval(Empty(f t))))	
  | BelongsTo (e, s)-> Bool(belongsto (eval e r) (eval s r))
  | Insert (e, s)-> Setval(insert (eval e r) (eval s r))
  | Remove (e, s)-> Setval(remove (eval e r) (eval s r))
  | Is_empty (s)-> Bool(is_empty (eval s r))
  | Is_subset (s1, s2)-> Bool(is_subset (eval s1 r) (eval s2 r))
  | Get_min (s)-> get_min (eval s r)
  | Get_max (s)-> get_max (eval s r)
  | Union (s1, s2)-> union (eval s1 r) (eval s2 r)
  | Intersection (s1, s2)-> intersection (eval s1 r) (eval s2 r)
  | Difference (s1, s2)-> difference (eval s1 r) (eval s2 r)
  | ForAll (p, s)->
      let peval= eval p r
      in
      (match peval with 
       |FunVal(arg,pbody,penv)->
           (match eval s r with
            |Setval(Set([],_))->Bool(true)
            |Setval(Empty(_))->Bool(true)
            |Setval(Set(l,_))->
                let env0 = emptyenv Unbound in
                let rec f lis=
                  match lis with
                  |[]->Bool(true)
                  |h::t-> let pval= eval pbody (bind penv arg (eval (evTToExp h) env0)) in
                      if (pval= Bool(true)) 
                      then f t
                      else (if (pval=Bool(false))
                            then Bool(false)
                            else failwith("not a predicate"))
                in 
                f l
            |_->failwith("not a set"))
       |_->failwith("not a function"))		
  |Exists (p, s)->
      let peval= eval p r
      in
      (match peval with
       |FunVal(arg,pbody,penv)->
           (match eval s r with
            |Setval(Set([],_))->Bool(true)
            |Setval(Empty(_))->Bool(true)
            |Setval(Set(l,_))->
                let env0 = emptyenv Unbound in
                let rec f lis=
                  match lis with
                  |[]->Bool(false)
                  |h::t-> let pval= eval pbody (bind penv arg (eval (evTToExp h) env0)) in
                      if (pval= Bool(false)) 
                      then f t
                      else (if (pval=Bool(true))
                            then Bool(true)
                            else failwith("not a predicate"))
                in 
                f l
            |_->failwith("not a set"))
       |_->failwith("not a function"))		
  |Filter (p, s)->
      let peval= eval p r
      in
      (match peval with
       |FunVal(arg,pbody,penv)->
           (match eval s r with
            |Setval(Set([],ty))->Setval(Set([],ty))
            |Setval(Empty(ty))->Setval(Empty(ty))
            |Setval(Set(l,ty))->
                let env0 = emptyenv Unbound in
                let rec f lis=
                  match lis with
                  |[]->[]
                  |h::t-> let pval= eval pbody (bind penv arg (eval (evTToExp h) env0)) in
                      if (pval= Bool(false)) 
                      then f t
                      else (if (pval=Bool(true))
                            then h::(f t)
                            else failwith("not a predicate"))
                in 
                Setval(Set(f l, ty))
            |_->failwith("not a set"))
       |_->failwith("not a function"))		
  |Map (p, s)->
      let peval= eval p r
      in
      (match peval with
       |FunVal(arg,pbody,penv)->
           (let seval = eval s r in
            match seval with
            |Setval(Set([],ty))->Setval(Set([],ty))
            |Setval(Empty(ty))->Setval(Empty(ty))
            |Setval(Set(l,ty))->
                let env0 = emptyenv Unbound in
                let rec f lis s=
                  match lis with
                  |[]->s
                  |h::t-> let pval= eval pbody (bind penv arg (eval (evTToExp h) env0)) in
                      f t (Setval(insert pval s))
                in 
                f l (Setval(Empty(ty)))
            |_->failwith("not a set")) 
       |_->failwith("not a predicate"))					
  |_->failwith("non functional def") ;;
 
(*testcases*)
 
let e0 = emptyenv Unbound ;;
(*expected: val e0 : '_weak1 -> evT = <fun>*)
let a = Of("int",[Eint 2; Eint 4; Eint 6]);;
(*expected:val a : exp = Of ("int", [Eint 2; Eint 4; Eint 6])*)
let f = Fun("n", Eq(Den "n", Eint(6))) ;;
(*expected:val f : exp = Fun ("n", Eq (Den "n", Eint 6))*)
let q = Fun("n", Eq(Den "n", Eint(9))) ;;
(*expected:val q : exp = Fun ("n", Eq (Den "n", Eint 9))*)
eval (Exists (q, a))(e0) ;;
(*expected:- : evT = Bool false*)
eval (Exists (f, a))(e0) ;;
(*expected:- : evT = Bool true*)
let w = Fun("n", Sum(Den "n", Eint(2))) ;;
(*expected:val w : exp = Fun ("n", Sum (Den "n", Eint 2))*)
eval (Map (w, a))(e0);;
(*expected:- : evT = Setval (Set ([Int 8; Int 6; Int 4], IntegerType))*)
let a = Of("int",[Eint 2; Eint 4; Eint 6; Eint 9; Eint 3; Eint 3]);;
(*expected:val a : exp = Of ("int", [Eint 2; Eint 4; Eint 6; Eint 9; Eint 3; Eint 3])*)
eval a e0 ;;
(*expected:- : evT = Setval (Set ([Int 3; Int 9; Int 6; Int 4; Int 2], IntegerType))*)
eval (Union (Map (w, a), a)) (e0);;
(*expected:-- : evT =Setval (Set ([Int 8; Int 11; Int 5; Int 3; Int 9; Int 6; Int 4; Int 2], IntegerType))*)
let b = Of("string",[Estring "cane"; Estring "gatto"; Estring "ornitorinco"; Estring "ornitorinco"; Estring "ornitorinco"; Estring "camaleonte"]);;
(*expected: val b : exp = Of ("string",[Estring "cane"; Estring "gatto"; Estring "ornitorinco"; Estring "ornitorinco"; Estring "ornitorinco"; Estring "camaleonte"])*)
eval b e0 ;;
(*expected: - : evT = Setval (Set ([String "camaleonte"; String "ornitorinco"; String "gatto"; String "cane"], StringType))*)
let z = Fun("n", Sum(Den "n", Eint(6))) ; ;;
(*expected: val z : exp = Fun ("n", Sum (Den "n", Eint 6))*)
eval (Map(z, a))(e0);;
(*expected: - : evT = Setval (Set ([Int 9; Int 15; Int 12; Int 10; Int 8], IntegerType))*) 
eval (EmptySet("string"))(e0) ;;
(*expected: - : evT = Setval (Empty StringType)*)
let c = Singleton (Eint 9, "int");;
(*expected: val c : exp = Singleton (Eint 9, "int")*)
eval c e0 ;;
(*expected: - : evT = Setval (Set ([Int 9], IntegerType))*)
let c = Insert(Eint 7, c) ;;
(*expected: val c : exp = Insert (Eint 7, Singleton (Eint 9, "int"))*)
eval c e0 ;;
(*expected: - : evT = Setval (Set ([Int 7; Int 9], IntegerType))*)
let c = Remove(Eint 9, c) ;;
(*expected: val c : exp = Remove (Eint 9, Insert (Eint 7, Singleton (Eint 9, "int")))*)
eval c e0 ;;
(*expected: - : evT = Setval (Set ([Int 7], IntegerType))*)
let c = Insert(Eint 15, c) ;;
(*expected: val c : exp = Insert (Eint 15, Remove (Eint 9, Insert (Eint 7, Singleton (Eint 9, "int"))))*)
let c = Insert(Eint 7, c) ;;
(*expected: val c : exp = Insert (Eint 7, Insert (Eint 15, Remove (Eint 9, Insert (Eint 7, Singleton (Eint 9, "int")))))*)
let c = Insert(Eint 1, c) ;;
(*expected: val c : exp = Insert (Eint 1, Insert (Eint 7, Insert (Eint 15, Remove (Eint 9, Insert (Eint 7, Singleton (Eint 9, "int"))))))*)
eval c e0 ;;
(*expected: - : evT = Setval (Set ([Int 1; Int 15; Int 7], IntegerType))*)
eval (BelongsTo(Eint 1, c))(e0) ;;
(*expected: - : evT = Bool true*)
eval (BelongsTo(Eint 0, c))(e0) ;;
(*expected: - : evT = Bool false*)
let d = Singleton (Estring "papera", "string");;
(*expected: val d : exp = Singleton (Estring "papera", "string")*)
let d = Remove (Estring "papera", d);;
(*expected: val d : exp = Remove (Estring "papera", Singleton (Estring "papera", "string"))*)
eval (Is_empty(d))(e0);;
(*expected: - : evT = Bool true*)
eval (Is_empty(c))(e0);;
(*expected: - : evT = Bool false*)
let d = Insert (Estring "papera", d);;
(*expected: - val d : exp = Insert (Estring "papera", Remove (Estring "papera", Singleton (Estring "papera", "string")))*)
let d = Insert (Estring "cane", d);;
(*expected: - val d : exp = Insert (Estring "cane", Insert (Estring "papera", Remove (Estring "papera", Singleton (Estring "papera", "string"))))*)
let d = Insert (Estring "anatra", d);;
(*expected: - val d : exp = Insert (Estring "anatra", Insert (Estring "cane", Insert (Estring "papera", Remove (Estring "papera", Singleton (Estring "papera", "string")))))*)
let d = Insert (Estring "oca", d);;
(*expected: - val d : exp = Insert (Estring "oca", Insert (Estring "anatra", Insert (Estring "cane", Insert (Estring "papera", Remove (Estring "papera", Singleton (Estring "papera", "string"))))))*)
eval (Get_min(d))(e0) ;;
(*expected: - - : evT = String "anatra"*)
eval (Get_max(d))(e0) ;;
(*expected: - - : evT = String "papera"*)
eval (Get_min(c))(e0) ;;
(*expected: - - : evT = Int 1*)
eval (Get_max(c))(e0) ;;
(*expected: - - : evT = Int 15*)
let q = EmptySet("int");;
(*expected: - val q : exp = EmptySet "int"*)
eval (BelongsTo(Estring "oca",q))(e0) ;;
(*expected: - - : evT = Bool false*)
let e = Of ("int", [Eint 2; Eint 6; Eint 9; Eint 3; Eint 3]) ;;
(*expected: -val e : exp = Of ("int", [Eint 2; Eint 6; Eint 9; Eint 3; Eint 3])*)
let f = Of ("string", [Estring "ornitorinco"; Estring "ornitorinco"; Estring "ornitorinco"; Estring "camaleonte"]) ;;
(*expected: -val f : exp = Of ("string", [Estring "ornitorinco"; Estring "ornitorinco"; Estring "ornitorinco"; Estring "camaleonte"])*)
eval (Intersection(a, Insert(Eint 99,Insert (Eint 38,e))))(e0) ;;
(*expected: - : evT = Setval (Set ([Int 2; Int 6; Int 9; Int 3], IntegerType))*)
eval (Intersection(a, Of ("int", [Eint 30; Eint 32; Eint 34; Eint 33])))(e0) ;;
(*expected: - : evT = Setval (Empty IntegerType)*)
eval (Difference(b, f))(e0) ;;
(*expected: - : evT = Setval (Set ([String "gatto"; String "cane"], StringType))*)
eval (Difference(a, e))(e0) ;;
(*expected: - : evT = Setval (Set ([Int 4], IntegerType))*)
let k = Of ("int", [Eint 2; Eint 6; Eint 8; Eint 0; Eint 12]) ;;
(*expected: val k : exp = Of ("int", [Eint 2; Eint 6; Eint 8; Eint 0; Eint 12])*)
let fn = Fun("n", Eq((Mod(Den "n", Eint(2)), Eint(0)))) ;;
(*expected: val fn : exp = Fun ("n", Eq (Mod (Den "n", Eint 2), Eint 0))*)
eval (ForAll(fn,a))(e0) ;;
(*expected: - : evT = Bool false*)
eval (ForAll(fn,k))(e0) ;;
(*expected: - : evT = Bool true*)
eval (Filter(fn,a))(e0) ;;
(*expected: - : evT = Setval (Set ([Int 6; Int 4; Int 2], IntegerType))*)
eval (Filter(fn,e))(e0) ;;
(*expected: - : evT = Setval (Set ([Int 6; Int 2], IntegerType))*)
eval (Filter(fn,k))(e0) ;;
(*expected: - : evT = Setval (Set ([Int 12; Int 0; Int 8; Int 6; Int 2], IntegerType))*)
eval (Is_subset (f, b))(e0) ;;
(*- : evT = Bool true*)
eval (Is_subset (k, a))(e0) ;;
(*- : evT = Bool false*)
eval (Is_subset (e, a))(e0) ;;
(*- : evT = Bool true*)