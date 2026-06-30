type ide = string;;

(* Valori che rappresentano tipi *)
type tval =
  | TInt
  | TBool
  | TString
  | TSet of tval
  | TFun of tval * tval
  | TUnbound

type set = 
  | Empty of tval
  | Set of tval list * tval
           
(* Albero di Sintassi Astratta *)
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
  | Fun of ide * tval * exp 
  | Apply of exp * exp 
  | Letrec of ide * ide * tval * tval * exp * exp
  | EmptySet of tval
  | Singleton of exp * tval
  | Of of tval * (exp list)
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


(*ambiente e operazioni sull'ambiente*)
type 't tenv = (string * 't) list;;

let emptyEnv = [("", TUnbound)];;

let bind (s:tval tenv) (i:string) (x:tval) = (i,x) :: s;;

let rec lookup (s:tval tenv) (i:string) =
  match s with
  | [] ->  TUnbound
  | (j,v)::sl when j = i -> v
  | _::sl -> lookup sl i;;


(*typechecker per set*)
let setTypeCheck t =
  match t with
  | TInt | TBool | TString -> t
  | _ -> failwith("typeSwitch: not a valid type");;
(*typechecker che da true se il tipo e' valido per i set*)
let setTypeCheckBool t =
  match t with
  | TInt | TBool | TString -> true
  | _ -> false;;

(* Interprete per la valutazione del tipo delle espressioni *)
let rec teval (e:exp) (s:tval tenv) =
  match e with
  | Eint(n) -> TInt
  | Ebool(b) -> TBool
  | Estring(x) -> TString
  | IsZero  a ->
      (match (teval a s) with
       | TInt -> TBool
       | _ -> failwith("IsZero:not a valid type"))
  | Den(i) -> lookup s i
  | Eq(e1,e2) -> if (teval e1 s) = (teval e2 s) then TBool else failwith("Eq:not a valid type")
  | Prod(e1,e2) | Sum(e1,e2) | Diff(e1,e2) ->
      (match ((teval e1 s),(teval e2 s)) with
       | (TInt, TInt) -> TInt
       | (_,_) -> failwith("Prod/Sum/Sub:not a valid type"))
  | Minus a ->
      (match (teval a s) with
       | TInt -> TInt
       | _ -> failwith("Minus:not a valid type"))
  | And(a, b) ->
      (match ((teval a s),(teval b s)) with
       | (TBool, TBool) -> TBool
       | (_,_) -> failwith("And:not a valid type"))
  | Or(a, b) ->
      (match ((teval a s),(teval b s)) with
       | (TBool, TBool) -> TBool
       | (_,_) -> failwith("Or:not a valid type"))
  | Mod(a, b) ->
      (match ((teval a s),(teval b s)) with
       | (TBool, TBool) -> TBool
       | (_,_) -> failwith("Mod:not a valid type"))
  | Not a ->
      (match (teval a s) with
       | TBool -> TBool
       | _ -> failwith("Not:not a valid type"))
  | Ifthenelse(e1,e2,e3) ->
      (match ((teval e1 s), ((teval e2 s)=(teval e3 s))) with
       | (TBool, true) -> teval e2 s
       | (_,_) -> failwith("Ifthenelse:non boolean guard or expressions of different types"))
  | Let(i, e, ebody) -> teval ebody (bind s i (teval e s))
  | Fun(arg, aType, ebody) -> TFun(aType, teval ebody (bind s arg aType))
  | Apply(eF, eArg) ->
      (match teval eF s with 
       | TFun(t1,t2) -> if t1 = (teval eArg s) then t2 else failwith("Apply:not a valid type")
       | _ -> failwith("Apply:non functional type"))
  | Letrec(f, arg, aType, rType, fBody, lBody) ->
      let fEnv = bind s f (TFun(aType,rType)) in
      let aEnv = bind fEnv arg aType in
      let t = teval fBody aEnv in
      if t = (teval lBody aEnv) then t else failwith("Letrec:not a valid type")
  | EmptySet (t)-> TSet(setTypeCheck t)
  | Singleton (e, t)->
      if (teval e s) = setTypeCheck t
      then TSet(t)
      else failwith("Singleton:not a valid type")
  | Of (t, l)->
      let rec aux lis en ty=
        match lis with
        |[]->true
        |h::tl->if (teval h en) = ty
            then aux tl en ty 
            else false
        |_-> failwith("Of:not a valid type")
      in
      (match l with
       |[]->if setTypeCheckBool t
           then TSet(t)
           else failwith("Of:not a valid type")
       |h::tl->if (aux l s (setTypeCheck t))
           then TSet(t) 
           else failwith("Of:not a valid type"))
  | BelongsTo (e, st)->
      (match (teval e s, teval st s) with
       | (t, TSet(t')) -> if t = t' then TBool else failwith("BelongsTo:not a valid type")
       | (_,_) -> failwith("ExistsIn:not a valid type"))
  | Insert (e, st)->
      (match (teval e s, teval st s) with
       | (t, TSet(t')) -> if t = t' then TSet(t') else failwith("Insert:not a valid type")
       | (_,_) -> failwith("ExistsIn:not a valid type"))
  | Remove (e, st)->
      (match (teval e s, teval st s) with
       | (t, TSet(t')) -> if t = t' then TSet(t') else failwith("Remove:not a valid type")
       | (_,_) -> failwith("ExistsIn:not a valid type"))
  | Is_empty (st)-> 
      (match teval st s with
       | TSet(t') -> TBool
       | _ -> failwith("Is_empty:not a valid type"))
  | Is_subset (s1, s2)->
      (match (teval s1 s, teval s2 s) with
       | (TSet(t), TSet(t')) -> if t = t' then TBool else failwith("Is_subset:not a valid type")
       | (_,_) -> failwith("Is_subset:not a valid type"))
  | Get_min (st)->
      (match teval st s with
       | TSet(t') -> t'
       | _ -> failwith("Get_min:not a valid type"))
  | Get_max (st)->
      (match teval st s with
       | TSet(t') -> t'
       | _ -> failwith("Get_max:not a valid type"))
  | Union (s1, s2)->
      (match (teval s1 s, teval s2 s) with
       | (TSet(t), TSet(t')) -> if t = t' then TSet(t') else failwith("Union:not a valid type")
       | (_,_) -> failwith("Union:not a valid type"))
  | Intersection (s1, s2)->
      (match (teval s1 s, teval s2 s) with
       | (TSet(t), TSet(t')) -> if t = t' then TSet(t') else failwith("Intersection:not a valid type")
       | (_,_) -> failwith("Intersection:not a valid type"))
  | Difference (s1, s2)->
      (match (teval s1 s, teval s2 s) with
       | (TSet(t), TSet(t')) -> if t = t' then TSet(t') else failwith("Difference:not a valid type")
       | (_,_) -> failwith("Difference:not a valid type"))
  | ForAll (p, st)->
      (match (teval p s, teval st s) with
       | (TFun(t1,t2), TSet(t)) ->
           if (t = t1) && (t2 = TBool) then TBool
           else failwith("ForAll:not a valid type")
       | (_,_) -> failwith("For_all:not a valid type"))
  | Exists (p, st)->
      (match (teval p s, teval st s) with
       | (TFun(t1,t2), TSet(t)) ->
           if (t = t1) && (t2 = TBool) then TBool
           else failwith("Exists:not a valid type")
       | (_,_) -> failwith("Exists:not a valid type"))
  | Filter (p, st)->
      (match (teval p s, teval st s) with
       | (TFun(t1,t2), TSet(t)) ->
           if (t = t1) && (t2 = TBool) then TSet(t)
           else failwith("Filter:not a valid type")
       | (_,_) -> failwith("Filter:not a valid type"))
  | Map (p, st)->
      (match (teval p s, teval st s) with
       | (TFun(t1,t2), TSet(t)) ->
           if (t = t1) then TSet(t)
           else failwith("Map:not a valid type")
       | (_,_) -> failwith("Map:not a valid type"));;

(*testcase*)
let env0 = emptyEnv;;
(*expected: val env0 : (string * tval) list = [("", TUnbound)]*)
let g= Fun("x", TInt, Eq(Den("x"), Eint(2))) ;;
 (*expected: val g : exp = Fun ("x", TInt, Eq (Den "x", Eint 2))*)
let f= Fun("x", TInt, Prod(Den("x"), Eint(2))) ;;
 (*expected: val f : exp = Fun ("x", TInt, Prod (Den "x", Eint 2))*)
let a = Of(TInt,[Eint 2; Eint 4; Eint 6; Eint 9; Eint 3; Eint 3]);;
 (*expectrd: val a : exp = Of (TInt, [Eint 2; Eint 4; Eint 6; Eint 9; Eint 3; Eint 3])*)
let b = Singleton(Eint 7, TInt) ;;
 (*expected: val b : exp = Singleton (Eint 7, TInt)*)
let c = EmptySet(TInt);;
 (*expected: val c : exp = EmptySet TInt*)
teval a env0 ;;
 (*expected: - : tval = TSet TInt*)
teval b env0 ;;
 (*expected: - : tval = TSet TInt*)
teval c env0 ;;
 (*expected: - : tval = TSet TInt*)
teval (Insert(Eint 6, b))(env0) ;;
 (*expected: - : tval = TSet TInt*)
teval (Remove(Eint 6, c))(env0) ;;
 (*expected: - : tval = TSet TInt*)
teval (BelongsTo(Eint 6, a))(env0) ;;
 (*expected: - : tval = TBool*)
teval (Is_empty a)(env0) ;;
 (*expected: - : tval = TBool*)
teval (Get_min(a))(env0) ;;
 (*expected: - : tval = TInt*)
teval (Get_max(b))(env0) ;;
 (*expected: - : tval = TInt*)
teval (Is_subset(a,b))(env0) ;;
 (*expected: - : tval = TBool*)
teval (Union(a,b))(env0) ;;
 (*expected: - : tval = TSet TInt*)
teval (Intersection(b,c))(env0) ;;
 (*expected: - : tval = TSet TInt*)
teval (Difference(b,c))(env0) ;;
 (*expected: - : tval = TSet TInt*)
teval (ForAll(g, a))(env0) ;;
 (*expected: - : tval = TBool*)
teval (Exists(g, a))(env0) ;;
 (*expected: - : tval = TBool*)
teval (Filter(g, a))(env0) ;;
 (*expected: - : tval = TSet TInt*)
teval (Map(f, a))(env0) ;;
(*expected: - : tval = TSet TInt*)