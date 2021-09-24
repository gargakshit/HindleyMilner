open HindleyMilner.Utils

type Literal =
    | LInt of int
    | LBool of bool

let literalToString =
    function
    | LInt i -> $"%d{i}"
    | LBool b -> $"%b{b}"

type Term =
    | Literal of Literal
    | Variable of string
    | Lambda of Argument: string * Body: Term
    | Apply of Function: Term * Argument: Term

let rec termToString =
    function
    | Literal lit -> literalToString lit
    | Variable name -> name
    | Lambda (name, body) -> $"(fn {name} => {termToString body})"
    | Apply (fn, arg) -> $"({termToString fn} {termToString arg})"

type Type =
    | TInt
    | TBool
    | TFunction of Argument: Type * Return: Type
    | TVariable of string

let rec typeToString =
    function
    | TInt -> "int"
    | TBool -> "bool"
    | TFunction (arg, ret) -> $"%s{typeToString arg} -> %s{typeToString ret}"
    | TVariable name -> $"'%s{name}"

type TypeScheme = { Type: Type; Instances: string list }
type TypeEnv = Map<string, TypeScheme>

let rec freeTypeVariables =
    function
    | TInt
    | TBool -> Set.empty
    | TVariable name -> Set.singleton name
    | TFunction (arg, ret) ->
        let var1 = freeTypeVariables arg
        let var2 = freeTypeVariables ret
        Set.union var1 var2

/// Generates a unique type variable.
/// Stateful
let newTypeVariable =
    let index = ref 'a'
    let wrap = ref 0

    fun () ->
        let suffix = if !wrap = 0 then "" else $"%d{!wrap}"
        let typeVar = $"%c{!index}%s{suffix}"

        if !index = 'z' then
            index := 'a'
            wrap := !wrap + 1
        else
            index := char ((int !index) + 1)

        TVariable typeVar

let rec substituteType type' (scheme: Map<string, Type>) =
    match type' with
    | TVariable name as var ->
        match scheme.TryFind name with
        | Some t -> t
        | None -> var
    | TFunction (arg, ret) -> TFunction((substituteType arg scheme), (substituteType ret scheme))
    | type' -> type'

/// Replace all bound type variables with fresh variables
let instantiate scheme =
    List.map (fun _ -> newTypeVariable ()) scheme.Instances
    |> Seq.zip scheme.Instances
    |> Map
    |> substituteType scheme.Type

/// Merges `map` and `map'`
let mergeMaps map map' =
    Map.fold (fun acc key value -> Map.add key value acc) map map'

let substituteTypeScheme scheme substituteMap =
    { scheme with
          Type =
              (substituteType
                  scheme.Type
                  (List.fold (fun xs x -> Map.remove x xs) substituteMap scheme.Instances)) }

let mergeEnv env env' =
    Map.map (fun _ v -> substituteTypeScheme v env') env

/// Uses `s` to apply type substitutions to `s'` and then merges the result
/// with `s`
let mergeConstraints s s' =
    mergeMaps (Map.map (fun _ value -> substituteType value s) s') s

let generateVariableConstraints name =
    function
    | TVariable name' when name = name' -> Ok Map.empty
    | t when (freeTypeVariables t).Contains name ->
        Error $"Occur check for {name} and {typeToString t} failed"
    | t -> Ok(Map [ name, t ])

/// unify tries to solve the type constraints for `t` and `t'`
let rec unify t t' =
    result {
        match t, t' with
        | TFunction (arg, ret), TFunction (arg', ret') ->
            let! constraints = unify arg arg'

            let! constraints' =
                unify (substituteType ret constraints) (substituteType ret' constraints)

            return Ok(mergeConstraints constraints constraints')
        | type', TVariable name
        | TVariable name, type' -> return (generateVariableConstraints name type')
        | TInt, TInt -> return Ok(Map.empty)
        | TBool, TBool -> return Ok(Map.empty)
        | _ -> return (Error $"Types {typeToString t} and {typeToString t'} do not unify")
    }

let rec typeInferenceWithSubstitutions (env: TypeEnv) term =
    result {
        match term with
        | Variable name ->
            match env.TryFind name with
            | None -> return Error $"Unbound Variable %s{name}"
            | Some sigma -> return Ok((Map.empty, (instantiate sigma)))
        | Literal l ->
            match l with
            | LInt _ -> return Ok((Map.empty, TInt))
            | LBool _ -> return Ok((Map.empty, TBool))
        | Apply (fn, arg) ->
            let typeVariable = newTypeVariable ()
            let! fnEnv, fnType = typeInferenceWithSubstitutions env fn
            let! argEnv, argType = typeInferenceWithSubstitutions (mergeEnv env fnEnv) arg

            match unify (substituteType fnType argEnv) (TFunction(argType, typeVariable)) with
            | Error e -> return Error e
            | Ok env' ->
                return
                    Ok(
                        ((mergeMaps (mergeMaps env' argEnv) fnEnv),
                         (substituteType typeVariable env'))
                    )
        | Lambda (arg, ret) ->
            let typeVariable = newTypeVariable ()
            let typeSchema = { Type = typeVariable; Instances = [] }

            let env' =
                env
                |> Map.remove arg
                |> mergeMaps (Map [ arg, typeSchema ])

            let! retEnv, retType = typeInferenceWithSubstitutions env' ret

            return Ok((retEnv, TFunction((substituteType typeVariable retEnv), retType)))
    }

let typeInference env term =
    match typeInferenceWithSubstitutions env term with
    | Error e -> Error e
    | Ok (env', type') -> Ok(substituteType type' env')

let initialEnv =
    Map [ "+",
          { Type = TFunction(TInt, TFunction(TInt, TInt))
            Instances = [] }
          "*",
          { Type = TFunction(TInt, TFunction(TInt, TInt))
            Instances = [] } ]

let testFor term =
    printfn $"Term: {termToString term}"

    match typeInference initialEnv term with
    | Error e -> printfn $"Inference error: {e}"
    | Ok type' -> printfn $"Inferred type: {typeToString type'}"

    printfn ""

[<EntryPoint>]
let main _argv =
    let terms =
        [
          // ((fn x => x) 1)
          Apply(Lambda("x", (Variable "x")), Literal(LInt 1))
          // ((fn x => x) true)
          Apply(Lambda("x", (Variable "x")), Literal(LBool true))
          // +
          Variable("+")
          // (+ 1)
          Apply(Variable("+"), Literal(LInt 1))
          // *
          Variable("*")
          // (* 1)
          Apply(Variable("*"), Literal(LInt 1))
          // (fn x => ((+ x) 1))
          Lambda("x", Apply(Apply(Variable("+"), Variable("x")), Literal(LInt 1)))
          // ((+ 1) false)
          Apply(Apply(Variable("+"), Literal(LInt 1)), Literal(LBool false))
          // (fn x => x)
          Lambda("x", Variable("x")) ]

    Seq.iter testFor terms

    0
