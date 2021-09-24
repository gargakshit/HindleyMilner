module HindleyMilner.Utils

type ResultBuilder() =
    member this.Bind(x, f) =
        match x with
        | Error _ as err -> err
        | Ok x' -> f x'
        
    member this.Return(x) = x

let result = ResultBuilder()
