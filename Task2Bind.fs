module async_tasks.Task2Bind

open System.Threading.Tasks
open System

type Task<'a> with
    member this.Bind(f) =
        task {
            let! t = this
            return! f t
        }

let pureTask x = task { return x }

let calculate n =
    task {
        return
            if n > 0 then
                n |> Math.Sqrt |> Ok
            else
                Error "Number should not be lesser then zero!"
    }

let waitAllTasks (l: Task<'a> list) =
    let mutable result = pureTask []

    for i in l do
        result <- i.Bind(fun i' -> result.Bind(fun result' -> pureTask <| i' :: result'))

    result

let promiseMap (mapping: 'a -> Task<Result<'ok, 'err>>) callback list =
    let either left right =
        function
        | Ok v -> left v
        | Error err -> right err

    let left a (l, r) = (a :: l, r)
    let right a (l, r) = (l, a :: r)

    (waitAllTasks <| List.map mapping list)
        .Bind(fun list' ->
            let results, errors = List.fold (fun s t -> either left right t s) ([], []) list'
            pureTask <| callback results errors)

let task2 () =
    ([ 1; 2; 3; -4; 4; 5 ]
     |> promiseMap calculate (fun a b ->
         printfn $"Calculated: {String.Join(' ', Seq.toArray a)}"
         printfn $"Got errors: {String.Join(' ', Seq.toArray b)}"))
        .Wait()
