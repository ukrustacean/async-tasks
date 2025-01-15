module async_tasks.Task2Promise

open System.Threading.Tasks
open System

let calculate n =
    task {
        return
            if n > 0 then
                n |> Math.Sqrt |> Ok
            else
                Error "Number should not be lesser then zero!"
    }

let waitAllTasks (l: Task<'a> list) =
    task {
        let mutable result = []

        for i in l do
            let! i' = i
            result <- i' :: result

        return result
    }

let promiseMap (mapping: 'a -> Task<Result<'ok, 'err>>) callback list =
    let either left right =
        function
        | Ok v -> left v
        | Error err -> right err

    let left a (l, r) = (a :: l, r)
    let right a (l, r) = (l, a :: r)

    task {
        let! list' = waitAllTasks <| List.map mapping list
        let results, errors = List.fold (fun s t -> either left right t s) ([], []) list'
        return callback results errors
    }

let task2 () =
    ([ 1; 2; 3; -4; 4; 5 ]
     |> promiseMap calculate (fun a b ->
         printfn $"Calculated: {String.Join(' ', Seq.toArray a)}"
         printfn $"Got errors: {String.Join(' ', Seq.toArray b)}"))
        .Wait()
