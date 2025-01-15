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