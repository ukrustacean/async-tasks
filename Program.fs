open System.Collections.Generic
open System.Threading
open System

let calculate n =
    if n > 0 then n |> Math.Sqrt |> Ok else Error "Number should not be lesser then zero!"

let callbackMap (mapping: 'a -> Result<'ok, 'err>) callback list =
    let results = new List<'ok>()
    let errors = new List<'err>()

    for item in list do
        let t =
            Thread(fun () ->
                item
                |> mapping
                |> function
                    | Ok v -> lock results (fun () -> results.Add v)
                    | Error e -> lock errors (fun () -> errors.Add e))

        t.Start()

    while results.Count + errors.Count <> List.length list do
        Thread.Sleep 100

    callback results errors

let main () =
    let l = [ 1; 2; 3; -4; 4; 5 ]

    l
    |> callbackMap calculate (fun a b ->
        printfn $"Calculated: {String.Join(' ', Seq.toArray a)}"
        printfn $"Got errors: {String.Join(' ', Seq.toArray b)}")
