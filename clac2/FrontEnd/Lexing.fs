module rec Clac2.FrontEnd.Lexing

open System.Collections.Generic
open Clac2.Core.Exc.Domain
open Clac2.Core.Exc.Exceptions
open Clac2.Core.Lang.Language
open Clac2.FrontEnd.NestedItems

let trimSplit (cs: char array) (s: string) =
    s.Split cs
    |> Array.map (fun x -> x.Trim())
    |> Array.filter (fun x -> x.Length > 0)
    |> Array.collect (fun x ->
        if x.Length = 1 then [| x |] else

        if Array.contains x[0] (Syntax.specialChars.ToCharArray()) then [| x[0].ToString(); x.Substring(1) |] else
        if Array.contains x.[x.Length-1] (Syntax.specialChars.ToCharArray()) then [| x.Substring(0, x.Length-1); x[x.Length-1].ToString() |] else
        [| x |]
    )

let trimSplitSimple (cs: char array) (s: string) =
    s.Split cs
    |> Array.map (fun x -> x.Trim())

let trimSplitIndexedArray cs arr =
    arr
    |> Array.collect (fun (i, x) -> trimSplitSimple cs x |> Array.map (fun y -> i, y))

let parseNestedByBrackets s = 
    let rec parseNestedByBracketsInner acc (splitS: string array) =
        if splitS.Length = 0 then 
            acc, splitS
        else if splitS[0] = ")"  then 
            acc, splitS[1..] // dont pass on the closing bracket
        else if splitS[0] = "(" then 
            let (innerAcc, rest) = parseNestedByBracketsInner [||] splitS[1..]
            parseNestedByBracketsInner (Array.append acc [|NestedArray innerAcc|]) rest
        else
            parseNestedByBracketsInner (Array.append acc [|NestedItem splitS[0]|]) splitS[1..]

    let maybeParanError: SimpleExc option = validateParantheses s
    if maybeParanError.IsSome then Error maybeParanError.Value else

    s 
    |> trimSplit [| ' ' |]
    |> parseNestedByBracketsInner [||]
    |> fst
    |> Ok

let validateParantheses s  =
    let mutable stack = Stack<char>()

    s
    |> Seq.fold (fun (maybeExc: SimpleExc option) c ->
        if maybeExc.IsSome then maybeExc else
        if stack.Count = 0 && c = ')' then Some(Simple.Exc (sprintf "Too many closing parantheses: %c" c)) 
        else

            if c = '(' then stack.Push c
            else if c = ')' then stack.Pop() |> ignore

            None
    ) None
    |> Option.orElse (if stack.Count = 0 then None else Some(Simple.Exc (sprintf "Unclosed parantheses: %A" stack)))
