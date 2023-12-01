open System.IO
open FSharpPlus

let wrodToNumeral message =
    message
    |> String.replace "zero" "0"
    |> String.replace "one" "1"
    |> String.replace "two" "2"
    |> String.replace "three" "3"
    |> String.replace "four" "4"
    |> String.replace "five" "5"
    |> String.replace "six" "6"
    |> String.replace "seven" "7"
    |> String.replace "eight" "8"
    |> String.replace "nine" "9"

let parseMessage message =
    let msg = ""

    let rec messageInternal message msg =
        let firstChar = String.tryHead message
        let messageNew = message.[1..]

        match firstChar with
        | Some c -> string msg + string c |> wrodToNumeral |> messageInternal messageNew
        | None -> msg

    messageInternal message msg

let loadFile filePath = File.ReadAllLines(filePath)

let filterOutNonNumbers x : char seq =
    x
    |> Seq.filter (fun x -> Seq.contains x [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' |])

let getNo x =
    let last = Seq.last x
    let first = Seq.head x
    string first + string last |> int


let part1 filePath =
    loadFile filePath
    |> Array.map (fun x -> filterOutNonNumbers x)
    |> Array.map (fun x -> getNo x)
    |> Array.sum

let part2 filePath =
    loadFile filePath
    |> Array.map (fun x -> parseMessage x)
    |> Array.map (fun x -> filterOutNonNumbers x)
    |> Array.map (fun x -> getNo x)
    |> Array.sum


[<EntryPoint>]
let main _ =

    let part1ExampleAns = part1 @"./data/part1Example.txt"
    assert (part1ExampleAns = 142)
    let part1Ans = part1 @"./data/input.txt"
    printfn "Answer for part 1 is %i" part1Ans

    let part2ExampleAns = part2 @"./data/part2Example.txt"
    assert (part2ExampleAns = 281)

    let part2Ans = part2 @"./data/input.txt"
    printfn "Answer for part 2 is %i" part2Ans
    0
