open System

// Function to create a list of integers from 1 to n
let rec range n =
    match n with
    | 1 -> [1] // base case
    | _ -> (range (n - 1)) @ [n] // using recursion to create the list

// Function to count the frequency of digits in a string
let countDigits str =
    str
    |> Seq.map string // converting the characters to strings
    |> Seq.countBy id // counting the frequency of each digit
    |> Map.ofSeq // converting the sequence to a map
    |> fun m -> [0..9] |> List.map (fun i -> Map.tryFind (string i) m |> Option.defaultValue 0) // using map to count the frequency of each digit

// Function to concatenate a list of integers into a single string
let concatInts nums =
    nums
    |> List.map string // converting the integers to strings
    |> String.concat "" // concatenating the strings

let rec processTestCases t =
    match t with
    | 0 -> () // base case
    | _ ->
        let n = Console.ReadLine() |> int // reading the input
        let sequence = range n |> concatInts // creating the sequence of integers
        let result = countDigits sequence // calling the function to count the frequency of digits
        printfn "%s" (String.Join(" ", result))// printing the result
        processTestCases (t - 1)// using recursion to process all test cases

let main () =
    let t = Console.ReadLine() |> int // reading the number of test cases
    processTestCases t // calling the function to process all test cases

main () // calling the main function
