[<EntryPoint>]
let main argv = 

    let listToSort = [45; 894; 12; 4546; 2; 454968; 11; 2; 6; 9; 33; 78451]
    printfn "Quicksort over list %A" listToSort

    let sorted = Quicksort.quicksort listToSort
    printfn "Sorted list: %A" sorted

    let intUpper = 100
    let intSum = Squares.sumOfSquares intUpper
    printfn "Sum of Squares from 1 to %d: %d" intUpper intSum
    
    let floatUpper = 99.5
    let floatSum = Squares.sumOfSquaresF floatUpper
    printfn "Sum of Squares from 1 to %f: %F" floatUpper floatSum
    
    //let input = scanf

    0 // return an integer exit code
