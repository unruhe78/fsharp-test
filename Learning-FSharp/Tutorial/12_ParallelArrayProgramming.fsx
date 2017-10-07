// *******************************************************************************************************
//   To execute the code in F# Interactive, highlight a section of code and press Alt-Enter or right-click 
//   and select "Execute in Interactive".  You can open the F# Interactive Window from the "View" menu. 
// *******************************************************************************************************

/// The FSharp.Core library defines a range of parallel processing functions.  Here
/// you use some functions for parallel processing over arrays.
///
/// To learn more, see: https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/array.parallel-module-%5Bfsharp%5D
module ParallelArrayProgramming =
              
    /// First, an array of inputs.
    let oneBigArray = [| 0 .. 100000 |]
    
    // Next, define a functions that does some CPU intensive computation.
    let rec computeSomeFunction x = 
        if x <= 2 then 1 
        else computeSomeFunction (x - 1) + computeSomeFunction (x - 2)
       
    // Next, do a parallel map over a large input array.
    let computeResults() = 
        oneBigArray 
        |> Array.Parallel.map (fun x -> computeSomeFunction (x % 20))

    // Next, print the results.
    printfn "Parallel computation results: %A" (computeResults())
