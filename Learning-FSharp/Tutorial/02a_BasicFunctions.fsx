// *******************************************************************************************************
//   To execute the code in F# Interactive, highlight a section of code and press Alt-Enter or right-click 
//   and select "Execute in Interactive".  You can open the F# Interactive Window from the "View" menu. 
// *******************************************************************************************************

/// Modules are the primary way to organize functions and values in F#.
/// Much of F# programming consists of defining functions that transform input data to produce
/// useful results.
///
/// To learn more, see: https://docs.microsoft.com/en-us/dotnet/articles/fsharp/language-reference/functions/
module BasicFunctions = 

    /// You use 'let' to define a function. This one accepts an integer argument and returns an integer. 
    /// Parentheses are optional for function arguments, except for when you use an explicit type annotation.
    let sampleFunction1 x = x*x + 3

    /// Apply the function, naming the function return result using 'let'. 
    /// The variable type is inferred from the function return type.
    let result1 = sampleFunction1 4573

    // This line uses '%d' to print the result as an integer. This is type-safe.
    // If 'result1' were not of type 'int', then the line would fail to compile.
    printfn "The result of squaring the integer 4573 and adding 3 is %d" result1

    /// When needed, annotate the type of a parameter name using '(argument:type)'.  Parentheses are required.
    let sampleFunction2 (x:int) = 2*x*x - x/5 + 3

    let result2 = sampleFunction2 (7 + 4)
    printfn "The result of applying the 1st sample function to (7 + 4) is %d" result2

    /// Conditionals use if/then/elid/elif/else.
    ///
    /// Note that F# uses whitespace indentation-aware syntax, similar to languages like Python.
    let sampleFunction3 x = 
        if x < 100.0 then 
            2.0*x*x - x/5.0 + 3.0
        else 
            2.0*x*x + x/5.0 - 37.0

    let result3 = sampleFunction3 (6.5 + 4.5)

    // This line uses '%f' to print the result as a float.  As with '%d' above, this is type-safe.
    printfn "The result of applying the 2nd sample function to (6.5 + 4.5) is %f" result3
