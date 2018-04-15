namespace Commons.Types

/// <summary>
/// Represents an operation result where the operation either succeeded or failed
/// </summary>
/// <typeparam name="'TSuccess">Result type if the operation succeeded</typeparam>
/// <typeparam name="'TError">Error type if the operation failed</typeparam>
type OperationResult<'TSuccess, 'TError> =
    /// <summary>
    /// Provides the operation result if the operation succeeded
    /// </summary>
    | Success of 'TSuccess
    /// <summary>
    /// Provides the error result if the operation failed
    /// </summary>
    | Error of 'TError
    /// <summary>
    /// Gets the unwrapped success value
    /// </summary>
    /// <exception cref="System.Exception">Thrown if the operation failed</exception>
    member result.SuccessValue = 
        match result with
        | Success value -> value
        | Error error   -> failwithf "Operation failed with error \"%A\"" error
    /// <summary>
    /// Gets the unwrapped error value
    /// </summary>
    /// <exception cref="System.Exception">Thrown if the operation succeeded</exception>
    member result.ErrorValue =
        match result with
        | Success value -> failwithf "Operation succeeded with result \"%A\"" value
        | Error error   -> error

/// <summary>
/// Basic operations on <seealso cref="OperationResult" />
/// </summary>
module OperationResult =
    /// <summary>
    /// Builds a new <seealso cref="OperationResult" /> by returning the given value as <seealso cref="Success" />
    /// </summary>
    /// <param name="value">value to wrap into <seealso cref="Success" /></param>
    let returnSuccess value = Success value

    /// <summary>
    /// Builds a new <seealso cref="OperationResult" /> by returning the given value as <seealso cref="Error" />
    /// </summary>
    /// <param name="value">value to wrap into <seealso cref="Success" /></param>
    let returnError error = Error error

    /// <summary>
    /// Builds a new <seealso cref="OperationResult" /> by applying the function from the given <seealso cref="Success" /> value of the operation result 'func' to the <seealso cref="Success" /> value of the operation result 'result'
    /// </summary>
    /// <param name="func"><seealso cref="OperationResult" /> containing the function to apply as <seealso cref="Success" /> value</param>
    /// <param name="result"><seealso cref="OperationResult" /> containing the input parameter of 'func' as <seealso cref="Success" /> value</param>
    let apply func result =
        match func, result with
        | Success fn, Success value -> Success (value |> fn)
        | _         , Error value   -> Error value
        | Error fn  , _             -> Error fn
    

    /// <summary>
    /// Builds a new <seealso cref="OperationResult" /> by applying the given function to the <seealso cref="Success" /> value of the operation result 'result'
    /// </summary>
    /// <param name="func">The function to apply</param>
    /// <param name="result"><seealso cref="OperationResult" /> containing the input parameter of 'func' as <seealso cref="Success" /> value</param>
    let bind func result =
        match result with
        | Success value -> (value |> func)
        | Error error   -> Error error

    /// <summary>
    /// Builds a new <seealso cref="OperationResult" /> whose result is the result of applying the given function to the <seealso cref="Success" /> value of the operation result 'result'
    /// </summary>
    /// <param name="func">The function to apply</param>
    /// <param name="result"><seealso cref="OperationResult" /> containing the input parameter of 'func' as <seealso cref="Success" /> value</param>
    let map func result = 
        match result with
        | Success value -> Success (value |> func)
        | Error error   -> Error error
