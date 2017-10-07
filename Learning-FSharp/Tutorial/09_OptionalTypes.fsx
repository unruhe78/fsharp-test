// *******************************************************************************************************
//   To execute the code in F# Interactive, highlight a section of code and press Alt-Enter or right-click 
//   and select "Execute in Interactive".  You can open the F# Interactive Window from the "View" menu. 
// *******************************************************************************************************

/// Option values are any kind of value tagged with either 'Some' or 'None'.
/// They are used extensively in F# code to represent the cases where many other
/// languages would use null references.
///
/// To learn more, see: https://docs.microsoft.com/en-us/dotnet/articles/fsharp/language-reference/options
module OptionValues = 

    /// First, define a zipcode defined via Single-case Discriminated Union.
    type ZipCode = ZipCode of string

    /// Next, define a type where the ZipCode is optionsl.
    type Customer = { ZipCode: ZipCode option }

    /// Next, define an interface type the represents an object to compute the shipping zone for the customer's zip code, 
    /// given implementations for the 'getState' and 'getShippingZone' abstract methods.
    type ShippingCalculator =
        abstract GetState : ZipCode -> string option
        abstract GetShippingZone : string -> int

    /// Next, calculate a shipping zone for a customer using a calculator instance.
    /// This uses combinators in the Option module to allow a functional pipeline for
    /// transforming data with Optionals.
    let CustomerShippingZone (calculator: ShippingCalculator, customer: Customer) =
        customer.ZipCode 
        |> Option.bind calculator.GetState 
        |> Option.map calculator.GetShippingZone
