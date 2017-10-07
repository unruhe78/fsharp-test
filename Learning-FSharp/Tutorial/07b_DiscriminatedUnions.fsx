// *******************************************************************************************************
//   To execute the code in F# Interactive, highlight a section of code and press Alt-Enter or right-click 
//   and select "Execute in Interactive".  You can open the F# Interactive Window from the "View" menu. 
// *******************************************************************************************************

/// Discriminated Unions (DU for short) are values which could be a number of named forms or cases.
/// Data stored in DUs can be one of several distinct values.
///
/// To learn more, see: https://docs.microsoft.com/en-us/dotnet/articles/fsharp/language-reference/discriminated-unions
module DiscriminatedUnions = 

    /// The following represents the suit of a playing card.
    type Suit = 
        | Hearts 
        | Clubs 
        | Diamonds 
        | Spades

    /// A Disciminated Union can also be used to represent the rank of a playing card.
    type Rank = 
        /// Represents the rank of cards 2 .. 10
        | Value of int
        | Ace
        | King
        | Queen
        | Jack

        /// Discriminated Unions can also implement object-oriented members.
        static member GetAllRanks() = 
            [ yield Ace
              for i in 2 .. 10 do yield Value i
              yield Jack
              yield Queen
              yield King ]
                                   
    /// This is a record type that combines a Suit and a Rank.
    /// It's common to use both Records and Disciminated Unions when representing data.
    type Card = { Suit: Suit; Rank: Rank }
              
    /// This computes a list representing all the cards in the deck.
    let fullDeck = 
        [ for suit in [ Hearts; Diamonds; Clubs; Spades] do
              for rank in Rank.GetAllRanks() do 
                  yield { Suit=suit; Rank=rank } ]

    /// This example converts a 'Card' object to a string.
    let showPlayingCard (c: Card) = 
        let rankString = 
            match c.Rank with 
            | Ace -> "Ace"
            | King -> "King"
            | Queen -> "Queen"
            | Jack -> "Jack"
            | Value n -> string n
        let suitString = 
            match c.Suit with 
            | Clubs -> "clubs"
            | Diamonds -> "diamonds"
            | Spades -> "spades"
            | Hearts -> "hearts"
        rankString  + " of " + suitString

    /// This example prints all the cards in a playing deck.
    let printAllCards() = 
        for card in fullDeck do 
            printfn "%s" (showPlayingCard card)

    // Single-case DUs are often used for domain modeling.  This can buy you extra type safety
    // over primitive types such as strings and ints.
    //
    // Single-case DUs cannot be implicitly converted to or from the type they wrap.
    // For example, a function which takes in an Address cannot accept a string as that input,
    // or vive/versa.
    type Address = Address of string
    type Name = Name of string
    type SSN = SSN of int

    // You can easily instantiate a single-case DU as follows.
    let address = Address "111 Alf Way"
    let name = Name "Alf"
    let ssn = SSN 1234567890

    /// When you need the value, you can unwrap the underlying value with a simple function.
    let unwrapAddress (Address a) = a
    let unwrapName (Name n) = n
    let unwrapSSN (SSN s) = s

    // Printing single-case DUs is simple with unwrapping functions.
    printfn "Address: %s, Name: %s, and SSN: %d" (address |> unwrapAddress) (name |> unwrapName) (ssn |> unwrapSSN)

    /// Disciminated Unions also support recursive definitions.
    ///
    /// This represents a Binary Search Tree, with one case being the Empty tree,
    /// and the other being a Node with a value and two subtrees.
    type BST<'T> =
        | Empty
        | Node of value:'T * left: BST<'T> * right: BST<'T>

    /// Check if an item exists in the binary search tree.
    /// Searches recursively using Pattern Matching.  Returns true if it exists; otherwise, false.
    let rec exists item bst =
        match bst with
        | Empty -> false
        | Node (x, left, right) ->
            if item = x then true
            elif item < x then (exists item left) // Check the left subtree.
            else (exists item right) // Check the right subtree.

    /// Inserts an item in the Binary Search Tree.
    /// Finds the place to insert recursively using Pattern Matching, then inserts a new node.
    /// If the item is already present, it does not insert anything.
    let rec insert item bst =
        match bst with
        | Empty -> Node(item, Empty, Empty)
        | Node(x, left, right) as node ->
            if item = x then node // No need to insert, it already exists; return the node.
            elif item < x then Node(x, insert item left, right) // Call into left subtree.
            else Node(x, left, insert item right) // Call into right subtree.

    /// Discriminated Unions can also be represented as structs via the 'Struct' attribute.
    /// This is helpful in situations where the performance of structs outweighs
    /// the flexibility of reference types.
    ///
    /// However, there are two important things to know when doing this:
    ///     1. A struct DU cannot be recursively-defined.
    ///     2. A struct DU must have unique names for each of its cases.
    [<Struct>]
    type Shape =
        | Circle of radius: float
        | Square of side: float
        | Triangle of height: float * width: float
