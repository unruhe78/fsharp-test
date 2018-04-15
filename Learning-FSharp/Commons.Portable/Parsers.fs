namespace Commons

open Commons.Types

type Parser<'TContent, 'TState> = Parser of ('TState -> OperationResult<'TContent * 'TState, ParseError>)
and ParseError  =
| DiscardPreviousSuccess
| FollowUpError
| InputCannotBeEmpty
| InputMissmatch of (string * string)
    member e.Message =
        match e with
        | DiscardPreviousSuccess           -> sprintf "Parse operation failed due to a follow-up error!"
        | FollowUpError                    -> sprintf "Parse operation failed due to previous error!"
        | InputCannotBeEmpty               -> sprintf "Cannot parse input value because it's empty!"
        | InputMissmatch (expected, value) -> sprintf "Parse operation failed! Expected value '%s' but found value '%s'!" expected value


module Parser =
    let run parser input =
        let (Parser parserfunc) = parser
        input |> parserfunc

    let map mapper parser =
        let innerParser input =
            match run parser input with
            | Success (v, rest) -> Success (v |> mapper, rest)
            | Error error -> Error error
        Parser innerParser

    let private orParse parser1 parser2 =
        let innerParser input =
            match input |> run parser1 with
            | Success result -> Success result
            | Error error    -> input |> run parser2
        Parser innerParser

    let (|||) = orParse

    let private andParse parser1 parser2 =
        let innerParser input =
            match input |> run parser1 with
            | Success (v1, remaining1) -> 
                match remaining1 |> run parser2 with
                | Success (v2, remaining2) -> Success ((v1, v2), remaining2)
                | Error e2 -> Error e2
            | Error e1 -> Error e1
        Parser innerParser

    let (&&&) = andParse

    let apply func parser =
        func &&& parser
        |> map (fun (f, x) -> f x)

    let (<*>) = apply

    let bind func parser =
         let innerParser input =
            match run parser input with
            | Success (content, state) -> run (content |> func) state
            | Error error -> Error error
         Parser innerParser

    let (>>=) = bind

    let returnAsParser value =
        let innerParser input = 
            Success (value, input)
        Parser innerParser

    let map2 mapper parser1 parser2 =
        returnAsParser mapper <*> parser1 <*> parser2

    let rec processParserList parsers =
        match parsers with
        | [] -> returnAsParser []
        | parser::remainingParsers -> map2 (fun h t -> h::t) parser (processParserList remainingParsers)

    let expectChar expected =
        let innerParser (input:char list) =
            match input with
            | []         -> Error ParseError.InputCannotBeEmpty
            | head::tail -> if head = expected 
                            then Success (head, tail) 
                            else Error (ParseError.InputMissmatch (expected.ToString(), head.ToString()))
    
        Parser innerParser
    
    let expectString expected =
        expected
        |> Convert.stringToCharList
        |> List.map expectChar
        |> processParserList
        |> map Convert.charListToString