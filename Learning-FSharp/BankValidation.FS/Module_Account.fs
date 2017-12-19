namespace BankValidation

module private Account =
    
    open BankValidation.Helpers.Comparison
    open BankValidation.Helpers.Comparison.List
    open BankValidation.Helpers.Conversion

    let private getAccountNumberDigits =
        let toCharArray s = 
            (string s).ToCharArray()
        int >> string >> toCharList >> List.map charToInt

    let fillAccountNumber numberOfDigits accountNumber = 
        (string accountNumber).PadLeft(numberOfDigits, '0')

    let getAccount (accountNumber : string, validationMethod) =
        let acc = accountNumber |> fillAccountNumber 10
        let length = accountNumber |> (int >> string >> String.length)
        let defaultAccount  = getAccountNumberDigits acc.[0..8], charToInt acc.[9]
        
        match (validationMethod, length) with
        | ("12", l) -> None // Not defined
        | ("13", l) -> if l = 10 then Some (getAccountNumberDigits acc.[1..6], charToInt acc.[7]) else None
        | ("14", l) -> if l = 10 then Some (getAccountNumberDigits acc.[3..8], charToInt acc.[9]) else None
        | ("15", l) -> if l = 10 then Some (getAccountNumberDigits acc.[5..8], charToInt acc.[9]) else None
        | ("16", l) -> if l = 10 then Some defaultAccount else None
        | ("17", l) -> if l = 10 then Some (getAccountNumberDigits acc.[1..6], charToInt acc.[7]) else None
        | ("23", l) -> if l = 10 then Some (getAccountNumberDigits acc.[0..5], charToInt acc.[6]) else None
        | ("24", l) -> if l = 10
                       then let a24 = match acc.[0] with
                                      | '3'
                                      | '4'
                                      | '5'
                                      | '6' -> "0" + acc.[1..8]
                                      | '9' -> "000" + acc.[3..8]
                                      | _   -> acc
                            Some (getAccountNumberDigits a24, charToInt a24.[9])
                       else None
        | ("25", l) -> if l = 10 then Some (getAccountNumberDigits acc.[1..8], charToInt acc.[6]) else Some defaultAccount
        | ("26", l) -> if l = 10
                       then let a26 = match acc.[0..1] with
                                      | "00" -> acc.[2..] + "00"
                                      | _    -> acc
                            Some (getAccountNumberDigits a26.[0..6], charToInt a26.[7])
                       else None
        | ("28", l) -> if l = 10 then Some (getAccountNumberDigits acc.[0..6], charToInt acc.[7]) else None
        | ("31", l) -> if l = 10 then Some defaultAccount else None
        | ("32", l) -> if l = 10 then Some (getAccountNumberDigits acc.[3..8], charToInt acc.[9]) else None
        | ("33", l) -> if l = 10 then Some (getAccountNumberDigits acc.[4..8], charToInt acc.[9]) else None
        | ("34", l) -> if l = 10 then Some (getAccountNumberDigits acc.[0..6], charToInt acc.[7]) else None
        | ("36", l) -> if l = 10 then Some (getAccountNumberDigits acc.[5..8], charToInt acc.[9]) else None
        | ("37", l) -> if l = 10 then Some (getAccountNumberDigits acc.[4..8], charToInt acc.[9]) else None
        | ("38", l) -> if l = 10 then Some (getAccountNumberDigits acc.[3..8], charToInt acc.[9]) else None
        | ("39", l) -> if l = 10 then Some (getAccountNumberDigits acc.[2..8], charToInt acc.[9]) else None
        | ("40", l) -> if l = 10 then Some defaultAccount else None
        | ("41", l) -> let a41 = match acc.[3] with
                                 | '9' -> acc.[3..8]
                                 | _   -> acc
                       Some (getAccountNumberDigits a41, charToInt a41.[9])
        | ("42", l) -> if l = 10 then Some (getAccountNumberDigits acc.[1..8], charToInt acc.[9]) else None
        | ("43", l) -> if l = 10 then Some defaultAccount else None
        | ("44", l) -> Some (getAccountNumberDigits acc.[4..8], charToInt acc.[9])
        | ("46", l) -> if l = 10 then Some (getAccountNumberDigits acc.[2..6], charToInt acc.[7]) else None
        | ("47", l) -> if l = 10 then Some (getAccountNumberDigits acc.[3..7], charToInt acc.[8]) else None
        | ("48", l) -> if l = 10 then Some (getAccountNumberDigits acc.[2..7], charToInt acc.[8]) else None
        | ("50", l) -> Some (getAccountNumberDigits acc.[0..5], charToInt acc.[6])
        | ("51a", l)-> Some (getAccountNumberDigits acc.[3..8], charToInt acc.[9])
        | ("51b", l)-> Some (getAccountNumberDigits acc.[4..8], charToInt acc.[9])
        | ("51s", l)-> Some (getAccountNumberDigits acc.[2..8], charToInt acc.[9])
        | ("52", l) -> if l = 10 && accountNumber.[0] = '9' then Some defaultAccount else None
        | ("53", l) -> if l = 10 && accountNumber.[0] = '9' then Some defaultAccount else None
        | ("54", l) -> if l = 10 && acc.[0..1] = "49" then Some (getAccountNumberDigits acc.[2..8], charToInt acc.[9]) else None
        | ("57", l) -> let caseA = [ 51; 55; 61; 64; 65; 66; 70; 73; 74; 75;
                                      76; 77; 78; 79; 80; 81; 82; 88; 94; 95 ]
                       let caseB = [ 32; 33; 34; 35; 36; 37; 38; 39; 41; 42; 43; 
                                      44; 45; 46; 47; 48; 49; 52; 53; 54; 56; 57;
                                      58; 59; 60; 62; 63; 67; 68; 69; 71; 72; 83;
                                      84; 85; 86; 87; 89; 90; 92; 93; 96; 97; 98 ]
                       let firstTwoDigits = int acc.[0..1]
                       if   caseA |> contains firstTwoDigits then Some defaultAccount 
                       elif caseB |> contains firstTwoDigits then Some (getAccountNumberDigits (acc.[0..1] + acc.[3..]), charToInt acc.[2])
                       else None
        | ("58", l) -> if l >= 6 then Some (getAccountNumberDigits acc.[4..8], charToInt acc.[9]) else None
        | ("59", l) -> Some (getAccountNumberDigits acc.[2..8], charToInt acc.[9])
        | ("60", l) -> if l = 10 then Some defaultAccount else None
        | ("61", l) -> if l = 10 
                       then match accountNumber.[8] with
                            | '8' -> Some (getAccountNumberDigits acc.[0..6], charToInt acc.[7])
                            | _   -> Some (getAccountNumberDigits (acc.[0..6] + acc.[8..9]), charToInt acc.[7])
                       else None
        | ("62", l) -> if l = 10 then Some (getAccountNumberDigits acc.[2..6], charToInt acc.[7]) else None
        | ("63", l) -> match acc.[0] with
                       | '0' -> match acc.[1..2] with
                                | "00" -> Some (getAccountNumberDigits acc.[3..8], charToInt acc.[9])
                                | _    -> Some (getAccountNumberDigits acc.[1..6], charToInt acc.[7])
                       | _   -> None
        | ("64", l) -> if l = 10 then Some (getAccountNumberDigits acc.[0..5], charToInt acc.[6]) else None
        | ("65", l) -> if l = 10 
                       then match accountNumber.[8] with
                            | '9' -> Some (getAccountNumberDigits acc.[0..6], charToInt acc.[7])
                            | _   -> Some (getAccountNumberDigits (acc.[0..6] + acc.[8..9]), charToInt acc.[7])
                       else None
        | ("66", l) -> if l =  9 then Some (getAccountNumberDigits acc.[1..8], charToInt acc.[9]) else None
        | ("67", l) -> if l = 10 then Some (getAccountNumberDigits acc.[0..6], charToInt acc.[7]) else None
        | ("68a", l)-> if   l = 10 && accountNumber.[3] = '9' then Some (getAccountNumberDigits acc.[3..8], charToInt acc.[9]) 
                       elif l >= 6 then Some defaultAccount
                       else None
        | ("68b", l)-> if   l = 10 then None
                       elif l >= 6 then Some (getAccountNumberDigits ((string accountNumber.[0]) + accountNumber.[3..]), charToInt acc.[9])
                       else None
        | ("70", l) -> if   l = 10 
                       then if accountNumber.[3] = '5' || accountNumber.[3..4] = "69" 
                            then Some (getAccountNumberDigits acc.[3..8], charToInt acc.[9])
                            else Some defaultAccount
                       else None
        | ("71", l) -> if l = 10 then Some (getAccountNumberDigits acc.[1..6], charToInt acc.[9]) else None
        | ("72", l) -> if l = 10 then Some (getAccountNumberDigits acc.[3..8], charToInt acc.[9]) else None
        | ("73a", l)-> Some (getAccountNumberDigits acc.[3..8], charToInt acc.[9])
        | ("73b", l)-> Some (getAccountNumberDigits acc.[4..8], charToInt acc.[9])
        | ("73s", l)-> Some (getAccountNumberDigits acc.[2..8], charToInt acc.[9])
        | ("75", l) -> match l with
                       | 6
                       | 7 -> Some (getAccountNumberDigits acc.[4..8], charToInt acc.[9])
                       | 9 -> match acc.[1] with
                              | '9' -> Some (getAccountNumberDigits acc.[2..6], charToInt acc.[7]) 
                              | _   -> Some (getAccountNumberDigits acc.[1..5], charToInt acc.[6])
                       | _ -> None
        | ("76a", l)-> match (acc.[0]) with
                       | '0'
                       | '4'
                       | '6'
                       | '7'
                       | '8'
                       | '9' -> match int acc.[1..2] with
                                | 0             -> Some (getAccountNumberDigits acc.[3..6], charToInt acc.[7])
                                | x when x < 10 -> Some (getAccountNumberDigits acc.[2..6], charToInt acc.[7])
                                | _             -> Some (getAccountNumberDigits acc.[1..6], charToInt acc.[7])
                       | _   -> None
        | ("76b", l)-> let a = acc.[2..] + "00" // this is the nasty shit case
                       match (a.[0]) with
                       | '0'
                       | '4'
                       | '6'
                       | '7'
                       | '8'
                       | '9' -> match int a.[1..2] with
                                | 0             -> Some (getAccountNumberDigits a.[3..6], charToInt a.[7])
                                | x when x < 10 -> Some (getAccountNumberDigits a.[2..6], charToInt a.[7])
                                | _             -> Some (getAccountNumberDigits a.[1..6], charToInt a.[7])
                       | _   -> None
        | ("77", l) -> Some (getAccountNumberDigits acc.[5..9], 0)
        | ("79", l) -> if acc.[0] = '0' then None
                       elif acc.[0] |> isIn [ '1'; '2'; '9' ] then Some (getAccountNumberDigits acc.[5..9], 0)
                       else Some defaultAccount
        | ("80a", l)-> Some (getAccountNumberDigits acc.[4..8], charToInt acc.[9])
        | ("80s", l)-> Some (getAccountNumberDigits acc.[2..8], charToInt acc.[9])
        | ("81a", l)-> Some (getAccountNumberDigits acc.[3..8], charToInt acc.[9])
        | ("81s", l)-> Some (getAccountNumberDigits acc.[2..8], charToInt acc.[9])
        | ("83a", l)-> Some (getAccountNumberDigits acc.[3..8], charToInt acc.[9])
        | ("83b", l)-> Some (getAccountNumberDigits acc.[4..8], charToInt acc.[9])
        | ("83s", l)-> Some (getAccountNumberDigits acc.[2..8], charToInt acc.[9])
        | ("84a", l)-> Some (getAccountNumberDigits acc.[4..8], charToInt acc.[9])
        | ("84s", l)-> Some (getAccountNumberDigits acc.[2..8], charToInt acc.[9])
        | ("85a", l)-> Some (getAccountNumberDigits acc.[3..8], charToInt acc.[9])
        | ("85b", l)-> Some (getAccountNumberDigits acc.[4..8], charToInt acc.[9])
        | ("85s", l)-> Some (getAccountNumberDigits acc.[2..8], charToInt acc.[9])
        | ("86a", l)-> Some (getAccountNumberDigits acc.[3..8], charToInt acc.[9])
        | ("86s", l)-> Some (getAccountNumberDigits acc.[2..8], charToInt acc.[9])
        | ("87a", l)-> Some (getAccountNumberDigits acc.[3..8], charToInt acc.[9])
        | ("87b", l)-> Some (getAccountNumberDigits acc.[4..8], charToInt acc.[9])
        | ("87s", l)-> Some (getAccountNumberDigits acc.[2..8], charToInt acc.[9])
        | ("88a", l)-> Some (getAccountNumberDigits acc.[3..8], charToInt acc.[9])
        | ("88s", l)-> Some (getAccountNumberDigits acc.[2..8], charToInt acc.[9])
        | ("89", l) -> if l = 7 then Some (getAccountNumberDigits acc.[3..8], charToInt acc.[9]) else Some defaultAccount
        | ("90a", l)-> Some (getAccountNumberDigits acc.[3..8], charToInt acc.[9])
        | ("90b", l)-> Some (getAccountNumberDigits acc.[4..8], charToInt acc.[9])
        | ("90s", l)-> Some (getAccountNumberDigits acc.[2..8], charToInt acc.[9])
        | ("91a", l)-> if l = 10 then Some (getAccountNumberDigits acc.[0..5], charToInt acc.[6]) else None
        | ("91b", l)-> if l = 10 then Some (getAccountNumberDigits acc       , charToInt acc.[6]) else None
        | ("92", l) -> if l = 10 then Some (getAccountNumberDigits acc.[3..8], charToInt acc.[9]) else None
        | ("93", l) -> match  l with
                       | 6  -> Some (getAccountNumberDigits acc.[0..4], charToInt acc.[5])
                       | 10 -> Some (getAccountNumberDigits acc.[4..9], charToInt acc.[9])
                       | _  -> None
        | ("98", l) -> if l = 10 then Some (getAccountNumberDigits acc.[2..8], charToInt acc.[9]) else None
        | ("A1", l) -> match l with
                       | 8  -> Some defaultAccount
                       | 10 -> Some (getAccountNumberDigits acc.[2..8], charToInt acc.[9])
                       | _  -> None
        | ("A4a", l)-> Some (getAccountNumberDigits acc.[3..8], charToInt acc.[9])
        | ("A4b", l)-> Some (getAccountNumberDigits acc.[4..8], charToInt acc.[9])
        | ("A4s", l)->  match  l with
                        | 6  -> Some (getAccountNumberDigits acc.[0..4], charToInt acc.[5])
                        | 10 -> Some (getAccountNumberDigits acc.[4..9], charToInt acc.[9])
                        | _  -> None
        | ("A8a", l)-> Some (getAccountNumberDigits acc.[3..8], charToInt acc.[9])
        | ("A8s", l)-> Some (getAccountNumberDigits acc.[2..8], charToInt acc.[9])
        | ("B0", l) -> if l = 10 && acc.[0] |> isNot '8' then Some defaultAccount else None
        | _         -> Some defaultAccount

    let getAccountAsNumber accountNumber =
        let acc = accountNumber |> fillAccountNumber 10
        (int acc.[0..8], charToInt acc.[9])

    let getEserAccount ((accountNumber : string), (bankCode : string)) =
        let trimAccountNumber = int >> string
        let eser = match bankCode.[3] with
                   | '5' -> match  accountNumber.Length with
                            | 8 -> bankCode.[4..] + (accountNumber |> trimAccountNumber)
                            | 9 -> bankCode.[4..5] + string accountNumber.[1] + string bankCode.[7] + string accountNumber.[0] + string accountNumber.[2] + (accountNumber.[3..] |> trimAccountNumber)
                            | _ -> ""
                   | _   -> ""
        if eser = "" then None else Some (getAccountNumberDigits eser, 10) // correct checksum must always be 10
        
