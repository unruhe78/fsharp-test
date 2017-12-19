namespace BankValidation

module private Validation =
    
    open BankValidation.Helpers.Calculation
    open BankValidation.Helpers.Calculation.Numbers
    open BankValidation.Helpers.Comparison
    open BankValidation.Helpers.Comparison.List
    
    let private transformationTable =
        let table = [| 
            [| 0; 1; 5; 9; 3; 7; 4; 8; 2; 6 |]; 
            [| 0; 1; 7; 6; 9; 8; 3; 2; 5; 4 |]; 
            [| 0; 1; 8; 4; 6; 2; 9; 5; 7; 3 |]; 
            [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 |]; 
        |]

        Array2D.init 4 10 (fun i j -> table.[i].[j])
    
    let private weightDigits weightings (operationAfterWeighting : (int -> int -> int) option) digits =
        let numberOfWeightings = List.length weightings
        let doWeighting idx digit =
            let weight = if idx < numberOfWeightings then weightings.[idx] else 0
            let weightedDigit = digit * weight
            match operationAfterWeighting with
            | Some(f) -> f weight weightedDigit
            | None    -> weightedDigit
        digits |> List.mapi doWeighting
    let private weightAllDigits weightings (operationAfterWeighting : (int -> int -> int) option) digits =
        let numberOfWeightings = List.length weightings
        let allWeightings = digits |> List.mapi (fun i v -> weightings.[i % numberOfWeightings])
        digits |> weightDigits allWeightings operationAfterWeighting
        
    let private transformDigits digits =  
        digits |> List.mapi (fun i v -> transformationTable.[(i % 4), v])

    let isValid accountNumber bankCode method =
        let trimmedAccount = accountNumber |> (int >> string)
        let filledAccount = accountNumber |> Account.fillAccountNumber 10
        let accountLength = trimmedAccount |> String.length

        let apply validationMethod account = 
            match account with
            | Some(a) -> (a |> snd) = (a |> (fst >> validationMethod))
            | None    -> false

        let replace valueToReplace replacement value = 
            if value = valueToReplace then replacement else value
        let replace10With = replace 10
        let replace11With = replace 11

        let modXSubFromX x value =
            value |> (modulo x >> subtractFromValue x)
        let mod7SubFrom7   = modXSubFromX 7
        let mod10SubFrom10 = modXSubFromX 10
        let mod11SubFrom11 = modXSubFromX 11

        let weightedDigits weightings            = weightDigits weightings None
        let weightedSum weightings               = weightedDigits weightings >> List.sum
        let weightedSumMod11 weightings          = weightedSum weightings >> modulo 11
        let weightedSumMod11SubFrom11 weightings = weightedSumMod11 weightings >> subtractFromValue 11
        let weightedSumOfCrossSum weightings     = weightedDigits weightings >> List.sumBy crossSum

        let rev_weightedDigits weightings            = List.rev >> weightedDigits weightings
        let rev_weightedSum weightings               = List.rev >> weightedSum weightings
        let rev_weightedSumMod11 weightings          = List.rev >> weightedSumMod11 weightings
        let rev_weightedSumMod11SubFrom11 weightings = List.rev >> weightedSumMod11SubFrom11 weightings
        let rev_weightedSumOfCrossSum weightings     = List.rev >> weightedSumOfCrossSum weightings

        let m00Mod7      weightings  = List.rev >> weightedSumOfCrossSum weightings >> mod7SubFrom7 >> replace 7 0
        let m00Mod10_fwd weightings  = weightedSumOfCrossSum weightings >> mod10SubFrom10 >> replace10With 0
        let m00Mod10     weightings  = List.rev >> m00Mod10_fwd weightings
        let m01Mod7      weightings = rev_weightedSum weightings >> mod7SubFrom7 >> replace 7 0
        let m01Mod10_fwd weightings = weightedSum weightings >> mod10SubFrom10 >> replace10With 0
        let m01Mod10     weightings = List.rev >> m01Mod10_fwd weightings
        let m02          weightings = rev_weightedSumMod11SubFrom11 weightings >> replace10With -1 >> replace11With 0
        let m06Mod11     weightings = rev_weightedSumMod11SubFrom11 weightings >> replace10With  0 >> replace11With 0
        let m10h                    = List.rev >> transformDigits >> List.sum >> mod10SubFrom10

        let m51 defaultCase (isDefaultCase : bool option) (specialCase : ((int list * int) option -> bool) option) =
            let isDefaultCase = match isDefaultCase with
                                | Some(b) -> b
                                | None    -> filledAccount.[2] <> '9'
            let specialCase   = match specialCase with
                                | Some(f) -> f
                                | None    -> (fun acc -> acc |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 8 ]) || 
                                                         acc |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 8; 9; 10 ]))
            match isDefaultCase with
            | true  -> let acc1 = Account.getAccount (trimmedAccount, method + "a")
                       let acc2 = Account.getAccount (trimmedAccount, method + "b")
                       (acc1, acc2) |> defaultCase
            | false -> let acc = Account.getAccount (trimmedAccount, method + "s")
                       acc |> specialCase
        
        match method with
        | "00" -> Account.getAccount (trimmedAccount, method) |> apply (m00Mod10 [ 2; 1 ])
        | "01" -> Account.getAccount (trimmedAccount, method) |> apply (m01Mod10 [ 3; 7; 1 ])
        | "02" -> Account.getAccount (trimmedAccount, method) |> apply (m02 [ 2; 3; 4; 5; 6; 7; 8; 9; 2 ])
        | "03" -> Account.getAccount (trimmedAccount, method) |> apply (m01Mod10 [ 2; 1 ])
        | "04" -> Account.getAccount (trimmedAccount, method) |> apply (m02 [ 2; 3; 4; 5; 6; 7; 2; 3; 4 ])
        | "05" -> Account.getAccount (trimmedAccount, method) |> apply (m01Mod10 [ 7; 3; 1 ])
        | "06" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7 ])
        | "07" -> Account.getAccount (trimmedAccount, method) |> apply (m02 [ 2; 3; 4; 5; 6; 7; 8; 9; 10 ])
        | "08" -> (int accountNumber) >= 60000 && (Account.getAccount (trimmedAccount, method) |> apply (m00Mod10 [ 2; 1 ]))
        | "09" -> true // No validation
        | "10" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 8; 9; 10 ])
        | "11" -> Account.getAccount (trimmedAccount, method) |> apply (rev_weightedSumMod11SubFrom11 [ 2; 3; 4; 5; 6; 7; 8; 9; 10 ] >> replace10With 9 >> replace11With 0)
        | "12" -> false // Not defined
        | "13" -> Account.getAccount (trimmedAccount, method) |> apply (m00Mod10 [ 2; 1 ])
        | "14" -> Account.getAccount (trimmedAccount, method) |> apply (m02 [ 2; 3; 4; 5; 6; 7 ])
        | "15" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5 ])
        | "16" -> let account = Account.getAccount (trimmedAccount, method)
                  match account with
                  | Some (acc, checkDigit) -> let calcCheckDigit = m02 [ 2; 3; 4; 5; 6; 7; 2; 3; 4 ] acc
                                              (calcCheckDigit = -1 && acc.[8] = acc.[9]) || checkDigit = calcCheckDigit
                  | None -> false
        | "17" -> let validation = weightedSumOfCrossSum [ 1; 2 ] >> subtract 1 >> modulo 11 >> subtractFromValue 10 >> replace10With 0
                  Account.getAccount (trimmedAccount, method) |> apply validation
        | "18" -> Account.getAccount (trimmedAccount, method) |> apply (m01Mod10 [ 3; 9; 7; 1 ])
        | "19" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 8; 9; 1 ])
        | "20" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 8; 9; 3 ])
        | "21" -> let validation = rev_weightedSumOfCrossSum [ 2; 1 ] >> oneDigitCrossSum >> subtractFromValue 10
                  Account.getAccount (trimmedAccount, method) |> apply validation
        | "22" -> let validation = rev_weightedDigits [ 3; 1 ] >> List.sumBy onesPlace >> subtractFromFunc (currentDecade >> add 10)
                  Account.getAccount (trimmedAccount, method) |> apply validation
        | "23" -> let account = Account.getAccount (trimmedAccount, method)
                  match account with
                  | Some (acc, checkDigit) -> let calcCheckDigit = m02 [ 2; 3; 4; 5; 6; 7 ] acc
                                              (calcCheckDigit = -1 && acc.[5] = acc.[6]) || checkDigit = calcCheckDigit
                  | None -> false
        | "24" -> let validation = weightDigits [ 1; 2; 3 ] (Some (fun w p -> (w + p) |> modulo 11)) >> List.sum >> replace10With 0
                  Account.getAccount (trimmedAccount, method) |> apply validation
        | "25" -> let account = Account.getAccount (trimmedAccount, method)
                  match account with
                  | Some (acc, checkDigit) -> let calcCheckDigit = acc|> m02 [ 2; 3; 4; 5; 6; 7 ]
                                              (calcCheckDigit = 1 && ([ 8; 9 ] |> List.exists (fun x -> x = acc.[0]))) || checkDigit = calcCheckDigit
                  | None -> false
        | "26" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 2 ])
        | "27" -> match trimmedAccount.Length with
                  | 10 -> match trimmedAccount.[0] with
                          | '9' -> Account.getAccount (trimmedAccount, method) |> apply m10h
                          | _   -> false
                  | _  -> Account.getAccount (trimmedAccount, method) |> apply (m00Mod10 [ 2; 1 ])
        | "28" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 8 ])
        | "29" -> Account.getAccount (trimmedAccount, method) |> apply  m10h
        | "30" -> Account.getAccount (trimmedAccount, method) |> apply (m01Mod10_fwd [ 2; 0; 0; 0; 0; 1; 2; 1; 2 ])
        | "31" -> let account = Account.getAccount (trimmedAccount, method)
                  match account with
                  | Some (acc, checkDigit) -> let calcCheckDigit = acc|> rev_weightedSumMod11 [ 9; 8; 7; 6; 5; 4; 3; 2; 1 ]
                                              calcCheckDigit <> 10 && checkDigit = calcCheckDigit
                  | None -> false
        | "32" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7 ])
        | "33" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6 ])
        | "34" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 4; 8; 5; 10; 9; 7 ])
        | "35" -> let account = Account.getAccount (trimmedAccount, method)
                  match account with
                  | Some (acc, checkDigit) -> let calcCheckDigit = acc |> rev_weightedSumMod11 [ 9; 8; 7; 6; 5; 4; 3; 2; 1 ]
                                              (calcCheckDigit = 10 && acc.[8] = acc.[9]) || checkDigit = calcCheckDigit
                  | None -> false
        | "36" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 4; 8; 5 ])
        | "37" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 4; 8; 5; 10 ])
        | "38" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 4; 8; 5; 10; 9 ])
        | "39" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 4; 8; 5; 10; 9; 7 ])
        | "40" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 4; 8; 5; 10; 9; 7; 6 ])
        | "41" -> Account.getAccount (trimmedAccount, method) |> apply (m00Mod10 [ 2; 1 ])
        | "42" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 8; 9 ])
        | "43" -> let validation = rev_weightedSum [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] >> mod10SubFrom10 >> replace10With 0
                  Account.getAccount (trimmedAccount, method) |> apply validation
        | "44" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 4; 8; 5; 10 ])
        | "45" -> filledAccount.[0] = '0' || filledAccount.[4] = '1' || Account.getAccount (trimmedAccount, method) |> apply  (m00Mod10 [ 2; 1 ])
        | "46" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6 ])
        | "47" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6 ])
        | "48" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7 ])
        | "49" -> (Account.getAccount (trimmedAccount, method) |> apply (m00Mod10 [ 2; 1 ])) ||
                  (Account.getAccount (trimmedAccount, method) |> apply (m01Mod10 [ 3; 7; 1 ]))
        | "50" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7 ])
        | "51" -> let defaultCase (acc1, acc2) =
                      acc1 |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7 ]) || 
                      acc2 |> apply (m06Mod11 [ 2; 3; 4; 5; 6 ]) || 
                      acc1 |> apply (m00Mod10 [ 2; 1 ]) || 
                      (acc2.Value |> (snd >> isNotIn [ 7; 8; 9 ]) &&
                       acc2 |> apply (m01Mod7 [ 2; 3; 4; 5; 6; 7 ]))
                  m51 defaultCase None None
        | "52"
        | "53" -> match trimmedAccount.Length with
                  | 10 -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 8; 9; 3 ])
                  | _  -> Account.getEserAccount (trimmedAccount, bankCode) |> apply (rev_weightedSumMod11 [ 2; 3; 4; 5; 6; 7; 8; 9; 3 ])
        | "54" -> Account.getAccount (trimmedAccount, method) |> apply (rev_weightedSumMod11SubFrom11 [ 2; 3; 4; 5; 6; 7; 2 ] >> replace10With -1 >> replace11With -1)
        | "55" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 8; 7; 8 ])
        | "56" -> let account = Account.getAccount (trimmedAccount, method)
                  match account with
                  | Some(acc, checkDigit) -> let replacement = if accountLength = 9 && acc.[0] = 9 then 7 else -1
                                             account |> apply (rev_weightedSumMod11SubFrom11 [ 2; 3; 4; 5; 6; 7; 2; 3; 4 ] >> replace10With replacement >> replace11With replacement)
                  | None -> false
        | "57" -> let case1Special = [ "777777"; "888888" ]
                  let case1OrCase2 = [         32; 33; 34; 35; 36; 37; 38; 39;
                                            41; 42; 43; 44; 45; 46; 47; 48; 49;
                                            51; 52; 53; 54; 55; 56; 57; 58; 59;
                                        60; 61; 62; 63; 64; 65; 66; 67; 68; 69;
                                        70; 71; 72; 73; 74; 75; 76; 77; 78; 79;
                                        80; 81; 82; 83; 84; 85; 86; 87; 88; 89;
                                        90;     92; 93; 94; 95; 96; 97; 98 ]
                  let case3 = [ 40; 50; 91; 99 ]
                  let case4 = [ 1..31 ]
                  let case1OrCase2Validation = m00Mod10_fwd [ 1; 2 ]
                  let case4Validation account = 
                      match account with
                      | "0185125434" -> true
                      | _            -> (int account.[2..3]) |> isIn [ 1..12 ] && (int account.[6..8]) < 500

                  let firstTwoDigits = int filledAccount.[0..1]
                  let firstSixDigits = filledAccount.[0..5]

                  (firstTwoDigits <> 0) || // Account number must be at least 9 digits long
                  (case1Special |> contains firstSixDigits) ||
                  (case1OrCase2 |> contains firstTwoDigits && Account.getAccount (trimmedAccount, method) |> apply case1OrCase2Validation) ||
                  (case3        |> contains firstTwoDigits) || // case 3 -> no validation
                  (case4        |> contains firstTwoDigits && filledAccount |> case4Validation)
        | "58" -> Account.getAccount (trimmedAccount, method) |> apply (m02 [ 2; 3; 4; 5; 6 ])
        | "59" -> match trimmedAccount.Length with
                  | 10 -> Account.getAccount (trimmedAccount, method) |> apply (m00Mod10 [ 2; 1 ])
                  | _  -> true // No validation for account numbers shorter than 9 characters
        | "60" -> Account.getAccount (trimmedAccount, method) |> apply (m00Mod10 [ 2; 1 ])
        | "61" -> Account.getAccount (trimmedAccount, method) |> apply (m00Mod10 [ 2; 1 ])
        | "62" -> let validation = rev_weightedSumOfCrossSum [ 2; 1 ] >> subtractFromFunc  (currentDecade >> add 10) >> replace10With 0
                  Account.getAccount (trimmedAccount, method) |> apply validation
        | "63" -> Account.getAccount (trimmedAccount, method) |> apply (m00Mod10 [ 2; 1 ])
        | "64" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 9; 10; 5; 8; 4; 2 ])
        | "65" -> Account.getAccount (trimmedAccount, method) |> apply (m00Mod10 [ 2; 1 ])
        | "66" -> trimmedAccount.[0] = '9' || 
                  Account.getAccount (trimmedAccount, method) |> apply (weightedSumMod11SubFrom11 [ 9; 10; 5; 8; 4; 2 ] >> replace10With 0)
        | "67" -> Account.getAccount (trimmedAccount, method) |> apply (m00Mod10 [ 2; 1 ])
        | "68" -> (accountLength = 9 && trimmedAccount.[0] = '4') ||
                  Account.getAccount (trimmedAccount, "68a") |> apply (m00Mod10 [ 2; 1 ]) ||
                  (accountLength < 10 && Account.getAccount (trimmedAccount, "68b") |> apply (m00Mod10 [ 2; 1 ]))
        | "69" -> match filledAccount.[0..1] with
                  | "93" -> true
                  | "97" -> Account.getAccount (trimmedAccount, method) |> apply  m10h
                  | _    -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 8 ]) ||
                            Account.getAccount (trimmedAccount, method) |> apply  m10h
        | "70" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7 ])
        | "71" -> Account.getAccount (trimmedAccount, method) |> apply (weightedSumMod11SubFrom11 [ 9; 10; 5; 8; 4; 2 ] >> replace10With 1 >> replace11With 0)
        | "72" -> Account.getAccount (trimmedAccount, method) |> apply (m00Mod10 [ 2; 1 ])
        | "73" -> let defaultCase (acc1, acc2) =
                      acc1 |> apply (m00Mod10 [ 2; 1 ]) || 
                      acc2 |> apply (m00Mod10 [ 2; 1 ]) || 
                      acc2 |> apply (m00Mod7  [ 2; 1 ])
                  m51 defaultCase None None
        | "74" -> let acc = Account.getAccount (trimmedAccount, method)
                  acc |> apply (m00Mod10 [ 2; 1 ]) ||
                  (accountLength = 6 && acc |> apply (rev_weightedSumOfCrossSum [ 2; 1 ] >> subtractFromFunc nextHalfDecade)) ||
                  acc |> apply (m02 [ 2; 3; 4; 5; 6; 7; 2; 3; 4 ])
        | "75" -> Account.getAccount (trimmedAccount, method) |> apply (m00Mod10_fwd [ 2; 1 ])
        | "76" -> Account.getAccount (filledAccount, method + "a") |> apply (rev_weightedSumMod11 [ 2; 3; 4; 5; 6; 7; 8 ] >> replace10With -1) ||
                  Account.getAccount (filledAccount, method + "b") |> apply (rev_weightedSumMod11 [ 2; 3; 4; 5; 6; 7; 8 ] >> replace10With -1)
        | "77" -> let acc = Account.getAccount (trimmedAccount, method)
                  acc |> apply (rev_weightedSumMod11 [ 1; 2; 3; 4; 5 ]) ||
                  acc |> apply (rev_weightedSumMod11 [ 5; 4; 3; 4; 5 ])
        | "78" -> match accountLength with
                  | 8 -> true // No validation if account has 8 digits
                  | _ -> Account.getAccount (trimmedAccount, method) |> apply (m00Mod10 [ 2; 1 ])
        | "79" -> Account.getAccount (trimmedAccount, method) |> apply (m00Mod10 [ 2; 1 ])
        | "80" -> let defaultCase (acc1, acc2) =
                      acc1 |> apply (m00Mod10 [ 2; 1 ]) ||
                      acc1 |> apply (m00Mod7  [ 2; 1 ])
                  m51 defaultCase None None
        | "81" -> let defaultCase (acc1, acc2) =
                      acc1 |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7 ])
                  m51 defaultCase None None
        | "82" -> match filledAccount.[2..3] with
                  | "99" -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 8; 9; 10 ])
                  | _    -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6 ])
        | "83" -> let defaultCase (acc1, acc2) =
                      acc1 |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7 ]) ||
                      acc2 |> apply (m06Mod11 [ 2; 3; 4; 5; 6 ]) ||
                      (acc2.Value |> (snd >> isNotIn [ 7; 8; 9 ]) &&
                       acc2 |> apply (m01Mod7 [ 2; 3; 4; 5; 6 ]))
                  m51 defaultCase (Some (filledAccount.[2..3] <> "99")) (Some (fun acc -> acc |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 8 ])))
        | "84" -> let defaultCase (acc1, acc2) =
                      acc1 |> apply (m06Mod11 [ 2; 3; 4; 5; 6 ]) ||
                      acc1 |> apply (m01Mod7 [ 2; 3; 4; 5; 6 ]) ||
                      acc1 |> apply (m01Mod10 [ 2; 1 ])
                  m51 defaultCase None None
        | "85" -> let defaultCase (acc1, acc2) =
                      acc1 |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7 ]) ||
                      acc2 |> apply (m06Mod11 [ 2; 3; 4; 5; 6 ]) ||
                      (acc2.Value |> (snd >> isNotIn [ 7; 8; 9 ]) &&
                       acc2 |> apply (m01Mod7 [ 2; 3; 4; 5; 6 ]))
                  m51 defaultCase (Some (filledAccount.[2..3] <> "99")) (Some (fun acc -> acc |> apply (m02 [ 2; 3; 4; 5; 6; 7; 8 ])))
        | "86" -> let defaultCase (acc1, acc2) =
                      acc1 |> apply (m00Mod10 [ 2; 1 ]) ||
                      acc1 |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7 ])
                  m51 defaultCase None None
        | "87" -> let validationA (digits, checkDigit) =
                      let mapDigits digit = 
                          match digit with
                          | 0 -> 5
                          | 1 -> 6
                          | 5 -> 10
                          | 6 -> 1
                          | _ -> digit
                      let calcA5 (c2, d2, a5) mappedDigit =
                          match c2 = d2 with
                          | true  -> match mappedDigit > 5 with
                                     | true  -> match c2 = 0 && d2 = 0 with
                                                | true  -> (1,  1, a5 + 6 - (mappedDigit - 6))
                                                | false -> (0,  0, a5 + mappedDigit)
                                     | false -> match c2 = 0 && d2 = 0 with
                                                | true  -> (1, d2, a5 + mappedDigit)
                                                | false -> (0, d2, a5 + mappedDigit)
                          | false -> match mappedDigit > 5 with
                                     | true  -> match c2 = 0 with
                                                | true  -> (1,  0, a5 - 6 + (mappedDigit - 6))
                                                | false -> (0,  1, a5 - mappedDigit)
                                     | false -> match c2 = 0 with
                                                | true  -> (1, d2, a5 - mappedDigit)
                                                | false -> (0, d2, a5 - mappedDigit)
                      let rec correctA5 (d2, a5) =
                          if   a5 < 0 then correctA5 (d2, a5 + 5)
                          elif a5 > 4 then correctA5 (d2, a5 - 5)
                          else (d2, a5)
                      let validateCheckDigit (d2, a5) =
                          let table = if d2 = 0 then [ 0; 4; 3; 2; 6 ] else [ 7; 1; 5; 9; 8 ]
                          let p = table.[a5]
                          match p = checkDigit with
                          | true  -> true
                          | false -> checkDigit = (if p > 4 then p - 5 else p + 5)

                      let length = digits |> List.length
                      let currentPosition = (10 - length + 1) // first non-zero-position of the full account number

                      digits |> (List.map mapDigits >> List.fold calcA5 (currentPosition % 2, 0, -1) >> (fun (c2, d2, a5) -> (d2, a5)) >> correctA5 >> validateCheckDigit)

                  let defaultCase (acc1, acc2) =
                      (match acc1 with
                      | Some(a, c) -> (a, c) |> validationA
                      | None       -> false) ||
                      acc2 |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7 ]) ||
                      acc2 |> apply (m01Mod7 [ 2; 3; 4; 5; 6 ]) ||
                      acc1 |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7 ])
                  m51 defaultCase None None
        | "88" -> let defaultCase (acc1, acc2) = acc1 |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7 ])
                  let specialCase acc = acc |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 8 ])
                  m51 defaultCase None (Some specialCase)
        | "89" -> let validation = match accountLength with
                                   | 7 -> Some (rev_weightedSumOfCrossSum [ 2; 3; 4; 5; 6; 7 ] >> mod11SubFrom11 >> replace10With  0 >> replace11With 0)
                                   | 8
                                   | 9 -> Some (m06Mod11 [ 2; 3; 4; 5; 6; 7; 8; 9; 10 ])
                                   | _ -> None // no validation
                  match validation with
                  | Some (f) -> Account.getAccount (trimmedAccount, method) |> apply f
                  | None     -> true
        | "90" -> let defaultCase (acc1, acc2) =
                      acc1 |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7 ]) ||
                      acc2 |> apply (m06Mod11 [ 2; 3; 4; 5; 6 ]) ||
                      (acc2.IsSome && acc2.Value |> (snd >> isNotIn [7; 8; 9]) && acc2 |> apply (m01Mod7 [ 2; 3; 4; 5; 6 ])) ||
                      (acc2.IsSome && acc2.Value |> (snd >> isNot 9)               && acc2 |> apply (rev_weightedSum [ 2; 3; 4; 5; 6 ] >> modXSubFromX 9)) ||
                      acc2 |> apply (m01Mod10 [ 2; 1 ]) ||
                      acc1 |> apply (m01Mod7  [ 2; 1 ])
                  let specialCase acc =
                      acc |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 8 ])
                  m51 defaultCase None (Some specialCase)
        | "91" -> let acc = Account.getAccount (trimmedAccount, "91a")
                  acc                                        |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7 ]) ||
                  acc                                        |> apply (m06Mod11 [ 7; 6; 5; 4; 3; 2 ]) ||
                  Account.getAccount (trimmedAccount, "91b") |> apply (m06Mod11 [ 2; 3; 4; 0; 5; 6; 7; 8; 9; 10 ]) ||
                  acc                                        |> apply (m06Mod11 [ 2; 4; 8; 5; 10; 9 ])
        | "92" -> Account.getAccount (trimmedAccount, method) |> apply (m01Mod10 [ 3; 7; 1 ])
        | "93" -> let acc = Account.getAccount (trimmedAccount, method)
                  acc |> apply (m06Mod11 [ 2; 3; 4; 5; 6 ]) || 
                  acc |> apply (m01Mod7 [ 2; 3; 4; 5; 6 ])
        | "94" -> Account.getAccount (trimmedAccount, method) |> apply (m00Mod10 [ 1; 2 ])
        | "95" -> let acc = int trimmedAccount
                  acc |> isBetween 0000000001 0001999999 || 
                  acc |> isBetween 0009000000 0025999999 || 
                  acc |> isBetween 0396000000 0499999999 || 
                  acc |> isBetween 0700000000 0799999999 || 
                  acc |> isBetween 0910000000 0989999999 || 
                  Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 8; 9 ])
        | "96" -> let acc = Account.getAccount (trimmedAccount, method)
                  acc |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 8; 9; 1 ]) ||
                  acc |> apply (m00Mod10 [ 2; 1 ]) ||
                  (int trimmedAccount) |> isBetween 0001300000 0099399999
        | "97" -> let (account, checkDigit) = Account.getAccountAsNumber trimmedAccount
                  checkDigit = (account |> (divide 11 >> multiplyWith 11 >> subtractFromValue account >> replace10With 0))
        | "98" -> let acc = Account.getAccount (trimmedAccount, method)
                  acc |> apply (m01Mod10 [ 3; 1; 7 ]) || 
                  acc |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7 ])
        | "99" -> let acc = int trimmedAccount
                  acc |> isBetween 0396000000 0499999999 || 
                  Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 2; 3; 4 ])
        | "A0" -> accountLength = 3 || Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 4; 8; 5; 10; 0; 0; 0; 0 ])
        | "A1" -> Account.getAccount (trimmedAccount, method) |> apply (m00Mod10 [ 2; 1 ])
        | "A2" -> let acc = Account.getAccount (trimmedAccount, method)
                  acc |> apply (m00Mod10 [ 2; 1 ]) || 
                  acc |> apply (m02 [ 2; 3; 4; 5; 6; 7; 2; 3; 4 ])
        | "A3" -> let acc = Account.getAccount (trimmedAccount, method)
                  acc |> apply (m00Mod10 [ 2; 1 ]) || 
                  acc |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 8; 9; 10 ])
        | "A4" -> let case4 acc = 
                      acc |> apply (m06Mod11 [ 2; 3; 4; 5; 6 ]) || 
                      acc |> apply (m01Mod7  [ 2; 3; 4; 5; 6 ])
                  let defaultCase (acc1, acc2) = 
                      acc1 |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7 ]) ||
                      acc1 |> apply (m01Mod7  [ 2; 3; 4; 5; 6; 7 ]) ||
                      Account.getAccount (trimmedAccount, "A4s") |> case4
                  let specialCase acc =
                      acc |> apply (m06Mod11 [ 2; 3; 4; 5; 6 ]) ||
                      acc |> case4

                      
                  m51 defaultCase (Some (filledAccount.[2..3] <> "99")) (Some specialCase)
        | "A5" -> let acc = Account.getAccount (trimmedAccount, method)
                  acc |> apply (m00Mod10 [ 2; 1 ]) ||
                  (filledAccount.[0] |> isNot '9' && acc |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 8; 9; 10 ]))
        | "A6" -> match filledAccount.[1] with
                  | '8' -> Account.getAccount (trimmedAccount, method) |> apply (m00Mod10 [ 2; 1 ])
                  | _   -> Account.getAccount (trimmedAccount, method) |> apply (m01Mod10 [ 3; 7; 1 ])
        | "A7" -> let acc = Account.getAccount (trimmedAccount, method)
                  acc |> apply (m00Mod10 [ 2; 1 ]) ||
                  acc |> apply (m01Mod10 [ 2; 1 ])
        | "A8" -> let defaultCase (acc1, acc2) =
                      acc1 |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7 ]) ||
                      acc1 |> apply (m00Mod10 [ 2; 1 ])
                  m51 defaultCase None None
        | "A9" -> let acc = Account.getAccount (trimmedAccount, method)
                  acc |> apply (m01Mod10 [ 3; 7; 1 ]) || 
                  acc |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 2; 3; 4 ])
        | "B0" -> if   filledAccount.[7] |> isIn [ '1'; '2'; '3'; '6' ]
                  then true // no checksum validation
                  else Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 2; 3; 4 ])
        | "B1" -> let acc = Account.getAccount (trimmedAccount, method)
                  acc |> apply (m01Mod10 [ 7; 3; 1 ]) ||
                  acc |> apply (m01Mod10 [ 3; 7; 1 ]) ||
                  acc |> apply (m00Mod10 [ 2; 1 ])
        | "B2" -> match filledAccount.[0] with
                  | '8'
                  | '9' -> Account.getAccount (trimmedAccount, method) |> apply (m00Mod10 [ 2; 1 ])
                  | _   -> Account.getAccount (trimmedAccount, method) |> apply (m02 [ 2; 3; 4; 5; 6; 7; 8; 9; 2 ])
        | "B3" -> match filledAccount.[0] with
                  | '9' -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 2; 3; 4 ])
                  | _   -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7 ])
        | "B4" -> match filledAccount.[0] with
                  | '9' -> Account.getAccount (trimmedAccount, method) |> apply (m00Mod10 [ 2; 1 ])
                  | _   -> Account.getAccount (trimmedAccount, method) |> apply (m02 [ 2; 3; 4; 5; 6; 7; 8; 9; 10 ])
        | "B5" -> let acc = Account.getAccount (trimmedAccount, method)
                  acc |> apply (m01Mod10 [ 7; 3; 1 ]) ||
                  (filledAccount.[0] |> isNotIn [ '8'; '9' ] && acc |> apply (m00Mod10 [ 2; 1 ]))
        | "B6" -> match filledAccount.[0] with
                  | '0' -> match (int filledAccount.[1..4]) |> isBetween 2691 2699 with
                           | true  -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 8; 9; 3 ])
                           | false -> Account.getEserAccount (trimmedAccount, bankCode) |> apply (rev_weightedSumMod11 [ 2; 3; 4; 5; 6; 7; 8; 9; 3 ])
                  | _   -> Account.getAccount (trimmedAccount, method) |> apply (m06Mod11 [ 2; 3; 4; 5; 6; 7; 8; 9; 3 ])
        | "B7" -> let acc = int trimmedAccount
                  if   (acc |> isBetween 0001000000 0005999999) || 
                       (acc |> isBetween 0700000000 0899999999)
                  then Account.getAccount (trimmedAccount, method) |> apply (m01Mod10 [ 3; 7; 1 ])
                  else true
        | "B8" -> true
        | "B9" -> true
        | "C0" -> true
        | "C1" -> true
        | "C2" -> true
        | "C3" -> true
        | "C4" -> true
        | "C5" -> true
        | "C6" -> true
        | "C7" -> true
        | "C8" -> true
        | "C9" -> true
        | "D0" -> true
        | "D1" -> true
        | "D2" -> true
        | "D3" -> true
        | "D4" -> true
        | "D5" -> true
        | "D6" -> true
        | "D7" -> true
        | "D8" -> true
        | "D9" -> true
        | "E0" -> true
        | "E1" -> true
        | "E2" -> true
        | "E3" -> true
        | "E4" -> true
        | _    -> false
