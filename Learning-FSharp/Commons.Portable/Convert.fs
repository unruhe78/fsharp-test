namespace Commons

module Convert =
    let stringToCharArray (str:string) =
        str.ToCharArray()

    let stringToCharList = 
        stringToCharArray >> List.ofArray

    let stringToCharSeq =
        stringToCharArray >> Seq.ofArray

    let charSeqToString (chars:char seq) =
        chars
        |> Array.ofSeq
        |> System.String
        |> string

    let charArrayToString =
        Seq.ofArray >> charSeqToString

    let charListToString = 
        Seq.ofList >> charSeqToString

    let charToInt (c:char) =
        int c - int '0'

    let intToString (i:int) =
        string i

    let intToDigitArray =
        intToString >> stringToCharArray >> (Array.map (fun c -> c |> charToInt))

    let intToDigitList =
        intToDigitArray >> List.ofArray

    let intToDigitSeq =
        intToDigitArray >> Seq.ofArray

    let digitSeqToInt (digits:int seq) =
        digits
        |> Seq.map intToString
        |> String.concat ""
        |> int

    let digitArrayToInt =
        Array.toSeq >> digitSeqToInt

    let digitListToInt =
        List.toArray >> digitArrayToInt
