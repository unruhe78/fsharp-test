namespace BankValidation

module private BankDataListReader =
    open BankValidation.Contracts

    let private filePath = "BLZ_Latest.csv"

    let private fetchFileLines = System.IO.File.ReadAllLines(filePath)
    let private splitFileLine (fileLine : string) = fileLine.Split ';'
    let private mapToBankInfo fileLineItems =
        let info = new BankValidation.Contracts.BankInfo()
        let mapper i v = 
            match i with
            | 0  -> info.BankCode <- v
            | 1  -> info.BankFlag <- BankFlag.Parse(typedefof<BankFlag>, v) :?> BankFlag
            | 2  -> info.BankName <- v
            | 3  -> info.PostalCode <- v
            | 4  -> info.City <- v
            | 5  -> info.ShortName <- v
            | 6  -> info.PAN <- v
            | 7  -> info.BIC <- v
            | 8  -> info.ValidationMethod <- v
            | 9  -> info.ItemNumber <- v
            | 10 -> info.ChangeFlag <- ChangeFlag.Parse(typeof<ChangeFlag>, v) :?> ChangeFlag
            | 11 -> info.IsDeleted <- (v = "1")
            | 12 -> info.SuccessorBankCode <- if info.ChangeFlag = ChangeFlag.D || info.IsDeleted then v else ""
            | _  -> v |> ignore
        fileLineItems |> (Array.mapi mapper >> ignore)
        info
    let private createBankInfoItems = Array.map (splitFileLine >> mapToBankInfo)
    let private filterBankInfoItems predicate array =
        match predicate with
        | Some(f) -> array |> Array.filter f
        | None    -> array

    let getBankData predicate =
        let isValidFileLine (fileLine : string) =
            fileLine.Length > 0 && System.Char.IsDigit fileLine.[0]
        let comp = 
            Array.filter isValidFileLine >>  // remove invalid file lines
            createBankInfoItems >>           // map each file line into a new instance of BankInfo
            filterBankInfoItems predicate >> // filter BankInfo instance with the specified predicate
            seq                              // convert filtered BankInfo instances into a sequence

        fetchFileLines |> comp

