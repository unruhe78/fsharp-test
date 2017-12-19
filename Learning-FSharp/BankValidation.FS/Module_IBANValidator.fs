namespace BankValidation

module private IBANValidator =
    let private getNumericCountryCode (code : string) =
        let folder str c = str + string (10 + int c - int 'A')
        code.ToCharArray() |> (Array.fold folder "" >> string)

    let private isIBANCheckSumValid (iban : string) =
        match iban.[0..1] with
        | "DE" -> let checkSum = bigint.Parse(iban.[4..] + (getNumericCountryCode iban.[0..1]) + iban.[2..3])
                  checkSum % (bigint 97) = (bigint 1)
        | _    -> false

    let validate bankData iban = 
        match isIBANCheckSumValid iban with
        | true  -> let bankCode = iban.[4..11]
                   let bankAccount = iban.[12..]
                   AccountValidator.validate bankData bankAccount bankCode
        | false -> false
