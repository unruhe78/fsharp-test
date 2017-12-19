namespace BankValidation

module private AccountValidator =
    open BankValidation.Contracts

    let private getBankData bankData bankCode =
        bankData |> Seq.find (fun (x : BankInfo) -> x.BankCode = bankCode)

    let validate bankData accountNumber bankCode =
        (getBankData bankData bankCode).ValidationMethod
        |> Validation.isValid accountNumber bankCode
        


