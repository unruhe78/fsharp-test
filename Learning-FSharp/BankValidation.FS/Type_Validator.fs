namespace BankValidation

open BankValidation.Contracts

type Validator(bankData) =
    // private fields ... must be defined first
    let bankData = bankData // value is taken from primary constructor

    // second, parameterless constructor
    new() = 
        let filter (info : BankInfo) = info.BankFlag = BankFlag.HasBankCode
        let bankData = BankDataListReader.getBankData (Some filter)
        Validator bankData

    // public methods to populate interface implementation
    member this.ValidateAccountNumber accountNumber bankCode = 
        (this :> IValidator).ValidateAccountNumber(accountNumber, bankCode)
    member this.ValidateIBAN iban = 
        (this :> IValidator).ValidateIBAN iban

    // interface implementation
    interface IValidator with
        member this.ValidateAccountNumber(accountNumber, bankCode) = 
            AccountValidator.validate bankData accountNumber bankCode
        member this.ValidateIBAN iban = 
            IBANValidator.validate bankData iban
