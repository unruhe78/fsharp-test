namespace BankValidation

open BankValidation.Contracts

type BankInfoReader(filePath) = 
    // private fields ... must be defined first
    let filePath = filePath // value is taken from primary constructor

    // second, parameterless constructor
    new() =
        let defaultFilePath = "BLZ_Latest.csv"
        BankInfoReader defaultFilePath

    // public methods to populate interface implementation
    member this.GetBankData() = (this :> IBankInfoReader).GetBankData() // use parathesis here to separate parameterless from parameterized method
    member this.GetBankData predicate = (this :> IBankInfoReader).GetBankData predicate

    // interface implementation
    interface IBankInfoReader with
        member this.GetBankData() = BankDataListReader.getBankData None
        member this.GetBankData(predicate) = BankDataListReader.getBankData (Some(fun x -> predicate.Invoke x))
