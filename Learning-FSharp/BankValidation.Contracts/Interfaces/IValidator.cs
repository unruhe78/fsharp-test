namespace BankValidation.Contracts
{
    using System.Collections.Generic;

    public interface IValidator
    {
        bool ValidateAccountNumber(string accountNumber, string bankCode);

        bool ValidateIBAN(string iban);
    }
}
