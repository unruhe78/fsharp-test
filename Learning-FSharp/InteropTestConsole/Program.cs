using System;
using System.Linq;
using BankValidation;
using BankValidation.Contracts;

namespace InteropTestConsole
{
    class Program
    {
        static void Main(string[] args)
        {
            IBankInfoReader reader = new BankInfoReader();
            var bankData = reader.GetBankData();
            Console.WriteLine($"{bankData.Count()} bank info items in total");

            var relevantBankData = reader.GetBankData(info => info.BankFlag == BankFlag.HasBankCode);
            Console.WriteLine($"{relevantBankData.Count()} relevant bank info items");

            //foreach (var bank in relevantBankData)
            //{
            //    Console.WriteLine($"BLZ: {bank.BankCode}, Flag: {bank.BankFlag}, ValidationMethod: {bank.ValidationMethod}");
            //}

            string iban = "DE58472601219017425700";
            IValidator validator = new Validator();
            bool isValidIban = validator.ValidateIBAN(iban);
            Console.WriteLine($"IBAN \"{iban}\" is {(isValidIban ? string.Empty : "not ")}valid!");

            Console.ReadKey();
        }
    }
}
