using System;
using System.Collections.Generic;
using System.Text;

namespace BankValidation.Contracts
{
    public enum BankFlag
    {
        /// <summary>
        /// Indicates that the bank has an own bank code
        /// </summary>
        HasBankCode = 1,
        /// <summary>
        /// Indicates that the bank doesn't have an own bank code 
        /// e. g. an affiliate of another bank
        /// </summary>
        HasNoBankCode = 2
    }

    public enum ChangeFlag
    {
        /// <summary>
        /// "Addition" - 
        /// Indicates that the bank info has been added to the list of all bank data
        /// </summary>
        A = 1,
        /// <summary>
        /// "Unchanged" - 
        /// Indicates that the bank info hasn't been changed since the last publication of the list of all bank data
        /// </summary>
        U = 2,
        /// <summary>
        /// "Modified" - 
        /// Indicates that the bank info has been changed since the last publication of the list of all bank data
        /// </summary>
        M = 3,
        /// <summary>
        /// "Deleted" - 
        /// Indicates that the bank info has been deleted from the list of all bank data
        /// </summary>
        D = 4
    }
}
