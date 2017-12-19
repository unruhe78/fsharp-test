using System;
using System.Collections.Generic;
using System.Text;

namespace BankValidation.Contracts
{
    public class BankInfo
    {
        /// <summary>
        /// Gets or sets the bank code
        /// </summary>
        public string BankCode { get; set; }

        /// <summary>
        /// Gets or sets the bank flag
        /// <para>Indicates whether the bank has an own bank code</para>
        /// </summary>
        public BankFlag BankFlag { get; set; }

        /// <summary>
        /// Gets or sets the name of the bank
        /// </summary>
        public string BankName { get; set; }

        /// <summary>
        /// Gets or sets the postal code of the bank location
        /// </summary>
        public string PostalCode { get; set; }

        /// <summary>
        /// Gets or sets the postal code of the bank location
        /// </summary>
        public string City { get; set; }

        /// <summary>
        /// Gets or sets the short name of the bank
        /// </summary>
        public string ShortName { get; set; }

        /// <summary>
        /// Gets or sets the "Primary Account Number" of the bank
        /// <para>Only relevant for e-Cash systems</para>
        /// </summary>
        public string PAN { get; set; }

        /// <summary>
        /// Gets or sets the BIC of the bank
        /// </summary>
        public string BIC { get; set; }

        /// <summary>
        /// Gets or sets the ID of the method to check the validity of an account number of the bank
        /// </summary>
        public string ValidationMethod { get; set; }

        /// <summary>
        /// Gets or sets the item number of the bank in the total bank list of Deutsche Bundesbank
        /// </summary>
        public string ItemNumber { get; set; }

        /// <summary>
        /// Gets or sets the change flag
        /// <para>Indicates whether there are change to the bank data in the total list compared to the latest publication</para>
        /// </summary>
        public ChangeFlag ChangeFlag { get; set; }

        /// <summary>
        /// Gets or sets a flag indicating wheter the bank data is marked to be deleted
        /// </summary>
        public bool IsDeleted { get; set; }

        /// <summary>
        /// Gets or sets the bank code to be used instead of <see cref="BankCode"/> in case of deletion
        /// <para>This property is only set if the bank is either deleted (<c><see cref="ChangeFlag"/> == <see cref="ChangeFlag.D"/></c>) or marked to be deleted (<c><see cref="IsDeleted"/> == true</c>)</para>
        /// </summary>
        public string SuccessorBankCode { get; set; }
    }
}
