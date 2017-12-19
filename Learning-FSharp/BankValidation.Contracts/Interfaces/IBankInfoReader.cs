using System;
using System.Collections.Generic;
using System.Text;

namespace BankValidation.Contracts
{
    public interface IBankInfoReader
    {
        IEnumerable<BankInfo> GetBankData();
        IEnumerable<BankInfo> GetBankData(Func<BankInfo, bool> predicate);
    }
}
