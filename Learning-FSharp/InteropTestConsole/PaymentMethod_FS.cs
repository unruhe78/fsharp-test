using DomainModelling;
using System;

namespace InteropTestConsole
{
    internal class PaymentMethod_FS
    {
        internal void Print()
        {
            PaymentMethod method = PaymentMethod.NewCheck("123");
            Console.WriteLine(PaymentMethodModule.print(method));
        }
    }
}
