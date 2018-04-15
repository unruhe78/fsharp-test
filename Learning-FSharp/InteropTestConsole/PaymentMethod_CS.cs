using DomainModelling;
using System;

namespace InteropTestConsole
{
    internal class PaymentMethod_CS
    {
        internal void Print()
        {
            IPaymentMethod method = new Check("123");
            Console.WriteLine(method.Print());
        }
    }
}
