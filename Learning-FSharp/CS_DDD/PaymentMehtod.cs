namespace DomainModelling
{
    public interface IPaymentMethod
    {
        string Print();
    }

    public class Cash : IPaymentMethod
    {
        public string Print()
        {
            return "Paid cash";
        }
    }

    public class Check : IPaymentMethod
    {
        private readonly string checkNumber;

        public Check(string checkNumber)
        {
            this.checkNumber = checkNumber;
        }

        public string Print()
        {
            return $"Paid with check no. {this.checkNumber}";
        }
    }

    public class CreditCard : IPaymentMethod
    {
        private readonly string cardNumber;
        private readonly string cardType;

        public CreditCard(string cardType, string cardNumber)
        {
            this.cardType = cardType;
            this.cardNumber = cardNumber;
        }

        public string Print()
        {
            return $"Paid with {this.cardType} no. {this.cardNumber}";
        }
    }
}
