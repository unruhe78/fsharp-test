using System;
using fs = DomainModelling.FinalDomainModel;
using Commons.Types;

namespace InteropTestConsole
{
    internal class DomainModel_FS
    {
        internal fs.Contact Dinge()
        {
            var name = fs.PersonName.create("Matthias", null, "Unruhe");
            var postalAddress = fs.PostalAddress.create("Karolinenstr.", "28", "91522", "Ansbach", fs.Country.Germany);
            var emailAddress = fs.EmailAddress.create("matthiasunruhe@gmx.de");

            if (name.IsError)
            {
                Console.WriteLine(name.ErrorValue.Message);
                return null;
            }

            if (emailAddress.IsError)
            {
                Console.WriteLine(emailAddress.ErrorValue.Message);
                return null;
            }

            var contact = fs.Contact.create(name, OperationResult.returnSuccess<fs.PersonContact, fs.Errors>(fs.PersonContact.NewEmail(emailAddress.SuccessValue)));

            if(contact.IsError)
            {
                Console.WriteLine(emailAddress.ErrorValue.Message);
                return null;
            }

            if (postalAddress.IsError)
            {
                Console.WriteLine(postalAddress.ErrorValue.Message);

                return contact.SuccessValue;
            }
            else
            {
                return contact.SuccessValue.addContact(fs.PersonContact.NewPostal(postalAddress.SuccessValue));
            }
        }
    }
}
