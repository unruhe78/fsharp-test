namespace DomainModelling

type PaymentMethod =
| Cash
| Check of checkNumber:string
| CreditCard of cardType:string * cardNumber:string

module PaymentMethod =

    let print method =
        match method with
        | Cash                              -> sprintf "Paid cash"
        | Check checkNumber                 -> sprintf "Paid with check no. %s" checkNumber
        | CreditCard (cardType, cardNumber) -> sprintf "Paid with %s no. %s" cardType cardNumber
