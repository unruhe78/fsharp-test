namespace DomainModelling

module NaiveDomainModel =

    type Contact = {
        FirstName:string
        MiddleInitial:string
        LastName:string

        Street:string
        HouseNo:string
        ZipCode:string
        City:string
        Country:string

        EmailAddress:string
        IsEmailVerified:bool
    }

(*
    Question 1: Which values are optional?
    Answer:     MiddleInitial
    General Considerations:
    - NULL is not allowed as a value is almost every functional programming language
    --> Use F# built-in type Option<'T> = Some of 'T | None
*)

module DomainModelConsideringOptionalValues =
    type Contact = {
        FirstName:string
        MiddleInitial:string option
        LastName:string

        Street:string
        HouseNo:string
        ZipCode:string
        City:string
        Country:string
    
        EmailAddress:string
        IsEmailVerified:bool
    }

(*
    Question 2: What are the constraints?
    Answer:     All strings have a max. length, the email address additionally has a specific format
    General Considerations:
    - NULL is not allowed as value
    - Throwing exceptions is like *SHAME*
    --> Use built-in type Option<'T> for error handling to return "Some VALUE" in success case and "None" in error case
    --> Define SINGLE CHOICE TYPEs to use instead of primitive types (String2, String50, EmailAddress instead of plain string)
    --> Implement FUNCTIONS for checking constraints and creating the value of the specific single choice type
    --> Make use of CURRYING to avoid duplicate code
*)

module DomainModelConsideringConstrainedValues =
    open System.Text.RegularExpressions

    type String1 = String1 of string

    type String5 = String5 of string

    type String10 = String10 of string

    type String50 = String50 of string

    type EmailAddress = EmailAddress of String50

    type Contact = {
        FirstName:String50
        MiddleInitial:String1
        LastName:String50

        Street:String50
        HouseNo:String10
        ZipCode:String5
        City:String50
        Country:String50
    
        EmailAddress:EmailAddress
        IsEmailVerified:bool
    }

    let private createMaxLengthString (ctor, maxLength) str =
        if str |> String.length <= maxLength
        then Some  (ctor str)
        else None
    
    let createString1 = createMaxLengthString (String1, 1)

    let createString5 = createMaxLengthString (String5, 5)

    let createString10 = createMaxLengthString (String10, 10)

    let createString50 = createMaxLengthString (String50, 50)

    let createEmailAddress email =
        match createString50 email with
        | Some email50 -> if Regex.IsMatch(email, @"^([\w\.\-]+)@([\w\-]+)((\.(\w){2,3})+)$")
                          then Some  (EmailAddress email50)
                          else None
        | None         -> None
        

(*
    Question 3: Which fields are linked?
    Answer:     (FirstName, MiddleInitial, LastName) + (Street, HouseNo, ZipCode, City, Country) + (EmailAddress, IsEmailVerified)
    --> Create special types to cluster linked fields
*)

module DomainModelConsideringLinkedFields =
    type PersonName = {
        FirstName:string
        MiddleInitial:string
        LastName:string
    }
    
    type PostalAddressInfo = {
        Street:string
        HouseNo:string
        ZipCode:string
        City:string
        Country:string
    }

    type EmailAddressInfo = {
        EmailAddress:string
        IsEmailVerified:bool
    }

    type Contact = {
        Name:PersonName
        PostalAddress:PostalAddressInfo
        EmailAddress:EmailAddressInfo
    }

(*
    Question 4: What is the domain logic?
    Answer:     Rule 1: If the email address changed, then the email verified flag must be reset to false
                Rule 2: The email verified flag can only be set to TRUE by a special verfication service
                Rule 3: The contact must have at least a postal address or an email address
                Rule 4: The country must be Germany or France or USA
    General Considerations:
    - "There is no problem that can't be solved by wrapping it into another type" - Scott Wlachin
    --> Create special types to cluster linked fields
*)

module DomainModelConsideringDomainLogic =

    type EmailAddress = EmailAddress of string
    type VerifiedEmailAddress = VerifiedEmailAddress of EmailAddress

    type VerificationHash = VerificationHash of string
    type VerificationService = EmailAddress * VerificationHash -> VerifiedEmailAddress option

    type Country =
    | Germany
    | France
    | USA

    type PostalAddressInfo = {
        Street:string
        HouseNo:string
        ZipCode:string
        City:string
        Country:Country
    }

    type EmailAddressInfo =
    | Unverified of EmailAddress
    | Verified of VerifiedEmailAddress

    type AddressInfo =
    | EmailAddressOnly of EmailAddressInfo
    | PostalAddressOnly of PostalAddressInfo
    | EmailAndPostalAddress of EmailAddressInfo * PostalAddressInfo

    type Contact = {

        FirstName:string
        MiddleInitial:string
        LastName:string

        Address:AddressInfo
    }

(*
    Final Questions:
    1. How do I unwrap the single choice types to get the plain value?
    2. How do I enforce using the factory functions instead of using the types directly? aka How to define private constructors of F# types?
    3. how do I get information what went wrong in case of an error?

    Answers:
    1. Every type in F# can be 
    2. Don't make the type private, but make its value private (value is everything after the "equals" character)
    3. Introduce a custom error type and use type Result
*)

module FinalDomainModel =
    open Commons.Types

    type FieldName = FieldName of string
    type MaxFieldLength = MaxFieldLength of int
    type Errors =
    | NullIsNotSupported
    | FieldCannotBeEmpty of FieldName
    | FieldExceedsMaximumLength of FieldName * MaxFieldLength
    | InvalidEmailFormat of string
        with
        member e.Message =
            match e with
            | NullIsNotSupported -> "NULL is no allowed value in F#"
            | FieldCannotBeEmpty (FieldName n) -> sprintf "Field \"%s\" cannot be empty" n
            | FieldExceedsMaximumLength (FieldName n, MaxFieldLength l) -> sprintf "Field \"%s\" cannot be longer than %i characters" n l
            | InvalidEmailFormat email -> sprintf "\"%s\" is not a valid email address" email

    let private createString (ctor, (MaxFieldLength maxLength)) fieldName str = // implement general function to validate string for its max. length
        match Option.ofObj(str) with
        | Some s -> let length = s |> String.length
                    if length =  0 then Error (Errors.FieldCannotBeEmpty fieldName)
                    elif length > maxLength then Error (Errors.FieldExceedsMaximumLength (fieldName, (MaxFieldLength maxLength)))
                    else Success (s |> ctor)
        | None   -> Error Errors.NullIsNotSupported

    type String1   = private String1 of string with
        member s.Value = match s with String1   str -> str // define additional member to get unwrapped plain value of custom type
        static member create fieldName str = str |> createString (String1, MaxFieldLength 1) (FieldName fieldName) // define factory function a static member of the type

    type String5   = private String5 of string with
        member s.Value = match s with String5   str -> str
        static member create fieldName str = str |> createString (String5, MaxFieldLength 5) (FieldName fieldName)

    type String10  = private String10 of string with
        member s.Value = match s with String10  str -> str
        static member create fieldName str = str |> createString (String10, MaxFieldLength 10) (FieldName fieldName)

    type String50  = private String50 of string with
        member s.Value = match s with String50  str -> str
        static member create fieldName str = str |> createString (String50, MaxFieldLength 50) (FieldName fieldName)

    type String100 = private String100 of string with
        member s.Value = match s with String100 str -> str
        static member create fieldName str = str |> createString (String100, MaxFieldLength 100) (FieldName fieldName)

    type StringEmail = private StringEmail of String100 with
        member s.Value = match s with StringEmail str -> str.Value
        static member internal create fieldName str =
            match str |> String100.create fieldName, System.Text.RegularExpressions.Regex.IsMatch(str, @"^([\w\.\-]+)@([\w\-]+)((\.(\w){2,3})+)$") with
            | Success s, true  -> Success (StringEmail s)
            | Success s, false -> Error (Errors.InvalidEmailFormat str)
            | Error e, _  -> Error e

    type VerifiedEmail = private VerifiedEmail of StringEmail with
        member s.Value = match s with VerifiedEmail str -> str.Value

    type Country = Germany | France | USA with
        member c.CountryCode  = match c with Germany -> "DE" | France -> "FR" | USA -> "US"
        member c.LanguageCode = match c with Germany -> "de" | France -> "fr" | USA -> "en"
    
    type VerificationHash = VerificationHash of string

    type EmailVerificationService = StringEmail * VerificationHash -> Result<VerifiedEmail, Errors>

    type PersonName = {
        FirstName:String50
        MiddleInitial:String1 option
        LastName:String50
    } with
        static member create firstName middleInitial lastName =
            let fn = firstName |> String50.create "FirstName"
            let ln = lastName |> String50.create "LastName"
            match fn, ln with
            | Success f, Success l -> match middleInitial with
                                      | Some mi -> let m = mi |> String1.create "MiddleInitial"
                                                   match m with
                                                   | Success middle -> Success { FirstName = f; MiddleInitial = Some middle; LastName = l }
                                                   | Error middle   -> Error middle
                                      | None    -> Success { FirstName = f; MiddleInitial = None; LastName = l }
            | Error f, _           -> Error f
            | _, Error l           -> Error l

    type PostalAddress = {
        Street:String50
        HouseNo:String10
        ZipCode:String5
        City:String50
        Country:Country
    } with
        static member create street houseNo zipCode city country =
            match street |> String50.create "Street", houseNo |> String10.create "HouseNo", zipCode |> String5.create "ZipCode", city |> String50.create "City" with
            | Success s, Success h, Success z, Success c -> Success {
                                                                Street = s
                                                                HouseNo = h
                                                                ZipCode = z
                                                                City = c
                                                                Country = country
                                                            }
            | Error s, _, _, _                           -> Error s
            | _, Error h, _, _                           -> Error h
            | _, _, Error z, _                           -> Error z
            | _, _, _, Error s                           -> Error s

    type EmailAddress = private Verified of VerifiedEmail | Unverified of StringEmail with
        member s.Value = 
            match s with
            | Unverified u -> u.Value
            | Verified v   -> v.Value
        static member create str =
            match str |> StringEmail.create "Unverified Email" with
            | Success s -> Success (Unverified s)
            | Error s   -> Error s
    
    type PersonContact =
    | Postal of PostalAddress
    | Email of EmailAddress with
        member c.PostalValue = 
            match c with
            | Postal p -> Some p
            | Email  e -> None
        member c.EmailValue = 
            match c with
            | Postal p -> None
            | Email  e -> Some e
        member c.StringValue = 
            match c with
            | Postal p -> sprintf "%s %s, %s-%s %s" p.Street.Value p.HouseNo.Value p.Country.CountryCode p.ZipCode.Value p.City.Value
            | Email  e -> e.Value

    type Contact = {
        Name:PersonName
        PrimaryContact:PersonContact
        AdditionalContacts:PersonContact seq
    } with
        static member create name primaryContact =
            match name, primaryContact with
            | Success n, Success c -> Success {
                                          Name = n
                                          PrimaryContact = c
                                          AdditionalContacts = Seq.empty
                                      }
            | Error n, _           -> Error n
            | _, Error c           -> Error c
        member c.addContacts contacts = {
            c with AdditionalContacts = c.AdditionalContacts |> Seq.append contacts
        }
        member c.addContact contact = c.addContacts (contact |> Seq.replicate 1)
