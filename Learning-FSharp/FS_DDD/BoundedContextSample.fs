namespace BoundedContextSample

module CardGame =

    type Suit = Kreuz | Pik | Herz | Karo
    
    type Rank = Zwei | Drei | Vier | Fünf | Sechs | Sieben | Acht | Neun | Zehn | Bube | Dame | König | Ass

    type Card = Suit * Rank

    type Hand = Card list

    type Deck = Card list

    type Player = { Name:string; Hand:Hand }

    type Game = { Deck:Deck; Players:Player list }

    type Deal = Deck -> Deck * Card

    type PickupCard = Hand * Card -> Hand

(*
   BOUNDED CONTEXT              --> The MODULE represents the BOUNDED CONTEXT
   UBIQUITOUS LANGUAGE          --> The TYPEs represent the DOMAIN MODELs and the DOMAIN LOGIC
   PERSISTENCE IGNORANCE        --> We only focus on the domain modelling process!
                                    Nothing else matters!
   TESTABILITY                  --> The model builds a static type system. The compiler does the type checking for you.
                                --> You almost get compile-time - mostly even design-time - unit tests when you use the model.
   MAKE THE CODE BE THE DESIGN  --> The design is the code and the code is the design!
                                    The model is executable code!

   x | y  = CHOICE   -> pick one from list   (aka Enum)
   x * y  = PAIR     -> set one of each type (aka Tuple)
   x -> y = FUNCTION -> pass X as input into the function and Y will be returned as output

   Question: Do you thinnk a non-programmer could understand this?
*)
