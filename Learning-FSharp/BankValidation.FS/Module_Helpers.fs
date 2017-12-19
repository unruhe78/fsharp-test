namespace BankValidation

module Helpers =

    module Conversion =
        let charToInt (c : char) = 
            int c - int '0'
        let toCharArray (s : string) = 
            s.ToCharArray()
        let toCharList (s : string) = 
            s |> (toCharArray >> Array.toList)
        let toCharSeq (s : string) = 
            s |> (toCharArray >> Array.toSeq)
    
    module Comparison =
        let is x value =
            value = x
        let isNot x value =
            value <> x
        let isBetween x y value =
            value >= x && value <= y

        module Array =
            let contains value array =
                array |> Array.exists (fun x -> x = value)
            let isIn array value =
                array |> contains value
            let isNotIn array value =
                array |> (not << contains value)

        module List =
            let contains value list =
                list |> List.exists (fun x -> x = value)
            let isIn list value =
                list |> contains value
            let isNotIn list value =
                list |> (not << contains value)

        module Seq =
            let contains value seq =
                seq |> Seq.exists (fun x -> x = value)
            let isIn seq value =
                seq |> contains value
            let isNotIn seq value =
                seq |> (not << contains value)
        
        
    module Calculation = 
        let add summand value =
            value + summand
    
        let subtract subtrahend number =  
            number - subtrahend
        let subtractFromValue minuend number =  
            minuend - number
        let subtractFromFunc minuend number =  
            (number |> minuend) - number
    
        let multiplyWith factor number =  
            number * factor
    
        let divide divisor number =  
            number / divisor
        let divideByValue dividend number =  
            dividend / number
        let divideByFunc dividend number =  
            (number |> dividend) / number
    
        let modulo divisor number =  
            number % divisor
        let moduloByValue dividend number =  
            dividend % number
        let moduloByFunc dividend number =  
            (number |> dividend) % number
    
        let crossSum number =  
            (number / 10) + (number % 10)
        let oneDigitCrossSum number =
            let mutable result = crossSum number
            while result >= 10 do
                result <- crossSum result
            result
    
        module Numbers =
            let private getPlace place number =
                let x = abs number
                if   x < place
                then 0
                else x / place % 10
            let onesPlace = getPlace 1
            let tensPlace = getPlace 10
            let hundredsPlace = getPlace 100
            let thausendsPlace = getPlace 1000
            let currentDecade number = 
                number |> (tensPlace >> multiplyWith 10)
            let currentCentury number = 
                number |> (hundredsPlace >> multiplyWith 100)
            let currentMillenium number = 
                number |> (thausendsPlace >> multiplyWith 1000)
            let nextHalfDecade number =  
                let halfDecade = number |> (currentDecade >> add 5)
                if   number < halfDecade
                then halfDecade
                else halfDecade + 10
