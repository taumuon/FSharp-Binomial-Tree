
module Program

open AmericanPut

let T = 1.0          // expiration time, years
let S = 36.0         // stock price
let K = 40.0         // strike price
let n = 500          // height of the binomial tree
let r = 0.06         // risk free rate
let sigma = 0.2      // volatility
let q = 0.0          // dividend yield
  
[<EntryPoint>]
let main argv = 
    let optionValue = americanPut T S K n r sigma q
    printfn "option value: %f" optionValue
    0   

