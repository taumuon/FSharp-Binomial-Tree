
module AmericanPut 
// original Wikipedia code
// results in 4.486090
  let americanPut T     // expiration time, years
                  S     // stock price
                  K     // strike price
                  n     // height of the binomial tree
                  r     // risk free rate
                  sigma // volatility
                  q     // dividend yield
                  =
    let deltaT = T / float n
    let up = exp(sigma * sqrt(deltaT))
    
    let p0 = (up * exp(-r * deltaT) - exp(-q * deltaT)) * up / ((up * up) - 1.0)
    let p1 = exp(-r * deltaT) - p0
    
    // initial values at time T
    let V = Array.zeroCreate (n + 1)
    for i = 0 to n do
      V.[i] <- (K - S) * (up ** (float (2 * i - n)))
      if (V.[i] >= 0.0) then V.[i] <- 0.0
    
    for j = n - 1 downto 0 do
      for i = 0 to j do
        V.[i] <- p0 * V.[i] + p1 * V.[i + 1]             // binomial value
        let exercise = K - S * (up ** float (2 * i - j)) // exercise value
        if V.[i] < exercise then V.[i] <- exercise
    
    V.[0]
    

