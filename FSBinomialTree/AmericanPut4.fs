module AmericanPut4
  // calculate no longer sets values in the initial array, instead inner loop creates new array
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
    let V = [|0 .. n|] |> Array.map(fun i ->
      let Si = (K - S) * (up ** (float (2 * i - n)))
      min Si 0.0)

    let rec calculate n (vi:array<float>) =
      match n with
      | -1 -> vi.[0]
      | _ -> 
        let v2 = Array.zeroCreate (n + 1)
        for i = 0 to n do
          let binomialValue = (p0 * vi.[i]) + (p1 * vi.[i + 1])
          let exerciseValue = K - S * (up ** float ((2 * i) - n))
          v2.[i] <- max binomialValue exerciseValue // only for american
        calculate (n - 1) v2

    calculate (n - 1) V


