module AmericanPut6
// Change value initialisation
// results in 4.486375

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
    let V = [0 .. n] |> Seq.map(fun i ->
      let Si = S * (up ** (float ((2 * i) - n)))
      max 0.0 (K - Si))

    let rec calculate n vi =
      match n with
      | -1 -> Seq.head vi
      | _ -> 
        let res = vi |> Seq.pairwise
                     |> Seq.mapi(fun i x ->
                                  let binomialValue = (p0 * (fst x)) + (p1 * (snd x))
                                  let exerciseValue = K - S * (up ** float ((2 * i) - n))
                                  max binomialValue exerciseValue)
        calculate (n - 1) res

    calculate (n - 1) V




