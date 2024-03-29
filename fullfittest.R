
K <- c(83,91,122,107,74,123)
P <- 1/6

RollDice<- function(N,K,P) {

    sum <- 0

    for (i in 1:length(K))
    {
        sum = sum + ((K[i]-600* P)^2) / (600 * P)
    }

    if (sum > 9.23 && sum < 0)
    {
        print("Nincs a tartományban!")
    }
    else {
       print("A tartományban van!")
    }
    
    return (sum)

}

RollDice(N,K,P)