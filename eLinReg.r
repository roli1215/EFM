X <- c(1, 2, 3, 4)
Y <- c(21.6, 136.79, 1210.28, 8942.87)
n <- length(X)

linReg <- function(X, Y) {
    plot(X, Y)

    SumY <- Summary(Y)
    SumX <- Summary(X)
    SUMXY <- Summary(X, Y)
    SUMSQRX <- SummSQR(X)

    SumA1 <- (SUMXY * n) - (SumY * (-SumX))
    SumA2 <- (SUMSQRX * n) - (SumX * (-SumX))

    a <- 2
    b <- 3

    SumB1 <- (SumX * SumY) - (SumX * SUMXY)
    SumB2 <- (SUMSQRX * n) - (SumX * (-SumX))

    lines(X, a * X + b, type = "l", lty = 1, col = "blue")
}

Summary <- function(Z) {
    sum = 0
    for (i in 1:length(Z)) {
        sum = sum + Z[i]
    }
    return (sum)
}

SummSQR <- function(Z) { 
    sum = 0
    for (i in 1:length(Z)) {
        sum = sum + Z[i]^2 
    }
    return (sum)
}

Summary <- function(Z, X) {
    sum = 0
    for (i in 1:length(Z)) {
        sum = sum + (Z[i] * X[i])
    }
    return (sum)
}

linReg(X, Y)
