X <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
Y <- 5 * X^2 + 3 * X + 7
n <- length(X)

PolynomialReg <- function(X, Y) {
    detUpper <- detUpperCalculate(X, Y)
    detBottom <- detBottomCalculate(X, Y)

    A0 <- detUpper / detBottom
    A1 <- Summary(X)
    A2sum <- n * (X^3)
    A2 <- Summary(A2sum)

    plot(X, Y)

    lines(X, A2 * X^2 + A1 * X + A0, type = "l", lty = 1, col = "red")
}

detBottomCalculate <- function(X, Y) {
    sumx4 <- Summary(X^4)
    sumx3 <- Summary(X^3)
    sumx2 <- Summary(X^2)
    sumx <- Summary(X)

    detBottom <- sumx4 * CalcDet(sumx2, sumx, sumx, n) -
        sumx3 * CalcDet(sumx3, sumx, sumx2, n) +
        sumx2 * CalcDet(sumx3, sumx2, sumx2, sumx)

    return(detBottom)
}

detUpperCalculate <- function(X, Y) {
    sumx4 <- Summary(X^4)
    sumx3 <- Summary(X^3)
    sumx2y <- Summary(X^2, Y)
    sumx2 <- Summary(X^2)
    sumxy <- Summary(X, Y)
    sumx <- Summary(X)
    sumy <- Summary(Y)

    detUpper <- sumx4 * CalcDet(sumx2, sumxy, sumx, sumy) -
        sumx3 * CalcDet(sumx3, sumxy, sumx2, sumy) +
        sumx2y * CalcDet(sumx3, sumx2, sumx2, sumx)

    return(detUpper)
}

Summary <- function(X, Y) {
    sum <- 0
    if (missing(Y)) {
        for (i in 1:n)
        {
            sum <- sum + X[i]
        }
        return(sum)
    }

    for (i in 1:n)
    {
        sum <- sum + (X[i] * Y[i])
    }
    return(sum)
}

CalcDet <- function(a, b, c, d) {
    total <- (a * d) - (b * c)

    return(total)
}

PolynomialReg(X, Y)
