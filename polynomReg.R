X <- c(1, 2, 3, 4)
Y <- 5 * X^2 + 3 * X + 2
n <- length(X)

PolynomialReg <- function(X, Y) {
    detUpperA0 <- detUpperCalculateA0(X, Y)
    detUpperA1 <-detUpperCalculateA1(X,Y)
    detUpperA2 <-detUpperCalculateA2(X,Y)
    detBottom <- detBottomCalculate(X, Y)

    A0 <- detUpperA0 / detBottom
    A1 <- detUpperA1 / detBottom
    A2 <- detUpperA2 / detBottom

    plot(X, Y)

    lines(X, A2 * X^2 + A1 * X + A0, type = "l", lty = 1, col = "red")

    print(A0)
    print(A1)
    print(A2)
    print(Y)
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

detUpperCalculateA0 <- function(X, Y) {
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
detUpperCalculateA1 <- function(X, Y) {
    sumx4 <- Summary(X^4)
    sumx3 <- Summary(X^3)
    sumx2y <- Summary(X^2, Y)
    sumx2 <- Summary(X^2)
    sumxy <- Summary(X, Y)
    sumx <- Summary(X)
    sumy <- Summary(Y)

    detUpper <- sumx4 * CalcDet(sumxy, sumx, sumy, n) -
        sumx2y * CalcDet(sumx3, sumx, sumx2, n) +
        sumx2 * CalcDet(sumx3, sumxy, sumx2, sumy)

    return(detUpper)
}
detUpperCalculateA2 <- function(X, Y) {
    sumx4 <- Summary(X^4)
    sumx3 <- Summary(X^3)
    sumx2y <- Summary(X^2, Y)
    sumx2 <- Summary(X^2)
    sumxy <- Summary(X, Y)
    sumx <- Summary(X)
    sumy <- Summary(Y)

    detUpper <- sumx2y * CalcDet(sumx2, sumx, sumx, n) -
        sumx3 * CalcDet(sumxy, sumx, sumy, n) +
        sumx2 * CalcDet(sumxy, sumx2, sumy, sumx)

    return(detUpper)
}

Summary <- function(X, Y) {
    sum <- 0
    if (missing(Y)) 
    {
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
