
matrix <- matrix(c(42,28,3,17,89,21), nrow = 3,    ncol = 2)

IndependencyTest <- function(K)
{
    row = rowSums(K)
    column = colSums(K)

    r = length(row)
    s = length(column)

    n <- sum(row)
    
    sumR <- 0

    for (i in 1:r)
    {
        sumS <- 0

        for (j in 1:s)
        {
            sumS = sumS + ((K[i,j] - (row[i] * column[j])/n)^2 / (row[i] * column[j]))
        }
        sumR = sumR + sumS
    }
    result = n * sumR
    sprintf("Result: %.3f", result)
}
IndependencyTest(matrix)