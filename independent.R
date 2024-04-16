
matrix <- matrix(c(42,28,3,17,89,21), nrow = 3,    ncol = 2)

IndependencyTest <- function(K)
{
    row = rowSums(K)
    column = colSums(K)

    print(row)
    print(column)

    r = length(row)
    s = length(column)

    print(r)
    print(s)

    n <- sum(row)
    
    print(n)
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
    
    if (result < qchisq(p=.95, df=(r-1)*(s-1) )){
     print('Az erteket elfogadjuk!')
  } else {
     print('Az erteket nem fogadjuk el!')
  }
    sprintf("Result: %.3f", result)
}
IndependencyTest(matrix)