x <-c(1,2,3,4,5) 

empirical <- function(x)
{
    s = 0
    avg = 0
    for (i in 1:length(x))
    {
        s = s + x[i]
    }
    avg = s / length(x)
    return (avg)
}

test <- function(x,m,s)
{
    u = (empirical(x) - m) / (s/sqrt(length(x)))

    if (abs(u) < 1.96)
    {
        return ("Elfogadjuk")
    }
    else
    {
        return ("Elutasitjuk")
    }
}
test(x,3,5)