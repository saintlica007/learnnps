##page test score
PG <- function(x, groupb, groupt)
{
  pg = 0
  ugt = unique(groupt)
  k = length(ugt)
  for (i in 1:k)
  {
    xi = x[groupt == ugt[i]]
    pg = pg + sum(xi) * i
  }
  pg
}
