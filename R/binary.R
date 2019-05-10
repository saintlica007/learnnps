###  把一个整数x转化成k位 2进制的数  ###################################
binary <- function(x, k)
{
  tmp = NULL
  y = x
  if (x < 2 ^ k)
  {
    for (i in k - 1:k)
    {
      a = floor(y / 2 ^ i)
      #     print(c(i, a))
      tmp = c(tmp, a)
      y = y - a * 2 ^ i
    }
  }
  2 * (tmp - 0.5)
}
