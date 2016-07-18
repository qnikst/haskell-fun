ones : Stream Int
ones = 1 :: ones

countFrom : Num numType => numType -> Stream numType
countFrom x = x :: countFrom (x + 1)

