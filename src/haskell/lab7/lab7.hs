secondToLast [x, y] = x
secondToLast (x:xs) = secondToLast xs

main = do
    print (secondToLast "abcd")
