for n in range(1, 100):
    summa = 0
    s = bin(n)[2:] 
    s = str(s)
    #for i in range(len(s)):
   #     summa += int(s[i])
    summa = sum(list(s))
    if summa % 2 != 0:
        s += "11"
    else:
        s += "00"
    r = int(s, 2) 
    if r > 114:
        print(r)
        break