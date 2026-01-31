
cislo = bin(214)[2:]
one, zero, otv  = [], [] , []
for i in range(len(cislo) - 1):
    if cislo[i + 1] != '0':
        one.append(int(cislo[:i + 1], 2))
        zero.append(int(cislo[i+1:], 2))
for j in range(len(one)):
    otv.append(int('1' + '0' * int(zero[j]) + '1'*int(one[j]-1),2))
print(min(otv))