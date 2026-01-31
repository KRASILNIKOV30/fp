from turtle import *

tracer(0)
k = 40
left(90)
pendown()

for i in range(5):
    forward(9 * k)
    right(90)
    forward(3 * k)
    right(90)

penup()

for x in range(-1, 11):
    for y in range(-1, 11):
        setpos(x*k, y*k)
        dot(5)

done()