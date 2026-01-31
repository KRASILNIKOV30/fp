from turtle import *

k = 20

tracer(0)
pendown()
left(90)
for i in range(3):
    forward(20 * k)
    right(90)
    forward(4 * k)
    right(90)

for i in range(3):
    forward(6 * k)
    right(90)
    forward(13 * k)
    right(90)

canvas = getcanvas()

penup()
count = 0
for x in range(-20, 20):
    for y in range(-5, 30):
        setpos(x*k, y*k)
        dot(3)
        if canvas.find_overlapping(x*k, y*k, x*k, y*k) == (5,):
            count += 1

print(count)
done()

