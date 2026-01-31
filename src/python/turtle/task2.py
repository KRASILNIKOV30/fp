from turtle import *

k = 20
tracer(0)
left(90)

begin_fill()
for i in range(3):
    forward(14 * k)
    right(120)
end_fill()

penup()
canvas = getcanvas()
count = 0
for x in range(-40, 40):
    for y in range(-40, 40):
        items = canvas.find_overlapping(x*k, y*k, x*k, y*k)
        if items == (5,):
            count += 1

print(count)
done()

