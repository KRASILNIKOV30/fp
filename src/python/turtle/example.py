from turtle import *

k = 10

tracer(0)
left(90)
pendown()
begin_fill()

for i in range(3):
    forward(k*12)
    right(120)

end_fill()
penup()

count = 0
canvas = getcanvas()
for x in range(-30, 30):
    for y in range(-30, 30):
        setpos(x*k, y*k)
        dot(1)
        if canvas.find_overlapping(x*k, y*k, x*k, y*k) == (5,):
            count += 1

print(count)
done()