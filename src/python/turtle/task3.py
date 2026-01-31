from turtle import *

# Направо 315
# Повтори 7 [Вперёд 16 Направо 45 Вперёд 8 Направо 135].
k = 20
tracer(0)
left(90)
right(315)

begin_fill()
for i in range(6):
    forward(16 * k)
    right(45)
    forward(8 * k)
    right(135)
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

