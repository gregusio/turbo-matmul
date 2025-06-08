import random

rows = 1000
cols = 900

with open("input/matrix1.txt", "w") as f:
    for _ in range(rows):
        row = [str(random.randint(0, 99)) for _ in range(cols)]
        f.write(" ".join(row) + "\n")

with open("input/matrix2.txt", "w") as f:
    for _ in range(cols):
        row = [str(random.randint(0, 99)) for _ in range(rows)]
        f.write(" ".join(row) + "\n")