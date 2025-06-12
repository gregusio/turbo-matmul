import random

rows1 = 512
cols1 = 512

with open("input/matrix1.txt", "w") as f:
    for _ in range(rows1):
        row = [str(random.randint(-99, 99)) for _ in range(cols1)]
        f.write(" ".join(row) + "\n")

rows2 = cols1
cols2 = 512

with open("input/matrix2.txt", "w") as f:
    for _ in range(rows2):
        row = [str(random.randint(-99, 99)) for _ in range(cols2)]
        f.write(" ".join(row) + "\n")