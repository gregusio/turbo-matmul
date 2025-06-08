result1 = open("output/result1.txt").readlines()
result2 = open("output/result2.txt").readlines()

if len(result1) != len(result2):
    print("The results have different number of lines.")
else:
    for i in range(len(result1)):
        if result1[i] != result2[i]:
            print(f"Difference found at line {i + 1}:")
            print(f"Result 1: {result1[i].strip()}")
            print(f"Result 2: {result2[i].strip()}")
            break
    else:
        print("Both results are identical.")
        print("No differences found between the results.")