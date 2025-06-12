result1 = open("output/result1.txt").readlines()
result2 = open("output/result2.txt").readlines()
true_result = open("output/result3.txt").readlines()

if len(result1) != len(true_result):
    print("Result 1 has a different number of lines than the true result.")
if len(result2) != len(true_result):
    print("Result 2 has a different number of lines than the true result.")

for i in range(len(true_result)):
    if result1[i] != true_result[i]:
        print(f"Result 1 differs at line {i + 1}:")
        print(f"Expected: {true_result[i].strip()}")
        print(f"Got: {result1[i].strip()}")
        
    if result2[i] != true_result[i]:
        print(f"Result 2 differs at line {i + 1}:")
        print(f"Expected: {true_result[i].strip()}")
        print(f"Got: {result2[i].strip()}")
else:
    print("Both results match the true result.")