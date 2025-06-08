# Turbo Matrix Multiplication

This repository contains a Haskell implementation of a optimized matrix multiplication algorithm, designed to be efficient and easy to use.

## How to Use

1. Clone the repository:
   ```bash
   git clone https://github.com/gregusio/turbo-matmul.git
   cd turbo-matmul
   ```

2. You can generate example matrices using the provided script:
   ```bash
   python input/generate_matrix.py
   ```

3. Compile the project using ghc:
   ```bash
   ghc -O2 -threaded -eventlog -package=time -iapp -isrc app/Main.hs
   ```

4. Run the application:
   ```bash
   app/Main
   ```  

5. To view a threadscope of the execution, you can use:
   ```bash
   threadscope Main.eventlog
   ```

## Features
- Efficient matrix multiplication using Haskell's concurrency features.
- Easy to generate and manipulate matrices.
- Provides a clear and simple interface for matrix operations.

## Requirements
- Haskell Platform (GHC)
- Python (for matrix generation script)
- ThreadScope (for performance analysis)

## License
This project is licensed under the MIT License. See the LICENSE file for details.