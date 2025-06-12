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
   ghc -O2 -threaded -eventlog -package=time -package=matrix -iapp -isrc app/Main.hs -o turbo-matmul
   ```

    Alternatively, you can use Cabal to build the project:
   ```bash
   cabal build
   ```

4. Run the application
   ```bash
   ./turbo-matmul +RTS -N -l
    ```
    
    or if you used Cabal:
    ```bash
    cabal run turbo-matmul -- +RTS -N -ls
    ```

5. To view a threadscope of the execution, you can use:
   ```bash
   threadscope turbo-matmul.eventlog
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