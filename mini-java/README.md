# MiniJava Compiler

Overview

This project is a MiniJava compiler developed as part of the Compiler Construction course during the WS 2023/24 semester.
The goal is to translate Java classes into bytecode files with specific constraints.

Table of Contents

1. [Team](#team)
2. [Features](#features)
3. [Usage](#usage)
4. [Installation](#installation)
5. [Building the Compiler](#building-the-compiler)
6. [Running Tests](#running-tests)
7. [Documentation](#documentation)
8. [Contributing](#contributing)
9. [License](#license)


## Team
 - Fabian Rostomily
- Lara Schierenberg
- Anabel Stammer 

## Features

Syntax: The compiler supports a subset of syntactically correct Java classes with specific constraints outlined in the course specification.
Type Checking: A type-checking phase ensures that the abstract syntax tree is well-typed according to the specified restrictions.


## Usage

To use the MiniJava compiler, follow these steps:

1. [Clone the repository.]()
2. [Build the compiler.]()
3. [Run the compiler.]()


# Installation 

To install the MiniJava compiler, clone the repository:

```markdown
git clone https://github.com/LaraSchi/Compilerbau.git
```


# Building the Compiler

To build the compiler, follow these steps:

```markdown
cd mini-java-compiler
stack build
```

# Running Tests

To run the tests, execute the following commands:
```markdown
stack test
```

# Running the Compiler
 With the following commands you can run our main. 
```markdown
stack ghci
:main
```

To compile a specific MiniJava code, modify the 'fileContent' path in the Main.hs file (line 13) to point to the file containing the desired code for compilation.
