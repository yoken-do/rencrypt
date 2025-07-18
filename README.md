
# rencrypt

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

An educational package implementing classical encryption algorithms 

## Installation

Install the development version from GitHub:

``` r
# Install devtools if not already installed
if (!require("devtools")) install.packages("devtools")

# Install rencrypt
devtools::install_github("yoken-do/rencrypt")
```

## Basic Usage

### Hill Cipher Example

Matrix-based polygraphic substitution

``` r
library(rencrypt)
msg <- "HELLO"
key <- matrix(c(1,3,2,4), nrow=2) # 2x2 encryption matrix
encrypted_msg <- hill_encrypt(msg, "A", key, 33)
print(encrypted_msg)
decrypted_msg <- hill_decrypt(encrypted_msg, "A", key, 33)
print(decrypted_msg)
```

## Implemented Algorithms

- Hill Cipher (polygraphic substitution cipher)
- Caesar Cipher (shift cipher)
- Vigenère Cipher (polyalphabetic substitution)

## Educational Purpose

⚠️ This package is for educational use only 

