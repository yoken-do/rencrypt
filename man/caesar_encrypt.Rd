% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/caesar.R
\name{caesar_encrypt}
\alias{caesar_encrypt}
\title{Caesar Cipher Encryption}
\usage{
caesar_encrypt(alphabet, shift, message)
}
\arguments{
\item{alphabet}{Character string containing the alphabet to use}

\item{shift}{Integer specifying the shift value for encryption}

\item{message}{Character string to be encrypted}
}
\value{
Encrypted message
}
\description{
Encrypts a message using the Caesar cipher algorithm
}
\examples{
encrypted_msg <- caesar_encrypt("abcdefghijklmnopqrstuvwxyz", 3, "hello")
print(encrypted_msg)
}
