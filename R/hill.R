#' Hill Cipher Encryption
#'
#' Encrypts a plaintext message using the Hill cipher algorithm, a polygraphic substitution cipher based on linear algebra.
#' @param text the message that we will encrypt
#' @param beginning alphabet detection by initial letter
#' @param key_matrix square matrix of size NxN
#' @param mod the value of the module that we will be working with
#' @return our encrypted message
#' @export
#' @examples
#' msg <- "HELLO"
#' key <- matrix(c(1,3,2,4), nrow=2)
#' encrypted_msg <- hill_encrypt(msg, "A", key, 33)
#' print(encrypted_msg)
hill_encrypt <- function(text, beginning, key_matrix, mod) {
  n <- nrow(key_matrix)
  nums <- text_to_num(text, beginning)

  if (length(nums) %% n != 0) {
    nums <- c(nums, rep(0, n - length(nums) %% n))
  }

  blocks <- matrix(nums, nrow = n)
  encrypted_blocks <- (key_matrix %*% blocks) %% mod
  encrypted_text <- num_to_text(as.vector(encrypted_blocks), beginning)
  return(encrypted_text)
}

#' Hill Cipher Decryption
#'
#' Decrypts a ciphertext message that was encrypted using the Hill cipher algorithm.
#' @param ciphertext the message that we will decode
#' @param beginning alphabet detection by initial letter
#' @param key_matrix square matrix of size NxN
#' @param mod the value of the module that we will be working with
#' @return our decrypted message
#' @export
#' @examples
#' msg <- "PEALOJ"
#' key <- matrix(c(1,3,2,4), nrow=2)
#' decrypted_msg <- hill_decrypt(msg, "A", key, 33)
#' print(decrypted_msg)
hill_decrypt <- function(ciphertext, beginning, key_matrix, mod) {
  inv_key <- matrix_mod_inv(key_matrix, mod)
  nums <- text_to_num(ciphertext, beginning)
  n <- nrow(key_matrix)

  if (length(nums) %% n != 0) {
    nums <- c(nums, rep(0, n - length(nums) %% n))
  }

  blocks <- matrix(nums, nrow = n)
  decrypted_blocks <- (inv_key %*% blocks) %% mod
  decrypted_text <- num_to_text(as.vector(decrypted_blocks), beginning)
  return(decrypted_text)
}
