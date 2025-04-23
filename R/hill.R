#' Hill Cipher Encryption
#'
#' Encrypts a plaintext message using the Hill cipher algorithm, a polygraphic substitution cipher based on linear algebra.
#' @param text input message
#' @param key_matrix key matrix
#' @return encrypted message
#' @export
hill_encrypt <- function(text, key_matrix) {
  n <- nrow(key_matrix)
  nums <- text_to_num(text)

  if (length(nums) %% n != 0) {
    nums <- c(nums, rep(0, n - length(nums) %% n))
  }

  blocks <- matrix(nums, nrow = n)

  encrypted_blocks <- (key_matrix %*% blocks) %% 26

  encrypted_text <- num_to_text(as.vector(encrypted_blocks))
  return(encrypted_text)
}

#' Hill Cipher Decryption
#'
#' Decrypts a ciphertext message that was encrypted using the Hill cipher algorithm.
#' @param ciphertext encrypted message
#' @param key_matrix key matrix (inverse of encryption key matrix)
#' @return decrypted message
#' @export
hill_decrypt <- function(ciphertext, key_matrix) {
  inv_key <- matrix_mod_inv(key_matrix, 26)
  hill_encrypt(ciphertext, inv_key)
}
