
#' Vigenère Cipher Encryption
#'
#' Encrypts a plaintext message using the Vigenère cipher algorithm, a polyalphabetic substitution cipher
#' @param msg The plaintext message to encrypt (character string)
#' @param beginning The starting character of the alphabet (e.g., "A" for English)
#' @param key_word The keyword for encryption (character string)
#' @param mod The modulus value (typically 26 for English alphabet)
#' @return The encrypted ciphertext
#' @export
#' @examples
#' vigenere_encrypt("HELLO", "A", "KEY", 26)
vigenere_encrypt <- function(msg, beginning, key_word, mod) {
  res <- ""
  beg_int <- utf8ToInt(beginning)
  kw <- unlist(strsplit(by_key_word_letters(msg, key_word), ""))
  msg_to_list <- unlist(strsplit(msg, ""))
  for (i in 1:nchar(msg)) {
    curr_chr <- intToUtf8(((utf8ToInt(msg_to_list[i]) + utf8ToInt(kw[i]) - 2 * beg_int) %% mod) + beg_int)
    res <- paste0(res, curr_chr)
  }
  return(res)
}

#' Vigenère Cipher Decryption
#'
#' Decrypts a message encrypted with the Vigenère cipher algorithm
#' @param encrypted_msg The ciphertext to decrypt (character string)
#' @param beginning The starting character of the alphabet (e.g., "A" for English)
#' @param key_word The keyword used for encryption (character string)
#' @param mod The modulus value (must match encryption modulus)
#' @return The decrypted plaintext
#' @examples
#' vigenere_decrypt("RIJVS", "A", "KEY", 26)
#' @export
vigenere_decrypt <- function(encrypted_msg, beginning, key_word, mod) {
  decrypted <- ""
  beg_int <- utf8ToInt(beginning)
  kw <- unlist(strsplit(by_key_word_letters(encrypted_msg, key_word), ""))
  msg_to_list <- unlist(strsplit(encrypted_msg, ""))

  for (i in 1:nchar(encrypted_msg)) {
    curr_code <- (utf8ToInt(msg_to_list[i]) - utf8ToInt(kw[i]) + mod) %% mod
    curr_chr <- intToUtf8(curr_code + beg_int)
    decrypted <- paste0(decrypted, curr_chr)
  }

  return(decrypted)
}
