#' Caesar Cipher Encryption
#'
#' Encrypts a message using the Caesar cipher algorithm
#' @param alphabet Character string containing the alphabet to use
#' @param shift Integer specifying the shift value for encryption
#' @param message Character string to be encrypted
#' @return Encrypted message
#' @export
#' @examples
#' encrypted_msg <- caesar_encrypt("abcdefghijklmnopqrstuvwxyz", 3, "hello")
#' print(encrypted_msg)
caesar_encrypt <- function(alphabet, shift, message) {

  message_chars <- unlist(strsplit(message, ""))
  alphabet_chars <- unlist(strsplit(alphabet, ""))
  length <- length(alphabet_chars)

  if (length(message_chars) == 0) {
    return(character(0))
  }

  shift <- shift %% length
  if (shift == 0) {
    return(message_chars)
  }

  new_alphabet <- c(alphabet_chars[(shift + 1):length],
                    alphabet_chars[1:shift])

  res <- character(length(message_chars))
  for (i in seq_along(message_chars)) {
    ch <- message_chars[i]
    ind <- which(alphabet_chars == ch)
    if (length(ind) == 0) {
      res[i] <- ch
    } else {
      res[i] <- new_alphabet[ind]
    }
  }

  return(paste(res, collapse=""))
}

#' Caesar Cipher Decryption
#'
#' Decrypts a message encrypted with the Caesar cipher
#' @param alphabet Character string containing the alphabet used for encryption
#' @param shift Integer specifying the shift value that was used for encryption
#' @param encrypted_message Character string to be decrypted
#' @return Decrypted message
#' @export
#' @examples
#' decrypted_msg <- caesar_decrypt("abcdefghijklmnopqrstuvwxyz", 3, "khoor")
#' print(decrypted_msg)
caesar_decrypt <- function(alphabet, shift, encrypted_message) {
  caesar_encrypt(alphabet, -shift, encrypted_message)
}
