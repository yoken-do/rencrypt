# for Hill Cipher Encryption

text_to_num <- function(text, beginning) {
  text <- toupper(text)
  utf8ToInt(text) - utf8ToInt(beginning)
}

num_to_text <- function(nums, beginning) {
  intToUtf8(nums + utf8ToInt(beginning))
}

mod_inv <- function(a, m) {
  if (a == 0) return(NA)
  a <- a %% m
  for (i in 1:m) {
    if ((a * i) %% m == 1) {
      return(i)
    }
  }
  return(NA)
}

matrix_mod_inv <- function(mat, mod) {
  det_val <- round(det(mat)) %% mod
  det_inv <- mod_inv(det_val, mod)

  if (is.na(det_inv)) {
    stop("Матрица необратима по модулю ", mod,
         ": det = ", det_val, " не имеет обратного")
  }

  adjugate <- t(matrix(
    c(mat[2,2], -mat[1,2],
      -mat[2,1], mat[1,1]),
    nrow = 2
  )) %% mod

  inv_mat <- (adjugate * det_inv) %% mod
  return(matrix(round(inv_mat), nrow = 2))
}
