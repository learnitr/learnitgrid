#' Encrypt solution files into `..._solution.xxx.aes` files
#'
#' @description
#' These functions manage encryption and decryption of solution files. The
#' password must be defined in an option `learnitgrid.key` or an environment
#' variable `LEARNITGRID_KEY` under a digest form.
#'
#'
#' @param key The key to use for encryption/decryption. If not provided, it is
#'  asked
#' @param error If `TRUE`, an error is generate in case the file encryption or
#'  decryption fails.
#'
#' @return `NULL` invisibly.
#' @export
encrypt_solutions <- function(key = NULL, error = TRUE) {
  # Get a list of solution files
  files <- fs::dir_ls(here::here(), all = TRUE, recurse = TRUE,
    type = "file", regexp = "_solution\\.[a-zA-Z0-9]+$")
  if (!length(files)) {
    if (isTRUE(error)) {
      stop("No solution files found")
    } else {
      return(NULL)
    }
  }
  if (is.null(key))
    key <- set_key()

  encrypt_one <- function(file) {
    dest_file <- paste0(file, ".aes")
    cyphr::encrypt_file(file, key = cyphr::key_openssl(key), dest = dest_file)
    # Check that the dest_file is created before removing unencrypted version
    if (!fs::file_exists(dest_file))
      stop("problem when encrypting ", file, ", process interrupted")
    fs::file_delete(file)
    file
  }
  unlist(purrr::map(files, encrypt_one))
}

#' @export
#' @rdname encrypt_solutions
decrypt_solutions <- function(key = NULL, error = TRUE) {
  # Get a list of encrypted solution files
  enc_files <- fs::dir_ls(here::here(), all = TRUE, recurse = TRUE,
    type = "file", regexp = "_solution\\.[a-zA-Z0-9]+.aes$")
  if (!length(enc_files)) {
    if (isTRUE(error)) {
      stop("No encoded solution files found")
    } else {
      return(NULL)
    }
  }
  if (is.null(key))
    key <- set_key()

  decrypt_one <- function(file) {
    dest_file <- fs::path_ext_remove(file)
    cyphr::decrypt_file(file, key = cyphr::key_openssl(key), dest = dest_file)
    # Check that the file is actually created before removing encrypted version
    if (!fs::file_exists(dest_file))
      stop("problem when decrypting ", file, ", process interrupted")
    fs::file_delete(file)
    file
  }
  unlist(purrr::map(enc_files, decrypt_one))
}

#' @export
#' @rdname encrypt_solutions
set_key <- function() {
  # Try first to retrieve it from a file
  key_file <- here::here("tests/.key")
  if (fs::file_exists(key_file))
    return(qs::qread(key_file))
  pass <- askpass::askpass("Veuillez entrer le mode de passe :")
  if (is.null(pass)) # User cancelled
    return()
  if (digest::digest(pass) != "cfe7383614aacd5035642bf60d7d1a3e")
    stop("Invalid password")
  key <- charToRaw(pass) |> openssl::md5()
  class(key) <- c("aes", "raw")
  # Save this key
  qs::qsave(key, file = key_file)
  invisible(key)
}
