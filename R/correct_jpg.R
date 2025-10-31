#' Correct magic number of JPEG or EXIF file
#'
#' It happens that the magic number of a JPEG or EXIF file misses the first byte. This function corrects this after checking that the other part of the magic number is correct
#' @name correct_jpeg
#' @param infile Character string with the name of a JPEG of EXIF file
#' @param outfile Character string with the name of the (corrected) copy of this file
#' @return NULL
#' @export
#' @section Details:
#'
#' The following cases can occur:
#' \describe{
#' \item{input file can't be found}{an error message is show and no file is created}
#' \item{input file contains a valid magic number}{the file is copied to the output file; a copy-message is shown}
#' \item{input file misses the first byte of magic number but the other bytes are good}{the first byte is added and copied to the output file; a repair-message is shown}
#' \item{input file misses the first byte of magic number and the other bytes are not good}{the file is copied to the output file; a not-repair-message is shown}
#' }

correct_jpeg <- function(infile, outfile) {
  # correct JPEG of EXIF file when first byte is missing
  first14a <- as.raw(c(0xff, 0xd8, 0xff, 0xe0)) # JFIF-formaat magic number
  first14b <- as.raw(c(0xff, 0xd8, 0xff, 0xe1)) # EXIF-formaat magic number
  first24a <- first14a[2:4]
  first24b <- first14b[2:4]
  # De ontbrekende eerste byte van de JPEG 'Start of Image' (SOI): FF (hex)
  missing_byte <- as.raw(0xFF)

  tryCatch({
    filesize <- file.size(infile)
    if (is.na(filesize))
      stop("File not found")
    # Lees het hele bestand in als een ruwe vector (raw vector)
    binaire_data <- readBin(con = infile, what = "raw", n = filesize)
    if ((all(binaire_data[1:4] == first14a) |
         all(binaire_data[1:4] == first14b))) {
      msg <- "File copied to \n"
      fixed_data <- binaire_data
    } else if ((all(binaire_data[1:3] == first24a) |
                all(binaire_data[1:3] == first24b))) {
      msg <- "File repaired to \n"
      fixed_data <- c(missing_byte, binaire_data)
    } else {
      fixed_data <- binaire_data
      msg <- "File not repaired but copied to \n"
    }
    writeBin(object = fixed_data, con = outfile)

    cat(paste0(msg, outfile, "\n" ))

  }, error = function(e) {
    cat(paste0(
      "Error when repairing file\n",
      e$message,
      "\n"
    ))
  })
  NULL
}
