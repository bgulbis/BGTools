# gzip_files.R

#' Compress data files
#'
#' \code{gzip_files} compresses all data files in a directory using gzip, if not
#' already compressed
#'
#' This function takes the name of a directory and checks all of the data files
#' within the directory to if they are already compressed. For each file that is
#' not, it will compress the file using \code{\link[R.utils]{gzip}}.
#'
#' @param dir.name A string with the name of the directory containing the data
#'   files. Defaults to the current working directory.
#' @param pattern An optional regular expression. Only file names which match
#'   the regular expression will be compressed.
#'
#' @seealso
#' \code{\link[R.utils]{gzip}},
#' \code{\link[base]{regex}} for regular expressions
#'
#' @export
gzip_files <- function(dir.name = getwd(), pattern = NULL) {
    comp.files <- list.files(dir.name, pattern = pattern, full.names=TRUE)
    lapply(comp.files, function(x)
        if (R.utils::isGzipped(x) == FALSE) R.utils::gzip(x, overwrite=TRUE)
    )
}

#' Uncompress data files
#'
#' \code{ungzip_files} uncompresses all data files in a directory using gzip
#'
#' This function takes the name of a directory and uncompresses each file that
#' is gzipped.
#'
#' @param dir.name A string with the name of the directory containing the data
#'   files. Defaults to the current working directory.
#' @param pattern An optional regular expression. Only file names which match
#'   the regular expression will be compressed.
#'
#' @seealso \code{\link[R.utils]{gzip}}, \code{\link[base]{regex}} for regular
#'   expressions
#'
#' @export
ungzip_files <- function(dir.name = getwd(), pattern = NULL) {
    comp.files <- list.files(dir.name, pattern = pattern, full.names=TRUE)
    lapply(comp.files, function(x)
        if (R.utils::isGzipped(x) == TRUE) R.utils::decompressFile(x, ext = "gz",
                                                                   FUN = gzfile)
    )
}
