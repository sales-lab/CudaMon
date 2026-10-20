#' Return a process ID and, optionally, all descendant process IDs
#'
#' @param pid Root process identifier.
#' @param include_descendants Whether to include child processes recursively.
#' @return Integer vector of process identifiers.
#' @noRd
process_pids <- function(pid, include_descendants = TRUE) {
    pid <- as.integer(pid)
    if (!include_descendants) {
        return(pid)
    }

    root <- tryCatch(
        ps::ps_handle(pid),
        error = function(...) NULL
    )
    if (is.null(root)) {
        return(pid)
    }

    children <- tryCatch(
        ps::ps_children(root, recursive = TRUE),
        error = function(...) list()
    )
    child_pids <- vapply(children, ps::ps_pid, integer(1))

    as.integer(c(pid, child_pids))
}
