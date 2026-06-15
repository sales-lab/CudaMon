#' Record a workflow step during an active NVML sampling session
#'
#' @param sampler A sampler object returned by `cm_start()`.
#' @param step A short label identifying the current workflow step.
#' @return Invisibly returns the sampler.
#' @examples
#' if (CudaMon:::nvml_is_available() && CudaMon:::nvml_device_count() > 0L) {
#'     sampler <- cm_start(period = 1)
#'     cm_timestamp(sampler, "example")
#'     cm_stop(sampler)
#' }
#' @export
cm_timestamp <- function(sampler, step) {
    if (!inherits(sampler, "nvml_sampler")) {
        stop("sampler must inherit from 'nvml_sampler'", call. = FALSE)
    }

    if (!is.character(step) || length(step) != 1L || !nzchar(step)) {
        stop("step must be a single non-empty string", call. = FALSE)
    }

    event_row <- data.frame(
        timestamp = format(Sys.time(), tz = "UTC", usetz = TRUE),
        root_pid = as.integer(sampler$root_pid),
        step = step,
        stringsAsFactors = FALSE
    )

    utils::write.table(
        event_row,
        file = sampler$paths$events,
        sep = ",",
        row.names = FALSE,
        col.names = !file.exists(sampler$paths$events),
        append = file.exists(sampler$paths$events),
        qmethod = "double"
    )
    invisible(sampler)
}
