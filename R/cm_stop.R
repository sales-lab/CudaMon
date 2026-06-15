#' Stop an NVML background sampler
#'
#' @param sampler A sampler object returned by `cm_start()`.
#' @return Invisibly returns the sampler.
#' @examples
#' if (CudaMon:::nvml_is_available() && CudaMon:::nvml_device_count() > 0L) {
#'     sampler <- cm_start(period = 1)
#'     cm_stop(sampler)
#' }
#' @export
cm_stop <- function(sampler) {
    if (!inherits(sampler, "nvml_sampler")) {
        stop("sampler must inherit from 'nvml_sampler'", call. = FALSE)
    }

    if (!is.null(sampler$process)) {
        wait_seconds <- if (
            !is.null(sampler$period) &&
                is.finite(sampler$period)
        ) {
            max(0, as.numeric(sampler$period))
        } else {
            1
        }
        Sys.sleep(wait_seconds)
        sampler$process$interrupt()
        sampler$process$wait(timeout = 1000L)
        if (sampler$process$is_alive()) {
            sampler$process$kill()
        }
    }
    invisible(sampler)
}
