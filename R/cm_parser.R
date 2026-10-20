#' Read CSV output produced by the NVML sampler
#'
#' @param sampler A sampler object returned by `cm_start()`, or a
#'   character path prefix used to build the sampler output paths.
#' @return A `CudaMonSession` object.
#' @examples
#' if (CudaMon:::nvml_is_available() && CudaMon:::nvml_device_count() > 0L) {
#'     sampler <- cm_start(period = 1)
#'     Sys.sleep(1)
#'     cm_stop(sampler)
#'     session <- cm_parser(sampler)
#'     session
#' }
#' @export
cm_parser <- function(sampler) {
    if (inherits(sampler, "nvml_sampler")) {
        device_metrics_path <- sampler$paths$device_metrics
        compute_processes_path <- sampler$paths$compute_processes
        events_path <- sampler$paths$events
        log_path <- sampler$paths$log
        metadata <- list(
            root_pid = sampler$root_pid,
            include_descendants = sampler$include_descendants,
            device_index = sampler$device_index
        )
    } else if (
        is.character(sampler) &&
            length(sampler) == 1L &&
            nzchar(sampler)
    ) {
        device_metrics_path <- paste0(sampler, "_device_metrics.csv")
        compute_processes_path <- paste0(sampler, "_compute_processes.csv")
        events_path <- paste0(sampler, "_events.csv")
        log_path <- paste0(sampler, "_sampler.log")
        metadata <- list()
    } else {
        stop(
            "sampler must be an 'nvml_sampler' object or a path prefix",
            call. = FALSE
        )
    }

    CudaMonSession(
        device_metrics = if (!file.exists(device_metrics_path) ||
            isTRUE(file.info(device_metrics_path)$size == 0)) {
            data.frame()
        } else {
            utils::read.csv(device_metrics_path, stringsAsFactors = FALSE)
        },
        compute_processes = if (!file.exists(compute_processes_path) ||
            isTRUE(file.info(compute_processes_path)$size == 0)) {
            data.frame()
        } else {
            utils::read.csv(compute_processes_path, stringsAsFactors = FALSE)
        },
        events = if (!file.exists(events_path) ||
            isTRUE(file.info(events_path)$size == 0)) {
            data.frame()
        } else {
            utils::read.csv(events_path, stringsAsFactors = FALSE)
        },
        paths = list(
            device_metrics = device_metrics_path,
            compute_processes = compute_processes_path,
            events = events_path,
            log = log_path
        ),
        metadata = metadata
    )
}
