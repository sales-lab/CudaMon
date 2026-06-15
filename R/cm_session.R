#' Construct a CudaMon session object
#'
#' @param device_metrics Data frame with GPU device-level samples. Expected
#'   columns include:
#'   \describe{
#'     \item{timestamp}{Sample timestamp in UTC.}
#'     \item{sampler_pid}{Root process identifier tracked by the sampler.}
#'     \item{device_index}{Zero-based GPU device index.}
#'     \item{gpu_utilization_pct}{Percent of the sampling interval during which
#'       at least one kernel was executing on the GPU. This is an activity
#'       ratio, not a measure of how saturated the GPU cores were.}
#'     \item{memory_utilization_pct}{Percent of the sampling interval during
#'       which global device memory was being read from or written to. This is
#'       memory-controller activity, not the fraction of GPU memory allocated.}
#'     \item{temperature_c}{GPU temperature in degrees Celsius.}
#'     \item{power_usage_mw}{Instantaneous board power draw in milliwatts.}
#'     \item{memory_used_bytes}{Bytes of device memory currently allocated.}
#'     \item{memory_total_device_bytes}{Total bytes of physical device memory
#'       available.}
#'   }
#' @param compute_processes Data frame with GPU process-level samples. Expected
#'   columns include:
#'   \describe{
#'     \item{timestamp}{Sample timestamp in UTC.}
#'     \item{sampler_pid}{Root process identifier tracked by the sampler.}
#'     \item{device_index}{Zero-based GPU device index.}
#'     \item{pid}{Process identifier reported by NVML as using the GPU.}
#'     \item{tracked_pid}{Tracked process identifier after filtering to the
#'       requested root process and, optionally, its descendants.}
#'     \item{is_root_pid}{Whether \code{tracked_pid} is the root process passed
#'       to the sampler.}
#'     \item{used_gpu_memory_bytes}{Bytes of GPU memory allocated by the
#'       process on the sampled device, when reported by NVML.}
#'     \item{gpu_instance_id}{MIG GPU instance identifier, or \code{NA} when it
#'       does not apply or is not reported.}
#'     \item{compute_instance_id}{MIG compute instance identifier, or \code{NA}
#'       when it does not apply or is not reported.}
#'   }
#' @param events Data frame with event markers recorded during sampling.
#' @param paths Named list of output paths.
#' @param metadata Named list with session metadata.
#' @return An object of class `CudaMonSession`.
#' @examples
#' session <- CudaMonSession()
#' session
#' @export
CudaMonSession <- function(
    device_metrics = data.frame(),
    compute_processes = data.frame(),
    events = data.frame(),
    paths = list(),
    metadata = list()
) {
    structure(
        list(
            device_metrics = device_metrics,
            compute_processes = compute_processes,
            events = events,
            paths = paths,
            metadata = metadata
        ),
        class = "CudaMonSession"
    )
}

#' @export
print.CudaMonSession <- function(x, ...) {
    cat("CudaMonSession\n")
    cat("  Device samples: ", nrow(x$device_metrics), "\n", sep = "")
    cat("  GPU process samples: ", nrow(x$compute_processes), "\n", sep = "")
    cat("  Event markers: ", nrow(x$events), "\n", sep = "")

    if (length(x$paths) > 0L) {
        if (!is.null(x$paths$device_metrics)) {
            cat(
                "  Device metrics path: ",
                x$paths$device_metrics,
                "\n",
                sep = ""
            )
        }
        if (!is.null(x$paths$compute_processes)) {
            cat(
                "  Compute processes path: ",
                x$paths$compute_processes,
                "\n",
                sep = ""
            )
        }
        if (!is.null(x$paths$events)) {
            cat("  Events path: ", x$paths$events, "\n", sep = "")
        }
        if (!is.null(x$paths$log)) {
            cat("  Sampler log path: ", x$paths$log, "\n", sep = "")
        }
    }

    if (length(x$metadata) > 0L) {
        if (!is.null(x$metadata$root_pid)) {
            cat("  Root PID: ", x$metadata$root_pid, "\n", sep = "")
        }
        if (!is.null(x$metadata$include_descendants)) {
            cat(
                "  Include descendants: ",
                x$metadata$include_descendants,
                "\n",
                sep = ""
            )
        }
    }

    invisible(x)
}
