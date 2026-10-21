# Copyright (c) 2025-2026 Gabriele Sales
# Copyright (c) 2025 Mohammad Amin Zadenoori
#


#' Interpret a raw NVML C return value
#'
#' @param res A value returned by an `nvml_*_c` routine: either a negative
#'   integer encoding an NVML error code, or the success payload (a list or a
#'   non-negative integer).
#' @return The positive NVML error code as an integer, or `NULL` on success.
#' @noRd
fault_code <- function(res) {
    if (is.integer(res) && length(res) == 1L && res < 0L) {
        return(-res)
    }

    NULL
}


#' Stop with a decoded NVML error if the return value indicates a fault
#'
#' @param res A value returned by an `nvml_*_c` routine, as in `fault_code()`.
#' @return `res`, invisibly, when no fault is detected; otherwise stops with
#'   an error describing the NVML failure.
#' @noRd
fault_check <- function(res) {
    code <- fault_code(res)
    if (!is.null(code)) {
        msg <- .Call("nvml_error_string_c", code, PACKAGE = "CudaMon")
        stop(sprintf("NVML failure: %s (code: %d)", msg, code), call. = FALSE)
    }

    invisible(res)
}


#' Check whether NVML is available
#'
#' @return `TRUE` if NVML can be initialised and queried, otherwise `FALSE`.
#'   This reports on the driver stack; it does **not** imply that any GPU is
#'   present. Use `nvml_device_count() > 0` to check for devices.
#' @examples
#' nvml_is_available()
#' @export
nvml_is_available <- function() {
    isTRUE(.Call("nvml_is_available_c", PACKAGE = "CudaMon"))
}


#' Number of NVML-visible devices
#'
#' @return An integer count of NVML-visible GPU devices. Stops with an error
#'   if the NVML query fails.
#' @examples
#' if (nvml_is_available()) {
#'     nvml_device_count()
#' }
#' @export
nvml_device_count <- function() {
    as.integer(fault_check(.Call("nvml_device_count_c", PACKAGE = "CudaMon")))
}


#' List NVML-visible devices
#'
#' @return A data frame with one row per GPU and stable device metadata.
#'   Columns: `device_index` (zero-based index), `name`, `uuid`, and
#'   `memory_total_device_bytes`. Returns a zero-row data frame with the same
#'   column types when no devices are visible. Stops with an error if any
#'   per-device query fails.
#' @examples
#' if (nvml_is_available() && nvml_device_count() > 0L) {
#'     nvml_list_devices()
#' }
#' @export
nvml_list_devices <- function() {
    count <- nvml_device_count()

    if (count == 0L) {
        return(data.frame(
            device_index = integer(),
            name = character(),
            uuid = character(),
            memory_total_device_bytes = double(),
            stringsAsFactors = FALSE
        ))
    }

    devices <- lapply(seq_len(count) - 1L, function(idx) {
        res <- fault_check(
            .Call("nvml_device_info_c", idx, PACKAGE = "CudaMon")
        )

        data.frame(
            device_index = as.integer(res[[1L]]),
            name = as.character(res[[2L]]),
            uuid = as.character(res[[3L]]),
            memory_total_device_bytes = as.double(res[[4L]]),
            stringsAsFactors = FALSE
        )
    })

    do.call(rbind, devices)
}


#' List compute processes active on NVML-visible GPUs
#'
#' @param device_index Optional integer GPU index. If `NULL`, query all
#'   devices.
#' @param pid Optional integer vector used to filter the returned processes.
#' @return A data frame with one row per GPU process observation. Columns:
#'   `device_index`, `pid`, `used_gpu_memory_bytes`, `gpu_instance_id`, and
#'   `compute_instance_id`. The MIG instance ids are `NA` when they do not
#'   apply or are not reported. Returns a zero-row data frame with the same
#'   column types when no processes are found.
#' @examples
#' if (nvml_is_available() && nvml_device_count() > 0L) {
#'     nvml_list_compute_processes()
#' }
#' @export
nvml_list_compute_processes <- function(device_index = NULL, pid = NULL) {
    if (is.null(device_index)) {
        indices <- seq_len(nvml_device_count()) - 1L
    } else {
        if (!is.numeric(device_index)) {
            stop("device_index must be NULL or a numeric vector", call. = FALSE)
        }
        indices <- as.integer(device_index)
    }

    if (length(indices) == 0L) {
        return(data.frame(
            device_index = integer(),
            pid = integer(),
            used_gpu_memory_bytes = double(),
            gpu_instance_id = integer(),
            compute_instance_id = integer(),
            stringsAsFactors = FALSE
        ))
    }

    process_frames <- lapply(indices, function(idx) {
        res <- fault_check(
            .Call(
                "nvml_device_compute_processes_c",
                as.integer(idx),
                PACKAGE = "CudaMon"
            )
        )

        data.frame(
            device_index = as.integer(res[[1L]]),
            pid = as.integer(res[[2L]]),
            used_gpu_memory_bytes = as.double(res[[3L]]),
            gpu_instance_id = as.integer(res[[4L]]),
            compute_instance_id = as.integer(res[[5L]]),
            stringsAsFactors = FALSE
        )
    })

    process_df <- do.call(rbind, process_frames)
    rownames(process_df) <- NULL

    if (nrow(process_df) == 0L) {
        return(process_df)
    }

    if (!is.null(pid)) {
        if (!is.numeric(pid)) {
            stop("pid must be NULL or a numeric vector", call. = FALSE)
        }
        keep_pid <- process_df$pid %in% as.integer(pid)
        process_df <- process_df[keep_pid, , drop = FALSE]
        rownames(process_df) <- NULL
    }

    process_df
}


#' Get metrics for a device
#'
#' @param device_index Integer, 0-based GPU index
#' @return A one-row data frame with device-level metrics. Columns:
#'   `device_index`, then
#' \describe{
#'   \item{gpu_utilization_pct}{Percent of the sampling interval during which
#'     at least one kernel was executing on the GPU. This is an activity
#'     ratio, not a measure of how saturated the GPU cores were.}
#'   \item{memory_utilization_pct}{Percent of the sampling interval during
#'     which global device memory was read from or written to. This is
#'     memory-controller activity, not the fraction of GPU memory allocated.}
#'   \item{temperature_c}{GPU temperature in degrees Celsius.}
#'   \item{power_usage_mw}{Instantaneous board power draw in milliwatts.}
#'   \item{memory_used_bytes}{Bytes of device memory currently allocated.}
#'   \item{memory_total_device_bytes}{Total bytes of physical device memory
#'     available.}
#' }
#' Metrics that the driver does not support for the device are returned as
#' `NA` rather than as an error. Stops with an error if the NVML query for
#' the device fails.
#' @examples
#' if (nvml_is_available() && nvml_device_count() > 0L) {
#'     nvml_get_metrics(0L)
#' }
#' @export
nvml_get_metrics <- function(device_index) {
    if (!is.numeric(device_index) || length(device_index) != 1L) {
        stop("device_index must be a single numeric value", call. = FALSE)
    }
    idx <- as.integer(device_index)

    res <- fault_check(.Call("nvml_get_metrics_c", idx, PACKAGE = "CudaMon"))

    data.frame(
        device_index = idx,
        gpu_utilization_pct = as.integer(res[[1L]]),
        memory_utilization_pct = as.integer(res[[2L]]),
        temperature_c = as.integer(res[[3L]]),
        power_usage_mw = as.integer(res[[4L]]),
        memory_used_bytes = as.double(res[[5L]]),
        memory_total_device_bytes = as.double(res[[6L]]),
        stringsAsFactors = FALSE
    )
}
