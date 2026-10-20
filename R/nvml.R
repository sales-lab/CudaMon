# Copyright (c) 2025 Mohammad Amin Zadenoori
# Copyright (c) 2025 Gabriele Sales
#


#' Convert an NVML error code to an R error
#'
#' @param code Integer NVML return code (0 == success)
#' @return Invisible TRUE on success; otherwise stops with an error
#' @noRd
nvml_check_status <- function(code) {
    if (!is.numeric(code) && !is.integer(code)) {
        stop("NVML status code is not numeric", call. = FALSE)
    }

    code <- as.integer(code)
    nvml_code <- if (code < 0L) abs(code) else code

    if (nvml_code == 0L) {
        return(invisible(TRUE))
    }

    msg <- .Call("nvml_error_string_c", nvml_code, PACKAGE = "CudaMon")
    stop(sprintf("NVML failure: %s (code: %d)", msg, nvml_code), call. = FALSE)
}


#' Number of NVML‑visible devices
#' @noRd
nvml_device_count <- function() {
    cnt <- .Call("nvml_device_count_c", PACKAGE = "CudaMon")
    if (cnt < 0L) {
        nvml_check_status(cnt)
    }
    as.integer(cnt)
}

#' Check whether NVML is available
#'
#' @return `TRUE` if NVML can be initialised and queried, otherwise `FALSE`
#' @noRd
nvml_is_available <- function() {
    isTRUE(.Call("nvml_is_available_c", PACKAGE = "CudaMon"))
}

#' List NVML-visible devices
#'
#' @return A data frame with one row per GPU and stable device metadata
#' @noRd
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
        res <- .Call("nvml_device_info_c", idx, PACKAGE = "CudaMon")
        if (is.integer(res) && length(res) == 1L && res < 0L) {
            nvml_check_status(res)
        }

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
#' @param device_index Optional integer GPU index. If `NULL`, query all devices.
#' @param pid Optional integer vector used to filter the returned processes.
#' @return A data frame with one row per GPU process observation
#' @noRd
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
        res <- .Call(
            "nvml_device_compute_processes_c",
            as.integer(idx),
            PACKAGE = "CudaMon"
        )
        if (is.integer(res) && length(res) == 1L && res < 0L) {
            nvml_check_status(res)
        }

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
#' @param device_index Integer, 0‑based GPU index
#' @return A named list with device-level metrics:
#' \describe{
#'   \item{gpu_utilization_pct}{Percent of the sampling interval during which
#'     at least one kernel was executing on the GPU. This is an activity ratio,
#'     not a measure of how saturated the GPU cores were.}
#'   \item{memory_utilization_pct}{Percent of the sampling interval during
#'     which global device memory was being read from or written to. This is
#'     memory-controller activity, not the fraction of GPU memory allocated.}
#'   \item{temperature_c}{GPU temperature in degrees Celsius.}
#'   \item{power_usage_mw}{Instantaneous board power draw in milliwatts.}
#'   \item{memory_used_bytes}{Bytes of device memory currently allocated.}
#'   \item{memory_total_device_bytes}{Total bytes of physical device memory
#'     available.}
#' }
#' @noRd
nvml_get_metrics <- function(device_index) {
    if (!is.numeric(device_index) || length(device_index) != 1L) {
        stop("device_index must be a single numeric value", call. = FALSE)
    }
    idx <- as.integer(device_index)

    res <- .Call("nvml_get_metrics_c", idx, PACKAGE = "CudaMon")
    if (is.integer(res) && length(res) == 1L && res < 0L) {
        nvml_check_status(res)
    }

    stats::setNames(
        list(
            as.integer(res[[1L]]),
            as.integer(res[[2L]]),
            as.integer(res[[3L]]),
            as.integer(res[[4L]]),
            as.double(res[[5L]]),
            as.double(res[[6L]])
        ),
        c(
            "gpu_utilization_pct",
            "memory_utilization_pct",
            "temperature_c",
            "power_usage_mw",
            "memory_used_bytes",
            "memory_total_device_bytes"
        )
    )
}
