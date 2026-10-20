args <- commandArgs(trailingOnly = TRUE)
device_metrics_path <- args[[1]]
compute_processes_path <- args[[2]]
witness_path <- args[[3]]
period <- as.numeric(args[[4]])
pid <- as.integer(args[[5]])
include_descendants <- identical(args[[6]], "true")
device_index <- if (length(args) >= 7 && nzchar(args[[7]])) {
    as.integer(strsplit(args[[7]], ",", fixed = TRUE)[[1]])
} else {
    NULL
}

suppressPackageStartupMessages(library(CudaMon))

writeLines("", witness_path)
on.exit(unlink(witness_path), add = TRUE)

while (TRUE) {
    sample_time <- format(Sys.time(), tz = "UTC", usetz = TRUE)
    indices <- if (is.null(device_index)) {
        seq_len(CudaMon:::nvml_device_count()) - 1L
    } else {
        device_index
    }

    if (length(indices) > 0L) {
        device_rows <- do.call(
            rbind,
            lapply(indices, function(idx) {
                metrics <- CudaMon:::nvml_get_metrics(idx)
                data.frame(
                    timestamp = sample_time,
                    sampler_pid = pid,
                    device_index = as.integer(idx),
                    gpu_utilization_pct = as.integer(
                        metrics$gpu_utilization_pct
                    ),
                    memory_utilization_pct = as.integer(
                        metrics$memory_utilization_pct
                    ),
                    temperature_c = as.integer(metrics$temperature_c),
                    power_usage_mw = as.integer(metrics$power_usage_mw),
                    memory_used_bytes = as.double(metrics$memory_used_bytes),
                    memory_total_device_bytes = as.double(
                        metrics$memory_total_device_bytes
                    ),
                    stringsAsFactors = FALSE
                )
            })
        )
        utils::write.table(
            device_rows,
            file = device_metrics_path,
            sep = ",",
            row.names = FALSE,
            col.names = !file.exists(device_metrics_path),
            append = file.exists(device_metrics_path),
            qmethod = "double"
        )
    }

    tracked_pids <- CudaMon:::process_pids(
        pid = pid,
        include_descendants = include_descendants
    )
    process_rows <- CudaMon:::nvml_list_compute_processes(
        device_index = device_index,
        pid = tracked_pids
    )

    if (nrow(process_rows) > 0L) {
        process_rows$tracked_pid <- as.integer(process_rows$pid)
        process_rows$is_root_pid <- process_rows$tracked_pid == pid
        process_rows$timestamp <- sample_time
        process_rows$sampler_pid <- pid
        process_rows <- process_rows[, c(
            "timestamp",
            "sampler_pid",
            "device_index",
            "pid",
            "tracked_pid",
            "is_root_pid",
            "used_gpu_memory_bytes",
            "gpu_instance_id",
            "compute_instance_id"
        )]
        utils::write.table(
            process_rows,
            file = compute_processes_path,
            sep = ",",
            row.names = FALSE,
            col.names = !file.exists(compute_processes_path),
            append = file.exists(compute_processes_path),
            qmethod = "double"
        )
    }

    Sys.sleep(period)
}
