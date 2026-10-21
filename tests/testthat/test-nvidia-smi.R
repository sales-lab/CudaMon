skip_if_no_nvidia_smi <- function() {
  testthat::skip_if_not(
    nzchar(Sys.which("nvidia-smi")),
    message = "nvidia-smi is not available"
  )
}

nvidia_smi_query <- function(query_type, fields) {
  args <- c(
    sprintf("--query-%s=%s", query_type, paste(fields, collapse = ",")),
    "--format=csv,noheader,nounits"
  )

  output <- tryCatch(
    system2("nvidia-smi", args, stdout = TRUE, stderr = TRUE),
    error = function(e) structure(conditionMessage(e), status = 1L)
  )

  status <- attr(output, "status")
  testthat::skip_if_not(
    is.null(status),
    message = "nvidia-smi query failed"
  )

  lines <- trimws(as.character(output))
  lines <- lines[nzchar(lines)]

  if (length(lines) == 1L && grepl("^No running ", lines[[1L]])) {
    lines <- character()
  }

  if (length(lines) == 0L) {
    out <- as.data.frame(
      setNames(
        replicate(length(fields), character(0), simplify = FALSE),
        fields
      )
    )
    return(out)
  }

  parsed <- utils::read.csv(
    text = paste(lines, collapse = "\n"),
    header = FALSE,
    strip.white = TRUE
  )

  names(parsed) <- fields
  parsed
}

normalize_device_names <- function(x) {
  trimws(as.character(x))
}

test_that("nvml_is_available is consistent with nvidia-smi availability", {
  testthat::skip_if_not(
    has_cudamon_native_symbol("nvml_is_available_c"),
    message = "CudaMon native NVML symbols are not available"
  )
  skip_if_no_nvidia_smi()

  smi_devices <- nvidia_smi_query("gpu", c("index"))

  expect_true(CudaMon::nvml_is_available())
  expect_gte(nrow(smi_devices), 1L)
})

test_that("nvml_device_count matches nvidia-smi", {
  skip_if_no_nvidia_gpu()
  skip_if_no_nvidia_smi()

  smi_devices <- nvidia_smi_query("gpu", c("index"))

  expect_equal(CudaMon::nvml_device_count(), nrow(smi_devices))
})

test_that("nvml_list_devices matches nvidia-smi device metadata", {
  skip_if_no_nvidia_gpu()
  skip_if_no_nvidia_smi()

  nvml_devices <- CudaMon::nvml_list_devices()
  smi_devices <- nvidia_smi_query("gpu", c("index", "name", "uuid"))

  smi_devices$index <- as.integer(smi_devices$index)
  smi_devices$name <- normalize_device_names(smi_devices$name)
  smi_devices$uuid <- trimws(smi_devices$uuid)

  nvml_devices <- nvml_devices[order(nvml_devices$device_index), , drop = FALSE]
  smi_devices <- smi_devices[order(smi_devices$index), , drop = FALSE]

  expect_equal(nrow(nvml_devices), nrow(smi_devices))
  expect_equal(nvml_devices$device_index, smi_devices$index)
  expect_equal(normalize_device_names(nvml_devices$name), smi_devices$name)
  expect_equal(trimws(nvml_devices$uuid), smi_devices$uuid)
})

test_that("nvml_list_compute_processes matches nvidia-smi compute processes", {
  skip_if_no_nvidia_gpu()
  skip_if_no_nvidia_smi()

  nvml_devices <- CudaMon::nvml_list_devices()
  nvml_processes <- CudaMon::nvml_list_compute_processes()
  smi_processes <- nvidia_smi_query("compute-apps", c("gpu_uuid", "pid", "used_memory"))

  if (nrow(smi_processes) == 0L) {
    expect_equal(nrow(nvml_processes), 0L)
    return()
  }

  smi_processes$gpu_uuid <- trimws(smi_processes$gpu_uuid)
  smi_processes$pid <- as.integer(smi_processes$pid)
  smi_processes$used_memory <- as.double(smi_processes$used_memory) * 1024^2

  smi_processes$device_index <- nvml_devices$device_index[
    match(smi_processes$gpu_uuid, trimws(nvml_devices$uuid))
  ]

  smi_processes <- smi_processes[
    !is.na(smi_processes$device_index),
    c("device_index", "pid", "used_memory"),
    drop = FALSE
  ]

  nvml_subset <- nvml_processes[, c("device_index", "pid", "used_gpu_memory_bytes"), drop = FALSE]

  nvml_subset <- nvml_subset[order(nvml_subset$device_index, nvml_subset$pid), , drop = FALSE]
  smi_processes <- smi_processes[order(smi_processes$device_index, smi_processes$pid), , drop = FALSE]

  expect_equal(nrow(nvml_subset), nrow(smi_processes))
  expect_equal(nvml_subset$device_index, smi_processes$device_index)
  expect_equal(nvml_subset$pid, smi_processes$pid)
  expect_true(all(abs(nvml_subset$used_gpu_memory_bytes - smi_processes$used_memory) <= 1024^2))
})
