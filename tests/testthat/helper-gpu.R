has_cudamon_native_symbol <- function(name) {
  tryCatch(
    {
      getNativeSymbolInfo(name, PACKAGE = "CudaMon")
      TRUE
    },
    error = function(...) FALSE
  )
}

skip_if_no_nvidia_gpu <- function() {
  testthat::skip_if_not(
    has_cudamon_native_symbol("nvml_is_available_c"),
    message = "CudaMon native NVML symbols are not available"
  )

  testthat::skip_if_not(
    has_cudamon_native_symbol("nvml_device_count_c"),
    message = "CudaMon native NVML symbols are not available"
  )

  testthat::skip_if_not(
    CudaMon::nvml_is_available(),
    message = "NVML is not available on this machine"
  )

  testthat::skip_if_not(
    CudaMon::nvml_device_count() > 0L,
    message = "No NVIDIA GPU detected"
  )
}
