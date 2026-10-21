test_that("the package can detect whether NVIDIA GPUs are available", {
  testthat::skip_if_not(
    has_cudamon_native_symbol("nvml_is_available_c"),
    message = "CudaMon native NVML symbols are not available"
  )

  available <- CudaMon::nvml_is_available()

  expect_type(available, "logical")
  expect_length(available, 1)
})

test_that("the package can report how many NVIDIA GPUs are visible", {
  skip_if_no_nvidia_gpu()

  count <- CudaMon::nvml_device_count()
  devices <- CudaMon::nvml_list_devices()

  expect_type(count, "integer")
  expect_gte(count, 1L)
  expect_equal(nrow(devices), count)
  expect_true(all(c("device_index", "name", "uuid") %in% names(devices)))
})

test_that("the sampler can monitor a short sleep and build a plot", {
  skip_if_no_nvidia_gpu()

  prefix <- tempfile("cudamon-")
  plot_path <- paste0(prefix, "_usage.png")
  stopped <- FALSE

  sampler <- CudaMon::cm_start(
    path_prefix = prefix,
    period = 0.2,
    pid = Sys.getpid(),
    include_descendants = TRUE
  )

  CudaMon::cm_timestamp(sampler, "before_sleep")
  Sys.sleep(1)
  CudaMon::cm_timestamp(sampler, "after_sleep")
  CudaMon::cm_stop(sampler)
  stopped <- TRUE

  session <- CudaMon::cm_parser(sampler)
  plot_obj <- CudaMon::cm_plot_usage(session)
  ggplot2::ggsave(plot_path, plot_obj, width = 8, height = 5, dpi = 120)

  expect_s3_class(session, "CudaMonSession")
  expect_gt(nrow(session$device_metrics), 0)
  expect_s3_class(plot_obj, "ggplot")
  expect_true(file.exists(plot_path))
})
