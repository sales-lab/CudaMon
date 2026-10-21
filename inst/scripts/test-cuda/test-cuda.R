test_that("CudaMon monitors GPU activity during compiled CUDA matrix multiplication", {
  skip_if_no_nvidia_gpu()

  build <- compile_manual_cuda_library()

  dll <- tryCatch(
    dyn.load(build$so_path),
    error = function(...) NULL
  )
  testthat::skip_if_not(!is.null(dll), message = "Compiled CUDA library could not be loaded")
  on.exit(dyn.unload(build$so_path), add = TRUE)

  prefix <- tempfile("cudamon-cuda-")
  plot_path <- paste0(prefix, "_usage.png")
  stopped <- FALSE

  sampler <- CudaMon::cm_start(
    path_prefix = prefix,
    period = 0.2,
    pid = Sys.getpid(),
    include_descendants = TRUE
  )

  symbol <- getNativeSymbolInfo("cuda_matrix_multiply", dll)

  CudaMon::cm_timestamp(sampler, "cuda_matmul_start")
  result <- .Call(symbol, as.integer(10))
  CudaMon::cm_timestamp(sampler, "cuda_matmul_end")
  CudaMon::cm_stop(sampler)
  stopped <- TRUE

  session <- CudaMon::cm_parser(sampler)
  plot_obj <- CudaMon::cm_plot_usage(session)
  ggplot2::ggsave(plot_path, plot_obj, width = 8, height = 5, dpi = 120)

  expect_type(result, "double")
  expect_named(result, "total_time_ms")
  expect_true(is.finite(unname(result[[1]])))
  expect_gt(unname(result[[1]]), 0)
  expect_gt(nrow(session$device_metrics), 0)
  expect_s3_class(plot_obj, "ggplot")
  expect_true(file.exists(plot_path))
})
