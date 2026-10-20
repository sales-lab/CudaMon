#' Reshape sampled GPU metrics for visualization
#'
#' @param x A `CudaMonSession` object returned by `cm_parser()`.
#' @param tz Time zone used to parse timestamps.
#' @param device_index Optional integer GPU index. If `NULL`, include all
#'   sampled devices.
#' @return A long-format data frame suitable for plotting.
#' @examples
#' device_metrics <- data.frame(
#'   timestamp = format(Sys.time() + 0:1, tz = "UTC", usetz = TRUE),
#'   sampler_pid = Sys.getpid(),
#'   device_index = 0L,
#'   gpu_utilization_pct = c(10L, 25L),
#'   memory_utilization_pct = c(5L, 12L),
#'   temperature_c = c(40L, 42L),
#'   power_usage_mw = c(50000L, 53000L),
#'   memory_used_bytes = c(1e9, 1.2e9),
#'   memory_total_device_bytes = 8e9
#' )
#' session <- CudaMonSession(device_metrics = device_metrics)
#' cm_vizdf(session)
#' @export
cm_vizdf <- function(x, tz = "UTC", device_index = NULL) {
  if (!inherits(x, "CudaMonSession")) {
    stop("x must inherit from 'CudaMonSession'", call. = FALSE)
  }

  df <- x$device_metrics
  if (!is.data.frame(df) || nrow(df) == 0L) {
    return(data.frame(
      tm = as.POSIXct(character(), tz = tz),
      xtype = character(),
      pos = character(),
      value = double(),
      type = character(),
      device_index = integer(),
      stringsAsFactors = FALSE
    ))
  }

  if (!is.null(device_index)) {
    df <- df[df$device_index %in% as.integer(device_index), , drop = FALSE]
  }

  if (nrow(df) == 0L) {
    return(data.frame(
      tm = as.POSIXct(character(), tz = tz),
      xtype = character(),
      pos = character(),
      value = double(),
      type = character(),
      device_index = integer(),
      stringsAsFactors = FALSE
    ))
  }

  tm <- as.POSIXct(df$timestamp, tz = "UTC")

  plot_df <- rbind(
    data.frame(
      tm = tm,
      xtype = "GPU_MEM",
      pos = "top",
      value = as.double(df$gpu_utilization_pct),
      type = "GPU memory activity (%)",
      device_index = as.integer(df$device_index),
      stringsAsFactors = FALSE
    ),
    data.frame(
      tm = tm,
      xtype = "GPU_MEM",
      pos = "bot",
      value = as.double(df$memory_used_bytes * 1e-9),
      type = "GPU memory used (Gb)",
      device_index = as.integer(df$device_index),
      stringsAsFactors = FALSE
    ),
    data.frame(
      tm = tm,
      xtype = "THERM_PWR",
      pos = "top",
      value = as.double(df$temperature_c),
      type = "Temperature (C)",
      device_index = as.integer(df$device_index),
      stringsAsFactors = FALSE
    ),
    data.frame(
      tm = tm,
      xtype = "THERM_PWR",
      pos = "bot",
      value = as.double(df$power_usage_mw * 1e-3),
      type = "Power (W)",
      device_index = as.integer(df$device_index),
      stringsAsFactors = FALSE
    )
  )
  has_value <- stats::ave(
    !is.na(plot_df$value),
    plot_df$type,
    FUN = any
  )
  plot_df[has_value, , drop = FALSE]
}

#' Plot sampled GPU usage over time
#'
#' @param x A `CudaMonSession` object returned by `cm_parser()`.
#' @param tz Time zone used to parse timestamps.
#' @param device_index Optional integer GPU index. If `NULL`, include all
#'   sampled devices.
#' @param show_points Logical. Show sampled points.
#' @param show_lines Logical. Show continuous lines.
#' @param show_events Logical. Show event markers.
#' @param event_labels One of `"inside"`, `"outside"`, or `"none"`.
#' @param event_color_by_step Logical. Color event markers by step.
#' @return A ggplot object with one facet per metric.
#' @examples
#' device_metrics <- data.frame(
#'   timestamp = format(Sys.time() + 0:1, tz = "UTC", usetz = TRUE),
#'   sampler_pid = Sys.getpid(),
#'   device_index = 0L,
#'   gpu_utilization_pct = c(10L, 25L),
#'   memory_utilization_pct = c(5L, 12L),
#'   temperature_c = c(40L, 42L),
#'   power_usage_mw = c(50000L, 53000L),
#'   memory_used_bytes = c(1e9, 1.2e9),
#'   memory_total_device_bytes = 8e9
#' )
#' session <- CudaMonSession(device_metrics = device_metrics)
#' cm_plot_usage(session)
#' @export
cm_plot_usage <- function(
    x,
    tz = "UTC",
    device_index = NULL,
    show_points = TRUE,
    show_lines = TRUE,
    show_events = TRUE,
    event_labels = c("none", "outside", "inside"),
    event_color_by_step = TRUE
) {
  event_labels <- match.arg(event_labels)

  plot_df <- cm_vizdf(x, tz = tz, device_index = device_index)

  has_device_index <- "device_index" %in% names(plot_df)

  if (has_device_index) {
    base_aes <- ggplot2::aes(
      x = .data$tm,
      y = .data$value,
      color = factor(.data$device_index)
    )
  } else {
    base_aes <- ggplot2::aes(
      x = .data$tm,
      y = .data$value
    )
  }

  p <- ggplot2::ggplot(plot_df, base_aes)

  if (show_lines) {
    if (has_device_index) {
      p <- p +
        ggplot2::geom_line(
          ggplot2::aes(group = interaction(.data$type, .data$device_index)),
          linewidth = 0.6,
          alpha = 0.85,
          na.rm = TRUE
        )
    } else {
      p <- p +
        ggplot2::geom_line(
          linewidth = 0.6,
          alpha = 0.85,
          na.rm = TRUE
        )
    }
  }

  if (show_points) {
    p <- p +
      ggplot2::geom_point(
        size = 1.2,
        alpha = 0.7,
        na.rm = TRUE
      )
  }

  p <- p +
    ggplot2::facet_grid(
      ggplot2::vars(.data$type),
      scales = "free_y"
    ) +
    ggplot2::scale_x_datetime(
      timezone = tz,
      date_labels = "%H:%M:%S"
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      color = "GPU"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_text(angle = 0),
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
    )

  events_df <- x$events

  has_events <- is.data.frame(events_df) &&
    nrow(events_df) > 0L &&
    all(c("timestamp", "step") %in% names(events_df))

  if (show_events && has_events) {
    events_df <- events_df[, c("timestamp", "step"), drop = FALSE]
    events_df$tm <- as.POSIXct(events_df$timestamp, tz = "UTC")
    events_df$step <- factor(events_df$step)

    if (event_color_by_step) {
      vline_aes <- ggplot2::aes(
        xintercept = .data$tm,
        color = .data$step
      )

      label_aes <- ggplot2::aes(
        x = .data$tm,
        y = Inf,
        label = .data$step,
        color = .data$step
      )
    } else {
      vline_aes <- ggplot2::aes(xintercept = .data$tm)

      label_aes <- ggplot2::aes(
        x = .data$tm,
        y = Inf,
        label = .data$step
      )
    }

    p <- p +
      ggplot2::geom_vline(
        data = events_df,
        mapping = vline_aes,
        inherit.aes = FALSE,
        linetype = "dashed",
        linewidth = 0.35,
        alpha = 0.75
      )

    if (event_labels == "inside") {
      p <- p +
        ggplot2::geom_text(
          data = events_df,
          mapping = label_aes,
          inherit.aes = FALSE,
          angle = 90,
          vjust = 1.2,
          hjust = 1,
          size = 3,
          alpha = 0.9
        )
    }

    if (event_labels == "outside") {
      p <- p +
        ggplot2::geom_label(
          data = events_df,
          mapping = label_aes,
          inherit.aes = FALSE,
          angle = 90,
          vjust = 1.1,
          hjust = 1,
          size = 2.8,
          label.size = 0.2,
          fill = "white",
          alpha = 0.9
        ) +
        ggplot2::coord_cartesian(clip = "off") +
        ggplot2::theme(
          plot.margin = ggplot2::margin(5.5, 35, 5.5, 5.5)
        )
    }

    if (!event_color_by_step) {
      p <- p +
        ggplot2::guides(color = "none")
    }
  }

  p
}
