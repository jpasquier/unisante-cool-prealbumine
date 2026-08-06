#' Preview a plot via a temporary SVG
#'
#' Opens a grDevices::svg() device, evaluates `expr`,
#' shows the file in a Wayland-native viewer, then deletes it when
#' the viewer is closed.
#'
#' @param expr  an expression that draws the plot
#' @param width,height  device size in inches
#' @param viewer  command used to open the SVG; default is "eog"
#'
#' @examples
#' preview_svg({
#'   plot(pressure)          # any plotting code here
#' })
preview_svg <- function(expr,
                        width  = 7,
                        height = 7,
                        viewer = Sys.getenv("R_SVG_VIEWER", "eog"))
{
  stopifnot(is.expression(substitute(expr)) || is.call(substitute(expr)))

  tmp <- tempfile(fileext = ".svg")

  # draw into the temporary SVG
  grDevices::svg(tmp, width = width, height = height)
  tryCatch(
    {
      eval.parent(substitute(expr))   # run the plotting code
      grDevices::dev.off()            # flush & close device
    },
    error = function(e) {
      grDevices::dev.off()
      unlink(tmp)
      stop(e)
    }
  )

  # launch the viewer and *wait* until the window is closed
  # eog (Eye of GNOME) keeps its PID alive for the lifetime of the window,
  # so `wait = TRUE` works reliably.
  #
  # You can export R_SVG_VIEWER="imv" or any other program that
  # behaves the same way.
  #
  # If the command is not found, fall back to xdg-open
  status <- tryCatch(
    system2(viewer, tmp, wait = TRUE),
    error = function(e) system2("xdg-open", tmp, wait = TRUE)
  )

  unlink(tmp, force = TRUE)           # clean up the file
  invisible(status)
}
