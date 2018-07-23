#' CreateTask
#'
#' Create a long-running task, on Linux executed in a forked process, on Windows in the same
#' session.\cr
#'
#' The return value is a promise-like object with three methods:\cr
#' - completed(): FALSE initially, then TRUE if the task succeeds, fails, or is cancelled. Reactive,
#'   so when the state changes any reactive readers will invalidate.\cr
#' - result(): Use this to get the return value. While execution is in progress, performs a
#'   req(FALSE). If task succeeded, returns the return value. If failed, throws error. Reactive, so
#'   when the state changes any reactive readers will invalidate.\cr
#' - cancel(): Call this to prematurely terminate the task.
#'
#' @param expr Expression to be executed. Required.
#' @param args List object with arguments to be passed to \code{expr}. Optional.
#'   Default = \code{NULL}
#' @param timeout Number of milliseconds between consecutive checks of the task. Optional.
#'   Default = 1000L
#'
#' @return Closure
#'
#' @examples
#' \dontrun{
#' task <- CreateTask({
#'   Sys.sleep(5)
#'   cars[sample(nrow(cars), 10),]
#' })
#' shiny::isolate(task$result())
#' }
#'
#' @export
CreateTask <- function(expr, args = NULL, timeout = 1000L)
{
  shiny::makeReactiveBinding("state")
  state <- "running"

  result <- NULL

  sysName <- tolower(Sys.info()[["sysname"]])
  switch(
    sysName,
    "linux" = {
      rmChild <- get("rmChild", envir = asNamespace("parallel"))

      # Launch the task in a forked process. This always returns
      # immediately, and we get back a handle we can use to monitor
      # or kill the job.
      task_handle <- parallel::mcparallel({
        force(expr)
      })

      # Poll every [timeout] milliseconds until the job completes
      o <- observe({
        res <- parallel::mccollect(task_handle, wait = FALSE)
        if (is.null(res)) {
          shiny::invalidateLater(timeout)
        } else {
          o$destroy()
          if (!is.list(res) || length(res) != 1 || !inherits(res[[1]], "try-error")) {
            state <<- "success"
            result <<- res[[1]]
          } else {
            state <<- "error"
            result <<- attr(res[[1]], "condition", exact = TRUE)
          }
          rmChild(task_handle)
        }
      })

      return(
        list(
          completed = function() {
            state != "running"
          },
          result = function() {
            if (state == "running") {
              shiny::req(FALSE)
            } else if (state == "success") {
              return(result)
            } else if (state == "cancel") {
              return(state)
            } else if (state == "error") {
              stop(result)
            }
          },
          cancel = function() {
            if (state == "running") {
              state <<- "cancel"
              o$destroy()
              tools::pskill(task_handle$pid, tools::SIGTERM)
              tools::pskill(-task_handle$pid, tools::SIGTERM)
              parallel::mccollect(task_handle, wait = FALSE)
              rmChild(task_handle)
            }
          }
        )
      )
    },
    "windows" = {
      # Launch the task in a forked process. This always returns
      # immediately, and we get back a handle we can use to monitor
      # or kill the job.
      task_handle <- callr::r_bg({
        force(expr)
      }, args = args)

      # Poll every [timeout] milliseconds until the job completes
      o <- observe({
        isRunning <- task_handle$is_alive()
        if (isRunning) {
          shiny::invalidateLater(timeout)
        } else {
          res <- try({task_handle$get_result()})
          o$destroy()
          if (!inherits(res, "try-error")) {
            state <<- "success"
            result <<- res
          } else {
            state <<- "error"
            result <<- NULL
          }
          task_handle$kill()
        }
      })

      return(
        list(
          completed = function() {
            state != "running"
          },
          result = function() {
            if (state == "running") {
              shiny::req(FALSE)
            } else if (state == "success") {
              return(result)
            } else if (state == "cancel") {
              return(state)
            } else if (state == "error") {
              stop(result)
            }
          },
          cancel = function() {
            if (state == "running") {
              state <<- "cancel"
              o$destroy()
              task_handle$kill()
            }
          }
        )
      )
    },
  )
}
