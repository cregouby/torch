#' Returns a bool indicating if CUDA is currently available.
#'
#' @export
cuda_is_available <- function() {
  cpp_cuda_is_available()
}

#' Returns the index of a currently selected device.
#'
#' @export
cuda_current_device <- function() {
  cpp_cuda_current_device()
}

#' Returns the number of GPUs available.
#'
#' @export
cuda_device_count <- function() {
  cpp_cuda_device_count()
}

#' Waits for all kernels in all streams on a CUDA device to complete.
#'
#' @param device device for which to synchronize. It uses the current device
#'  given by [cuda_current_device()] if no device is specified.
#'
#' @export
cuda_synchronize <- function(device = NULL) {
  if (is.null(device)) {
    device <- -1L
  }
  cpp_cuda_synchronize(device)
}

#' Returns the major and minor CUDA capability of `device`
#'
#' @param device Integer value of the CUDA device to return capabilities of.
#'
#' @export
cuda_get_device_capability <- function(device = cuda_current_device()) {
  if (device < 0 | device >= cuda_device_count()) {
    value_error("device must be an integer between 0 and the number of devices minus 1")
  }
  res <- as.integer(cpp_cuda_get_device_capability(device))
  names(res) <- c("Major", "Minor")
  res
}

paste_for_each <- function(x, y, ...) {
  unlist(lapply(x, function(l) paste(l, y, ...)))
}

#' Returns a dictionary of CUDA memory allocator statistics for a given device.
#'
#' The return value of this function is a dictionary of statistics, each of which
#' is a non-negative integer.
#'
#' @inheritParams cuda_get_device_capability
#' @section Core statistics:
#'
#' - "allocated.\{all,large_pool,small_pool\}.\{current,peak,allocated,freed\}": number of allocation requests received by the memory allocator.
#' - "allocated_bytes.\{all,large_pool,small_pool\}.\{current,peak,allocated,freed\}": amount of allocated memory.
#' - "segment.\{all,large_pool,small_pool\}.\{current,peak,allocated,freed\}": number of reserved segments from cudaMalloc().
#' - "reserved_bytes.\{all,large_pool,small_pool\}.\{current,peak,allocated,freed\}": amount of reserved memory.
#' - "active.\{all,large_pool,small_pool\}.\{current,peak,allocated,freed\}": number of active memory blocks.
#' - "active_bytes.\{all,large_pool,small_pool\}.\{current,peak,allocated,freed\}": amount of active memory.
#' - "inactive_split.\{all,large_pool,small_pool\}.\{current,peak,allocated,freed\}": number of inactive, non-releasable memory blocks.
#' - "inactive_split_bytes.\{all,large_pool,small_pool\}.\{current,peak,allocated,freed\}": amount of inactive, non-releasable memory.
#'
#' For these core statistics, values are broken down as follows.
#'
#' Pool type:
#'
#' - all: combined statistics across all memory pools.
#' - large_pool: statistics for the large allocation pool (as of October 2019, for size >= 1MB allocations).
#' - small_pool: statistics for the small allocation pool (as of October 2019, for size < 1MB allocations).
#'
#' Metric type:
#'
#' - current: current value of this metric.
#' - peak: maximum value of this metric.
#' - allocated: historical total increase in this metric.
#' - freed: historical total decrease in this metric.
#'
#' @section Additional metrics:
#' - "num_alloc_retries": number of failed cudaMalloc calls that result in a cache flush and retry.
#' - "num_ooms": number of out-of-memory errors thrown.
#'
#' @export
cuda_memory_stats <- function(device = cuda_current_device()) {
  if (!cuda_is_available()) {
    rlang::abort("CUDA is not available.")
  }

  # quickly allocate some memory to initialize the device
  torch_tensor(1, device = torch_device("cuda", device))

  stat <- c("current", "peak", "allocated", "freed")
  stat_type <- c("all", "small_pool", "large_pool")

  nms <- c("num_alloc_retries", "num_ooms", "max_split_size")
  nms <- c(nms, paste("oversize_allocations", stat, sep = "."))
  nms <- c(nms, paste("oversize_segments", stat, sep = "."))

  nms <- c(nms, paste_for_each(paste("allocation", stat_type, sep = "."), stat, sep = "."))
  nms <- c(nms, paste_for_each(paste("segment", stat_type, sep = "."), stat, sep = "."))
  nms <- c(nms, paste_for_each(paste("active", stat_type, sep = "."), stat, sep = "."))
  nms <- c(nms, paste_for_each(paste("inactive_split", stat_type, sep = "."), stat, sep = "."))

  nms <- c(nms, paste_for_each(paste("allocated_bytes", stat_type, sep = "."), stat, sep = "."))
  nms <- c(nms, paste_for_each(paste("reserved_bytes", stat_type, sep = "."), stat, sep = "."))
  nms <- c(nms, paste_for_each(paste("active_bytes", stat_type, sep = "."), stat, sep = "."))
  nms <- c(nms, paste_for_each(paste("inactive_split_bytes", stat_type, sep = "."), stat, sep = "."))


  values <- cpp_cuda_memory_stats(device)
  names(values) <- nms

  get_stat <- function(values, prefix) {
    out <- list()
    for (nm in stat) {
      out[[nm]] <- unname(values[paste(prefix, nm, sep = ".")])
    }
    out
  }

  get_stat_type <- function(values, prefix) {
    out <- list()
    for (nm in stat_type) {
      out[[nm]] <- get_stat(values, paste(prefix, nm, sep = "."))
    }
    out
  }

  result <- list(
    "num_alloc_retries" = unname(values["num_alloc_retries"]),
    "num_ooms" = unname(values["num_ooms"]),
    "max_split_size" = unname(values["max_split_size"]),
    "oversize_allocations" = get_stat(values, "oversize_allocations"),
    "oversize_segments" = get_stat(values, "oversize_segments"),
    "allocation" = get_stat_type(values, "allocation"),
    "segment" = get_stat_type(values, "segment"),
    "active" = get_stat_type(values, "active"),
    "inactive_split" = get_stat_type(values, "inactive_split"),
    "allocated_bytes" = get_stat_type(values, "allocated_bytes"),
    "reserved_bytes" = get_stat_type(values, "reserved_bytes"),
    "active_bytes" = get_stat_type(values, "active_bytes"),
    "inactive_split_bytes" = get_stat_type(values, "inactive_split_bytes")
  )
  class(result) <- "cuda_memory_stats"
  result
}

#' @rdname cuda_memory_stats
#' @export
cuda_memory_summary <- function(device = cuda_current_device()) {
  result <- cuda_memory_stats(device)
  print(result)
}

#' @export
print.cuda_memory_stats <- function(x, ...) {
  utils::str(x)
  invisible(x)
}

#' Returns the CUDA runtime version
#'
#' @export
cuda_runtime_version <- function() {
  v <- cpp_cuda_get_runtime_version()
  major <- trunc(v / 1000)
  minor <- trunc((v - major * 1000) / 10)
  patch <- v - major * 1000 - minor * 10
  numeric_version(paste(major, minor, patch, sep = "."))
}

#' Empty cache
#' 
#' Releases all unoccupied cached memory currently held by the caching allocator 
#' so that those can be used in other GPU application and visible in `nvidia-smi`.
#' 
#' @note [cuda_empty_cache()] doesn’t increase the amount of GPU memory available 
#' for torch. However, it may help reduce fragmentation of GPU memory in certain 
#' cases. See Memory management article for more details about GPU memory management.
#' 
#' @export
cuda_empty_cache <- function() {
  cpp_cuda_empty_cache()
}

#' Enable Recording of Memory Allocation Stack Traces
#'
#' Enables recording of stack traces associated with memory allocations, allowing
#' users to identify the source of memory allocation in CUDA snapshots.
#'
#' Alongside tracking stack traces for each current allocation and free event,
#' this function can also keep a historical log of all allocation and free events.
#'
#' Use `cuda_memory_snapshot()` to retrieve recorded information. Visualization
#' can be performed using [pytorch.org/memory_viz](https://docs.pytorch.org/memory_viz).
#'
#' @param enabled Character or \code{NULL}. Controls memory history recording. Options:
#'   \describe{
#'     \item{\code{NULL}}{Disable recording of memory history.}
#'     \item{\code{"state"}}{Record currently allocated memory information.}
#'     \item{\code{"all"}}{Record the history of all allocation and free events (default).}
#'   }
#'
#' @param context Character or \code{NULL}. Controls traceback recording. Options:
#'   \describe{
#'     \item{\code{NULL}}{Do not record any tracebacks.}
#'     \item{\code{"state"}}{Record tracebacks for currently allocated memory.}
#'     \item{\code{"alloc"}}{Record tracebacks for allocation events.}
#'     \item{\code{"all"}}{Record tracebacks for both allocation and free events (default).}
#'   }
#'
#' @param stacks Character. Defines the stack trace frames to include. Options:
#'   \describe{
#'     \item{\code{"all"}}{Include all frames (default).}
#'   }
#'
#' @param max_entries Integer. The maximum number of allocation/free events to retain.
#'
#' @return None; function invoked for side effects.
#' @examples
#' \dontrun{
#' cuda_record_memory_history(enabled = 'all', context = 'all', stacks = 'all', max_entries = 10000)
#' }
#' @export
cuda_record_memory_history <- function(enabled, context = "all", stacks = "all", max_entries = 1) {
  cpp_cuda_record_memory_history(enabled, context, stacks, as.integer(max_entries))
}

#' Capture CUDA Memory State Snapshot
#'
#' Saves a snapshot of the CUDA memory state at the time it was called. The resulting
#' binary output is in pickle format and can be visualized using the interactive snapshot
#' viewer available at [pytorch.org/memory_viz](https://docs.pytorch.org/memory_viz).
#'
#' @return Raw binary data representing the snapshot in pickle format.
#' @examples
#' \dontrun{
#' snapshot <- cuda_memory_snapshot()
#' }
#' @export
cuda_memory_snapshot <- function() {
  cpp_cuda_memory_snapshot()
}

#' Save CUDA Memory State Snapshot to File
#'
#' Calls `cuda_memory_snapshot()` and saves the resulting binary snapshot
#' to a specified file using `writeBin`. The resulting file can be visualized using the interactive 
#' snapshot viewer available at [pytorch.org/memory_viz](https://docs.pytorch.org/memory_viz).
#'
#' @param filepath Character; the path to the file where the snapshot will be saved.
#'
#' @return None; snapshot is saved directly to the file.
#' @examples
#' \dontrun{
#' cuda_dump_memory_snapshot("snapshot.bin")
#' }
#' @export
cuda_dump_memory_snapshot <- function(filepath) {
  snapshot <- cuda_memory_snapshot()
  con <- file(filepath, "wb")
  on.exit(close(con))
  writeBin(snapshot, con)
}