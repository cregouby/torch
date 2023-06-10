branch <- "main"
<<<<<<< HEAD

install_config <- list(
  "1.12.1" = list(
    "cpu" = list(
      "darwin" = list(
        x86_64 = list(
          "libtorch" = list(
            url = "https://download.pytorch.org/libtorch/cpu/libtorch-macos-1.12.1.zip",
            path = "libtorch/",
            filter = ".dylib",
            md5hash = "bc05ee56d9134e5a36b97b949adf956a"
          ),
          "liblantern" = sprintf("https://storage.googleapis.com/torch-lantern-builds/refs/heads/%s/latest/macOS-cpu.zip", branch)  
        ),
        aarch64 = list(
          libtorch = list(
            url = "https://github.com/mlverse/libtorch-mac-m1/releases/download/LibTorch-for-R/libtorch-v1.12.1.zip",
            path = "libtorch/",
            filter = ".dylib",
            md5hash = "90be2e718f10bf72280d595cbf5f70ef"
          ),
          "liblantern" = sprintf("https://storage.googleapis.com/torch-lantern-builds/refs/heads/%s/latest/macOSArm64-cpu.zip", branch)  
        )
      ),
      "windows" = list(
        "libtorch" = list(
          url = "https://download.pytorch.org/libtorch/cpu/libtorch-win-shared-with-deps-1.12.1%2Bcpu.zip",
          path = "libtorch/",
          filter = ".dll",
          md5hash = "18f46c55560cefe628f8c57b324a0a5c"
        ),
        "liblantern" = sprintf("https://storage.googleapis.com/torch-lantern-builds/refs/heads/%s/latest/Windows-cpu.zip", branch)
      ),
      "linux" = list(
        "libtorch" = list(
          path = "libtorch/",
          url = "https://download.pytorch.org/libtorch/cpu/libtorch-cxx11-abi-shared-with-deps-1.12.1%2Bcpu.zip",
          md5hash = "cfbc46b318c1e94efa359a98d4047ec8"
        ),
        "liblantern" = sprintf("https://storage.googleapis.com/torch-lantern-builds/refs/heads/%s/latest/Linux-cpu.zip", branch)
      )
    ),
    "CUDA 10.2" = list(
      "linux" = list(
        "libtorch" = list(
          path = "libtorch/",
          url = "https://download.pytorch.org/libtorch/cu102/libtorch-cxx11-abi-shared-with-deps-1.12.1%2Bcu102.zip",
          md5hash = "5d57d7bf0e6344c17a1709c78fc59ad9"
        ),
        "liblantern" = sprintf("https://storage.googleapis.com/torch-lantern-builds/refs/heads/%s/latest/Linux-gpu-102.zip", branch)
      )
    ),
    "CUDA 11.3" = list(
      "linux" = list(
        "libtorch" = list(
          path = "libtorch/",
          url = "https://download.pytorch.org/libtorch/cu113/libtorch-cxx11-abi-shared-with-deps-1.12.1%2Bcu113.zip",
          md5hash = "2bdfda891189d93164ff6bb7a0171ffc"
        ),
        "liblantern" = sprintf("https://storage.googleapis.com/torch-lantern-builds/refs/heads/%s/latest/Linux-gpu-113.zip", branch)
      ),
      "windows" = list(
        "libtorch" = list(
          path = "libtorch/",
          url = "https://download.pytorch.org/libtorch/cu113/libtorch-win-shared-with-deps-1.12.1%2Bcu113.zip",
          filter = ".dll",
          md5hash = "a1c302dd40e11f6db269445ec049e5f5"
        ),
        "liblantern" = sprintf("https://storage.googleapis.com/torch-lantern-builds/refs/heads/%s/latest/Windows-gpu-113.zip", branch)
      )
    ),
    "CUDA 11.6" = list(
      "linux" = list(
        "libtorch" = list(
          path = "libtorch/",
          url = "https://download.pytorch.org/libtorch/cu116/libtorch-cxx11-abi-shared-with-deps-1.12.1%2Bcu116.zip",
          md5hash = "7972b17fa957f5d8f079035790b92207"
        ),
        "liblantern" = sprintf("https://storage.googleapis.com/torch-lantern-builds/refs/heads/%s/latest/Linux-gpu-116.zip", branch)
      )
    ),
    "ROCm 5.1" = list(
      "linux" = list(
        "libtorch" = list(
          path = "libtorch/",
          url = "https://download.pytorch.org/libtorch/rocm5.1.1/libtorch-cxx11-abi-shared-with-deps-1.12.1%2Brocm5.1.1.zip",
          md5hash = "496d4681f77422ee42fd8e49d3d5620d"
        ),
        "liblantern" = sprintf("https://storage.googleapis.com/torch-lantern-builds/refs/heads/%s/latest/Linux-rocm-51.zip", branch)
      )
    ),
    "ROCm 5.2" = list(
      "linux" = list(
        "libtorch" = list(
          path = "libtorch/",
          url = "https://download.pytorch.org/libtorch/nightly/rocm5.2/libtorch-cxx11-abi-shared-with-deps-latest.zip",
          md5hash = "e40b4e526197310296c992256de3bd9b"
        ),
        "liblantern" = sprintf("https://storage.googleapis.com/torch-lantern-builds/refs/heads/%s/latest/Linux-rocm-52.zip", branch)
      )
    ),
    "ROCm 5.3" = list(
      "linux" = list(
        "libtorch" = list(
          path = "libtorch/",
          url = "https://download.pytorch.org/libtorch/nightly/rocm5.3/libtorch-cxx11-abi-shared-with-deps-latest.zip",
          md5hash = "61fb206aecf076aa4afc3422fdbcc8ba"
        ),
        "liblantern" = sprintf("https://storage.googleapis.com/torch-lantern-builds/refs/heads/%s/latest/Linux-rocm-53.zip", branch)
      )
    )
  )
)

install_path <- function(version = "1.12.1") {
  path <- Sys.getenv("TORCH_HOME")
  if (nzchar(path)) {
    normalizePath(path, mustWork = FALSE)
  } else {
    normalizePath(file.path(system.file("", package = "torch")), mustWork = FALSE)
  }
}

#' A simple exported version of install_path
#' Returns the torch installation path.
#' @export
torch_install_path <- function() {
  install_path()
}

install_exists <- function() {
  if (!dir.exists(install_path())) {
    return(FALSE)
  }

  if (!length(list.files(file.path(install_path(), "lib"), "torch")) > 0) {
    return(FALSE)
  }

  if (!length(list.files(file.path(install_path(), "lib"), "lantern")) > 0) {
    return(FALSE)
  }

  TRUE
}

#' Verifies if torch is installed
#'
#' @export
torch_is_installed <- function() {
  install_exists()
}

lib_installed <- function(library_name, install_path) {
  x <- list.files(file.path(install_path, "lib"))

  if (library_name == "liblantern") {
    any(grepl("lantern", x))
  } else if (library_name == "libtorch") {
    any(grepl("torch", x))
  }
}

lantern_install_lib <- function(library_name, library_url,
                                install_path, source_path, filter, md5hash,
                                inst_path) {
  library_extension <- paste0(".", tools::file_ext(library_url))
  temp_file <- tempfile(fileext = library_extension)
  temp_path <- tempfile()

  utils::download.file(library_url, temp_file)
  on.exit(try(unlink(temp_file)))

  if (!is.null(md5hash) && is.character(md5hash) && length(md5hash) == 1) {
    hash <- tools::md5sum(temp_file)
    if (hash != md5hash) {
      stop(
        "The file downloaded from '", library_url,
        "' does not match the expected md5 hash '",
        md5hash, "'. The observed hash is '", hash,
        "'. Due to security reasons the installation is stopped."
      )
    }
  }

  uncompress <- if (identical(library_extension, "tgz")) utils::untar else utils::unzip

  uncompress(temp_file, exdir = temp_path)

  file.copy(
    from = dir(file.path(temp_path, source_path), full.names = TRUE),
    to = file.path(install_path, inst_path),
    recursive = TRUE
  )

  # if (!is.null(filter)) source_files <- Filter(filter, source_files)
  # file.copy(source_files, install_path, recursive = TRUE)
  #
  #
  # header_files <- dir(file.path(temp_path, "libtorch", "include"), full.names = T)
  # if (length(header_files) > 0) {
  #   # also install the include files.
  #   include_dir <- file.path(system.file("", package = "torch"), "include/")
  #   dir.create(include_dir, showWarnings = FALSE, recursive = TRUE)
  #   file.copy(header_files, include_dir, recursive = TRUE)
  # }
}

install_os <- function() {
  tolower(Sys.info()[["sysname"]])
}

lantern_install_libs <- function(version, type, install_path, install_config) {
  current_os <- install_os()

  if (!version %in% names(install_config)) {
    stop(
      "Version ", version, " is not available, available versions: ",
      paste(names(install_config), collapse = ", ")
    )
  }

  if (!type %in% names(install_config[[version]])) {
    stop("The ", type, " installation type is currently unsupported.")
  }

  if (!current_os %in% names(install_config[[version]][[type]])) {
    stop("The ", current_os, " operating system is currently unsupported.")
  }

  install_info <- install_config[[version]][[type]][[current_os]]
  
  if (current_os == "darwin") {
    install_info <- install_info[[R.version$arch]]
  }

  for (library_name in names(install_info)) {
    if (lib_installed(library_name, install_path)) {
      next
    }

    library_info <- install_info[[library_name]]

    if (!is.list(library_info)) {
      library_info <- list(url = library_info, filter = "", path = "", inst_path = "lib")
    }
    if (is.null(library_info$filter)) library_info$filter <- ""
    if (is.null(library_info$inst_path)) library_info$inst_path <- ""
    
    library_info <- maybe_switch_precxx11abi(library_info)

    lantern_install_lib(
      library_name = library_name,
      library_url = library_info$url,
      install_path = install_path,
      source_path = library_info$path,
      filter = function(e) grepl(library_info$filter, e),
      md5hash = library_info$md5hash,
      inst_path = library_info$inst_path
    )
  }

  invisible(install_path)
}

install_type_windows <- function(version) {
  cuda_version <- NULL
  cuda_path <- Sys.getenv("CUDA_PATH")

  if (nzchar(cuda_path)) {
    versions_file <- file.path(cuda_path, "version.txt")
    if (file.exists(versions_file)) {
      cuda_version <- gsub("CUDA Version |\\.[0-9]+$", "", readLines(versions_file))
    }
  }

  # Query nvcc from cuda in cuda_path.
  if (nzchar(cuda_path) && is.null(cuda_version)) {
    nvcc_path <- file.path(cuda_path, "bin", "nvcc.exe")
    cuda_version <- nvcc_version_from_path(nvcc_path)
    cuda_type <- paste0("CUDA ", cuda_version)
  }

  if (is.null(cuda_version)) {
    return("cpu")
  }

  types_available <- names(install_config[[version]])

  if (cuda_type %in% types_available) {
    return(cuda_type)
  } else {
    message(cuda_type, " detected but torch version ",version," only supports: ", paste(types_available, collapse = ", "))
    return("cpu")
  }
}

nvcc_version_from_path <- function(nvcc) {
  suppressWarnings(
    nvcc <- tryCatch(system2(nvcc, "--version", stdout = TRUE, stderr = TRUE), error = function(e) NULL)
  )

  if (is.null(nvcc) || !any(grepl("release", nvcc))) {
    return(NULL)
  }

  gsub(".*release |, V.*", "", nvcc[grepl("release", nvcc)])
}

hipcc_version_from_path <- function(hipcc) {
  suppressWarnings(
    hipcc <- tryCatch(system2(hipcc, "--version", stdout = TRUE, stderr = TRUE), error = function(e) NULL)
  )

  if (is.null(hipcc) || !any(grepl("version:", hipcc))) {
    return(NULL)
  }

  gsub(".*version: |\\.\\d+-.*", "", hipcc[grepl("version:", hipcc)])
}

#' @keywords internal
install_type <- function(version) {
  if (nzchar(Sys.getenv("CUDA"))) {
    return(Sys.getenv("CUDA"))
  }
  if (install_os() == "windows") {
    return(install_type_windows(version))
  }

  if (install_os() != "linux") {
    return("cpu")
  } # macOS

  # Detect cuda version on Linux

  cuda_version <- NULL
  cuda_home <- Sys.getenv("CUDA_HOME")

  # This file no longer exists with cuda >= 11
  if (nzchar(cuda_home)) {
    versions_file <- file.path(cuda_home, "version.txt")
    if (file.exists(versions_file)) {
      cuda_version <- gsub("CUDA Version |\\.[0-9]+$", "", readLines(versions_file))
    }
  }

  # Query nvcc from cuda in cuda_home path.
  if (nzchar(cuda_home) && is.null(cuda_version)) {
    nvcc_path <- file.path(cuda_home, "bin", "nvcc")
    cuda_version <- nvcc_version_from_path(nvcc_path)
  }

  # Try to find in conventional location.
  if (is.null(cuda_version)) {
    versions_file <- "/usr/local/cuda/version.txt"
    if (file.exists(versions_file)) {
      cuda_version <- gsub("CUDA Version |\\.[0-9]+$", "", readLines(versions_file))
    }
  }

  # Query nvcc from conventional location
  if (is.null(cuda_version)) {
    cuda_version <- nvcc_version_from_path("/usr/local/cuda/bin/nvcc")
  }

  if (is.null(cuda_version)) {
    cuda_version <- nvcc_version_from_path("nvcc")
  }

  if (!is.null(cuda_version)) {
    cuda_type <- paste0("CUDA ",cuda_version)
  }

  # Detect rocm version on Linux
  rocm_version <- NULL

  # Query hipcc from environment PATH.
  if (is.null(rocm_version)) {
    hipcc_path <- system("which hipcc", intern = T)
    rocm_version <- hipcc_version_from_path(hipcc_path)
  }
  
  # Query hipcc from conventional location
  if (is.null(rocm_version)) {
    rocm_version <- hipcc_version_from_path("/opt/rocm/bin/hipcc")
  }
  
  if (is.null(rocm_version)) {
    rocm_version <- hipcc_version_from_path("hipcc")
  }
  
  if (!is.null(rocm_version)) {
    rocm_type <- paste0("ROCm ",rocm_version)
  }
  
  # no version detected
  if (is.null(cuda_version) && is.null(rocm_version)) {
    return("cpu")
  }

  types_available <- names(install_config[[version]])
  cuda_type_available <- grep("CUDA ", types_available, value = TRUE)
  rocm_type_available <- grep("ROCm ", types_available, value = TRUE)

  # One of CUDA or ROCm type exists. Check it is supported or fallback to CPU
  if (!is.null(cuda_version)) {
    if (cuda_type %in% cuda_type_available) {
      return(cuda_type)
    } else {
      message(cuda_type, " detected but torch version ",version," only supports: ", paste(types_available, collapse = ", "))
      return("cpu")
    }
  } else {
    if (rocm_type %in% rocm_type_available) {
      return(rocm_type)
    } else {
      message(rocm_type, " detected but torch version ",version," only supports: ", paste(types_available, collapse = ", "))
      return("cpu")
    }
  }
}
=======
torch_version <- "1.13.1"
>>>>>>> origin/main

#' Install Torch
#'
#' Installs Torch and its dependencies.
#'
#' @param reinstall Re-install Torch even if its already installed?
#' @param ... Currently unused.
#' @param .inform_restart if `TRUE` and running in an `interactive()` session, after
#'   installation it will print a message to inform the user that the session must
#'   be restarted for torch to work correctly.
#' 
#' @details
#' This function is mainly controlled by environment variables that can be used
#' to override the defaults:
#' 
#' - `TORCH_HOME`: the installation path. By default dependencies are installed
#'    within the package directory. Eg what's given by `system.file(package="torch")`.
#' - `TORCH_URL`: A URL, path to a ZIP file or a directory containing a LibTorch version.
#'    Files will be installed/copied to the `TORCH_HOME` directory.
#' - `LANTERN_URL`: Same as `TORCH_URL` but for the Lantern library.
#' - `TORCH_INSTALL_DEBUG`: Setting it to 1, shows debug log messages during installation.
#' - `PRECXX11ABI`: Setting it to `1` will will trigger the installation of
#'    a Pre-cxx11 ABI installation of LibTorch. This can be useful in environments with
#'    older versions of GLIBC like CentOS7 and older Debian/Ubuntu versions.
#' - `LANTERN_BASE_URL`: The base URL for lantern files. This allows passing a directory
#'   where lantern binaries are located. The filename is then constructed as usual.
#' - `TORCH_COMMIT_SHA`: torch repository commit sha to be used when querying lantern
#'   uploads. Set it to `'none'` to avoid looking for build for that commit and 
#'   use the latest build for the branch.
#' - `CUDA`: We try to automatically detect the CUDA version installed in your system,
#'   but you might want to manually set it here. You can also disable CUDA installation
#'   by setting it to 'cpu'.
#' - `TORCH_R_VERSION`: The R torch version. It's unlikely that you need to change it,
#'   but it can be useful if you don't have the R package installed, but want to
#'   install the dependencies.
#' 
#' The \code{TORCH_INSTALL} environment
#' variable can be set to \code{0} to prevent auto-installing torch and \code{TORCH_LOAD} set to \code{0}
#' to avoid loading dependencies automatically. These environment variables are meant for advanced use
#' cases and troubleshooting only.
#' When timeout error occurs during library archive download, or length of downloaded files differ from
#' reported length, an increase of the \code{timeout} value should help.
#' 
#' @export
install_torch <- function(reinstall = FALSE, ..., .inform_restart = TRUE) {
  have_installed <- !torch_is_installed() || reinstall
  
  liblantern <- lantern_url()
  libtorch <- libtorch_url()
  
  install_lib("torch", libtorch, reinstall)
  install_lib("lantern", liblantern, reinstall)
  
  if (.inform_restart && have_installed && interactive()) {
    cli::cli_inform(c(
      v = "torch dependencies have been installed.",
      i = "You must restart your session to use {.pkg torch} correctly."
    ))
  }
  
  return(invisible(TRUE))
}

#' A simple exported version of install_path
#' Returns the torch installation path.
#' @export
torch_install_path <- function() {
  normalizePath(inst_path(), mustWork = FALSE)
}

#' Verifies if torch is installed
#'
#' @export
torch_is_installed <- function() {
  lib_is_installed("lantern", torch_install_path()) && 
    lib_is_installed("torch", torch_install_path())
}

install_lib <- function(libname, url, reinstall = FALSE) {
  inst_path <- torch_install_path()
  installer_message(c(
    "We are now proceeding to download and installing lantern and torch.",
    "The installation path is: {.path {inst_path}}"
  ))
  
  if (lib_is_installed(libname, inst_path) && !reinstall) {
    installer_message(c(
      "An installation of {.strong {libname}} already exists.",
      "Found file at {.path {inst_path}}."
    ))
    return(invisible(TRUE))
  }
  
  # The library URL can be 3 different things:
  # - real URL
  # - path to a zip file containing the library
  # - path to a directory containing the files to be installed. 
  if (is_url(url)) {
    tmp <- tempfile(fileext = ".zip")
    file.create(tmp)
    on.exit({file.remove(tmp)}, add = TRUE)
    
    download_file(url = url, destfile = tmp)
    url <- tmp
  }
  
  if (grepl("\\.zip$", url) && file.exists(url)) {
    tmp_ex <- tempfile()
    dir.create(tmp_ex)
    on.exit({unlink(tmp_ex)}, add = TRUE)
    
    utils::unzip(url, exdir = tmp_ex)
    url <- tmp_ex
  }

  if (dir.exists(url)) {
    # sometimes the extracted dir includes another directory that contains the
    # library within it.
    if (!lib_is_installed(libname, url)) {
      dirs <- list.files(url, full.names = TRUE)
      if (length(dirs) == 1) {
        url <- dirs
      }
    }
    
    # this where the installation actually happens
    if (lib_is_installed(libname, url)) {
      if (!dir.exists(inst_path)) {
        dir.create(inst_path, recursive = TRUE)
      }
      
      file.copy(
        from = dir(url, full.names = TRUE),
        to = file.path(inst_path, ""),
        recursive = TRUE
      )
    }
  }
  
  if (lib_is_installed(libname, inst_path)) {
    return(invisible(TRUE))
  } 
  
  rlang::abort(c(
    "Installation failed.",
    "Could not install {.strong {libname}} from {.val {url}}."
  ))
}

lib_is_installed <- function(libname, install_path) {
  if (file.exists(file.path(install_path, "lib", lib_name(libname))))
    return(TRUE)
  
  if (file.exists(file.path(install_path, "lib64", lib_name(libname))))
    return(TRUE)
  
  if (file.exists(file.path(install_path, "bin", lib_name(libname))))
    return(TRUE)
  
  FALSE
}

inst_path <- function() {
  install_path <- Sys.getenv("TORCH_HOME")
  if (nzchar(install_path)) return(install_path)
  system.file("", package = "torch")
}

libtorch_url <- function() {
  url <- Sys.getenv("TORCH_URL", "")
  
  if (url != "")
    return(url)
  
  if (is_macos()) {
    arch <- architecture()
    if (arch == "x86_64") {
      url <- glue::glue("https://download.pytorch.org/libtorch/cpu/libtorch-macos-{torch_version}.zip") 
    } else if (arch == "arm64") {
      url <- glue::glue("https://github.com/mlverse/libtorch-mac-m1/releases/download/LibTorch-for-R/libtorch-v{torch_version}.zip") 
    }
  }
  kind <- installation_kind()
  if (is_windows()) {
    url <- glue::glue("https://download.pytorch.org/libtorch/{kind}/libtorch-win-shared-with-deps-{torch_version}%2B{kind}.zip")
  }
  if (is_linux()) {
    precxx11 <- if(precxx11abi()) "" else "cxx11-abi-"
    url <- glue::glue("https://download.pytorch.org/libtorch/{kind}/libtorch-{precxx11}shared-with-deps-{torch_version}%2B{kind}.zip")
  }
  
  installer_message(c(
    "LibTorch will be downloaded from:",
    "{.url {url}}"
  ))
  
  url
}

lantern_url <- function() {
  url <- Sys.getenv("LANTERN_URL", "")
  
  # If a `LANTERN_URL` is set we use it for the download.
  if (url != "")
    return(url)
  
  # Otherwise we construct it from available information
  # file name we want to download has the following format:
  # lantern-<pkg-version>+<cpu|cu113>+<arch>+<precxx11>-<os>.zip
  pkg_version <- torch_r_version()
  kind <- installation_kind()
  arch <- architecture()
  precxx11 <- precxx11abi()
  os <- os_name()
  
  fname <- paste0("lantern-", pkg_version, "+", kind)
  if (is_linux() || is_macos()) {
    fname <- paste0(fname, "+", arch)
  }
  if (is_linux() && !is.null(precxx11) && precxx11) {
    fname <- paste0(fname, "+pre-cxx11")
  }
  fname <- paste0(fname, "-", os, ".zip")
  
  # we now query the base URL for that file name. There are 2 cases:
  # the package has been installed with remotes::install_github()
  # in this case the RemoteSha is stored in the package description and
  # we can install directly from it.
  # In the other cases, we download the latest version of the 'branch' variable.
  base_url <- Sys.getenv("LANTERN_BASE_URL", "")

  if (!nzchar(base_url)) {
    base_url <- "https://storage.googleapis.com/torch-lantern-builds/binaries/"
  
    remote_sha <- Sys.getenv("TORCH_COMMIT_SHA", "")
    if (remote_sha == "none") {
      remote_sha <- NA # if the user explicitly set it to none, we won't search for the SHA
    } else if (!nzchar(remote_sha)) {
      remote_sha <- desc::desc(package = "torch")$get("RemoteSha")  
      
      # pak adds the package version as the remote_sha when installing from
      # CRAN which breaks our assumption that it's always a commit sha. 
      # We identify this case and manually remove it.
      if (is_package_version(remote_sha)) {
        remote_sha <- NA
      }
    }
    
    if (is.na(remote_sha)) {
      installer_message(c(
        "Could not find the SHA of the commit that installed the package.",
        "Using the latest build for the specified branch: {.val {branch}}."
      ))
      base_url <- paste0(base_url, "refs/heads/", branch, "/latest/")
    } else {
      installer_message(c(
        "Could find the SHA of the commit that installed the package.",
        "SHA: {.val {remote_sha}}."
      ))
      base_url <- paste0(base_url, remote_sha, "/")
    }
  }

  final_url <- paste0(base_url, fname)

  if (is_url(final_url)) {
    final_url <- utils::URLencode(final_url)
  }

  installer_message(c(
    "Lantern will be downloaded from the following URL:",
    "{.url {final_url}}"
  ))
  
  final_url
}

torch_r_version <- function() {
  version <- Sys.getenv("TORCH_R_VERSION", unset = "")
  if (version != "") return(version)
  
  as.character(utils::packageVersion("torch"))
}

os_name <- function() {
  os <- Sys.info()["sysname"]
  if (!grepl('windows', os, ignore.case = TRUE)) {
    os
  } else {
    "win64"
  }
}

precxx11abi <- function() {
  abi <- Sys.getenv("PRECXX11ABI", "")
  
  if (abi != "" && !is_linux()) {
    installer_message("{.envvar PRECXX11ABI} value will be ignored. Only supported on Linux.")
  }
  
  if (!is_linux()) {
    return(NULL)
  }
  
  if (!is_truthy(abi)) {
    installer_message("Installing the CXX11 ABI enabled build.")
    return(FALSE)
  } else {
    installer_message("Installing the pre-CXX11 ABI enabled build.")
    return(TRUE)
  }
}


architecture <- function() {
  arch <- Sys.info()["machine"]

  if (!is_x86_64(arch) && (!is_macos())) {
    cli::cli_abort("Architecture {.val {arch}} is not supported in this OS.")
  }
  
  if ((!is_arm64(arch)) && (!is_x86_64(arch))) {
    cli::cli_abort("Unsupported architecture {.val {arch}}.")
  }
  
  installer_message("Architecture is {.val {arch}}")
  arch
}

is_x86_64 <- function(x) {
  x %in% c("x86_64", "x86-64")
}

is_arm64 <- function(x) {
  x %in% c("arm64")
}

installation_kind <- function() {
  cu <- cuda_version()
  if (is.null(cu)) {
    installer_message("No cuda instalation has been found. Using {.val cpu}.")
    return("cpu")
  } else if (cu == "cpu") {
    installer_message("{.envvar CUDA} is set to {.val cpu}, so using the {.val cpu}.")
    return("cpu")
  } else {
    cu <- paste0("cu", gsub(".", "", cu, fixed = TRUE))
    installer_message("Installation kind will be {.val {cu}}.")
    return(cu)
  }
}

cuda_version <- function() {
  
  version <- Sys.getenv("CUDA", "")
  if (version == "") {
    version <- NULL
  }
  
  if (!is.null(version)) {
    installer_message("{.envvar CUDA} has been specified. The CUDA version is {.strong {version}}")
    return(version)
  }
    
  if (is_windows()) {
    return(cuda_version_windows())
  }
  
  if (is_linux()) {
    return(cuda_version_linux())
  }
  
  installer_message("Not on Windows or Linux. No CUDA installation supported.")
  return(NULL)
}

cuda_version_linux <- function() {
  
  cuda_version <- NULL
  cuda_home <- Sys.getenv("CUDA_HOME")
  
  if (nzchar(cuda_home)) {
    installer_message("{.envvar CUDA_HOME}={.path {cuda_home}} is specified.")
  } else {
    installer_message("{.envvar CUDA_HOME} is not specified. Looking in conventional locations.")
  }
  
  # This file no longer exists with cuda >= 11
  if (nzchar(cuda_home)) {
    versions_file <- file.path(cuda_home, "version.txt")
    cuda_version <- cuda_version_from_version_txt_file(versions_file)
  }
  
  # Query nvcc from cuda in cuda_home path.
  if (nzchar(cuda_home) && is.null(cuda_version)) {
    nvcc_path <- file.path(cuda_home, "bin", "nvcc")
    cuda_version <- nvcc_version_from_path(nvcc_path)
  }
  
  # Try to find in conventional location.
  if (is.null(cuda_version)) {
    versions_file <- "/usr/local/cuda/version.txt"
    cuda_version <- cuda_version_from_version_txt_file(versions_file)
  }
  
  # Query nvcc from conventional location
  if (is.null(cuda_version)) {
    cuda_version <- nvcc_version_from_path("/usr/local/cuda/bin/nvcc")
  }
  
  if (is.null(cuda_version)) {
    cuda_version <- nvcc_version_from_path("nvcc")
  }
  
  check_supported_cuda_version_linux(cuda_version)
  
  cuda_version
}

cuda_version_windows <- function() {
  cuda_version <- NULL
  cuda_path <- Sys.getenv("CUDA_PATH")
  
  if (nzchar(cuda_path)) {
    installer_message(c(
      "{.envvar CUDA_PATH}={.path {cuda_path}}.", 
      "Trying to find CUDA in this path."
    ))
  } else {
    installer_message(c(
      "{.envvar CUDA_PATH} is not specified.", 
      "Searching for installation in conventional locations."
    ))
  }
  
  if (nzchar(cuda_path)) {
    versions_file <- file.path(cuda_path, "version.txt")
    cuda_version <- cuda_version_from_version_txt_file(versions_file)
  }
  
  # Query nvcc from cuda in cuda_path.
  if (nzchar(cuda_path) && is.null(cuda_version)) {
    nvcc_path <- file.path(cuda_path, "bin", "nvcc.exe")
    cuda_version <- nvcc_version_from_path(nvcc_path)
  }
  
  if (is.null(cuda_version)) {
    installer_message("Trying to use the nvcc version that might be on your path.")
    cuda_version <- nvcc_version_from_path("nvcc")
  }
  
  check_supported_cuda_version_windows(cuda_version)
  
  cuda_version
}

check_supported_cuda_version_windows <- function(version) {
  supported_versions <- c("11.7")
  check_supported_version(version, supported_versions)
}

check_supported_cuda_version_linux <- function(version) {
  supported_versions <- c("11.7", "11.6")
  check_supported_version(version, supported_versions)
}

check_supported_version <- function(version, supported_versions) {
  if (!is.null(version)) {
    if (!version %in% supported_versions) {
      cli::cli_abort(c(
        x = "Unsupported CUDA version {.val {version}}",
        i = "Currently supported versions are: {.val {supported_versions}}."
      ))
    }
  }
}

is_macos <- function() {
  grepl("darwin", Sys.info()["sysname"], ignore.case = TRUE)
}

is_windows <- function() {
  grepl("windows", Sys.info()["sysname"], ignore.case = TRUE)
}

is_linux <- function() {
  grepl("linux", Sys.info()["sysname"], ignore.case = TRUE)
}

cuda_version_from_version_txt_file <- function(versions_file) {
  cuda_version <- NULL
  if (file.exists(versions_file)) {
    cuda_version <- gsub("CUDA Version |\\.[0-9]+$", "", readLines(versions_file))
    installer_message(c(
      "Found CUDA version {.strong {cuda_version}}.",
      "This version was specified in {.path {versions_file}}"
    ))
  } else {
    installer_message(c(
      "Could not find a CUDA version in {.path {versions_file}}."
    ))
  }
  cuda_version
}

nvcc_version_from_path <- function(nvcc_path) {
  suppressWarnings(
    nvcc <- tryCatch(system2(nvcc_path, "--version", stdout = TRUE, stderr = TRUE), error = function(e) NULL)
  )
  
  if (is.null(nvcc) || !any(grepl("release", nvcc))) {
    installer_message(c(
      "Tried to query nvcc from {.path {nvcc_path}}, but was unable to find a CUDA version."
    )) 
    return(NULL)
  }
  
  version <- gsub(".*release |, V.*", "", nvcc[grepl("release", nvcc)])
  installer_message(c(
    "Found CUDA version {.strong {version}}.",
    "It was found by querying nvcc at {.path {nvcc_path}}."
  ))
  
  version
}

installer_message <- function(msg) {
  if (!is_truthy(Sys.getenv("TORCH_INSTALL_DEBUG", FALSE)))
    return(invisible(msg))
  names(msg) <- rep("i", length(msg))
  cli::cli_inform(msg, class = "torch_install", .envir = parent.frame())
}

is_truthy <- function(x) {
  if (length(x) == 0) {
    return(FALSE)
  }
  
  if (length(x) > 1) {
    stop("Unexpected value")
  }
  
  if (x == "") {
    return(FALSE)
  }
  
  if (x == "1") {
    return(TRUE)
  }
    
  (toupper(x) == TRUE)
}

lib_name <- function(name = "torch") {
  if (.Platform$OS.type == "unix") {
    paste0("lib", name, lib_ext())
  } else {
    paste0(name, lib_ext())
  }
}

lib_ext <- function() {
  if (grepl("darwin", version$os))
    ".dylib"
  else if (grepl("linux", version$os))
    ".so"
  else
    ".dll"
}

is_url <- function(x) {
  grepl("^https", x) || grepl("^http", x)
}

#' Install Torch from files
#'
#' List the Torch and Lantern libraries URLs to download as local files in order to proceed with  \code{install_torch_from_file()}.
#'
#' @inheritParams install_torch
#' @param version Not used
#' @param type Not used. This function is deprecated.
#'
#' @rdname install_torch_from_file
#' @export
get_install_libs_url <- function(version = NA, type = NA) {
  if (!is.na(type)) {
    cli::cli_abort("Please use the env vars describe in {.fn install_torch} to configure the installation type.")
  }
  if (!is.na(version)) {
    cli::cli_abort("It's not possible to configure the libtorch version.")
  }
  out <- list(
    libtorch = libtorch_url(), 
    liblantern = lantern_url()
  )
  
  if (interactive()) {
    cli::cli_inform(c(
      "Follow the links to download the dependencies, then set the {.envvar TORCH_URL} and {.envvar LANTERN_URL} env vars to the file paths.",
      "LibTorch: {.url {out$libtorch}}",
      "LibLantern: {.url {out$liblantern}}"
    ))
  }
  invisible(out)
}

#' Install Torch from files
#'
#' Installs Torch and its dependencies from files.
#'
#' @inheritParams install_torch
#' @param libtorch The installation archive file to use for Torch. Shall be a \code{"file://"} URL scheme.
#' @param liblantern The installation archive file to use for Lantern. Shall be a \code{"file://"} URL scheme.
#' @param ... other parameters to be passed to \code{"install_torch()"}
#'
#' @details
#'
#' When \code{"install_torch()"} initiated download is not possible, but installation archive files are
#' present on local filesystem, \code{"install_torch_from_file()"} can be used as a workaround to installation issue.
#' \code{"libtorch"} is the archive containing all torch modules, and \code{"liblantern"} is the C interface to libtorch
#' that is used for the R package. Both are highly dependent, and should be checked through \code{"get_install_libs_url()"}
#'
#' @examples
#' \dontrun{
#' # on a linux CPU platform 
#' get_install_libs_url()
#' # then after making both files available into /tmp/
#' Sys.setenv(TORCH_URL="/tmp/libtorch-v1.13.1.zip")
#' Sys.setenv(LANTERN_URL="/tmp/lantern-0.9.1.9001+cpu+arm64-Darwin.zip")
#' torch::install_torch()
#' }
#' @export
install_torch_from_file <- function(version = NA, type = NA, libtorch, liblantern, ...) {
  cli::cli_abort(c(
    "This function is now deprecated. The same results can be achieved with {.fn install_torch}.",
    i = "Use the envvars {.envvar TORCH_URL} and {.envvar LANTERN_URL} to set the file locations."
  ))
}

download_file <- function(url, destfile) {
  withr::local_options(timeout = max(600, getOption("timeout", default = 60)))
  tryCatch({
    utils::download.file(url = url, destfile = destfile)  
  }, 
  error = function(e) {
    
    additional_messages <- c()
    if (grepl("torch-lantern-builds", url)) {
      if (!grepl("refs", url)) {
        additional_messages <- c(
          i = "If you installed from GitHub, there might be no lantern build for that commit. Try setting {.envvar TORCH_COMMIT_SHA} to {.val none} to install from the latest commit."
        )
      }
    }
    
    cli::cli_abort(c(
      x = "Unable to download from {.url {url}}",
      i = "Please verify that the URL is not blocked by your firewall. See also {.url https://torch.mlverse.org/docs/articles/installation.html#file-based-download}",
      additional_messages
    ), parent = e)
  })
}

is_package_version <- function(x) {
  # .standard_regexps()$valid_numeric_version
  regex <- "([[:digit:]]+[.-])*[[:digit:]]+"
  grepl(regex, x)
}
