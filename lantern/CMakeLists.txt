cmake_minimum_required(VERSION 3.16)

project(lantern)

############################################################
# Helpers
############################################################

function(download_file url filename)
    if(NOT EXISTS ${filename})
        message("-- Downloading ${filename}: ${url}")
        file(DOWNLOAD  ${url} ${filename}
             TIMEOUT 600
             TLS_VERIFY ON
        )
    endif()
endfunction()

function(retrieve_lib url name)
    download_file("${url}" "${CMAKE_CURRENT_BINARY_DIR}/${name}.zip")

    if(NOT EXISTS "${name}")
        message("-- Extracting ${name}")
        execute_process(COMMAND ${CMAKE_COMMAND} -E tar -xf ${name}.zip
                        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
    endif()
endfunction()


############################################################
# Torch
############################################################

if (DEFINED ENV{CUDA} AND NOT '$ENV{CUDA}' STREQUAL '')
  if (WIN32)
    find_package(CUDAToolkit)
  endif()
  enable_language(CUDA)
endif()

## ROCm based on https://github.com/ROCm-Developer-Tools/HIP/tree/master/samples/2_Cookbook/12_cmake_hip_add_executable#including-findhip-cmake-module-in-the-project
if(NOT DEFINED HIP_PATH)
    if(NOT DEFINED ENV{HIP_PATH})
        set(HIP_PATH "/opt/rocm/hip" CACHE PATH "Path to which HIP has been installed")
    else()
        set(HIP_PATH $ENV{HIP_PATH} CACHE PATH "Path to which HIP has been installed")
    endif()
endif()
set(CMAKE_MODULE_PATH "${HIP_PATH}/cmake" ${CMAKE_MODULE_PATH})
find_package(HIP QUIET)


if(APPLE)
    if(DEFINED ENV{CUDA} AND NOT '$ENV{CUDA}' STREQUAL '')
        message(FATAL_ERROR, "CUDA not supported on macos")
    else()
      if ('${CMAKE_HOST_SYSTEM_PROCESSOR}' STREQUAL 'x86_64')
        retrieve_lib("https://download.pytorch.org/libtorch/cpu/libtorch-macos-1.12.1.zip" "libtorch")
      elseif ('${CMAKE_HOST_SYSTEM_PROCESSOR}' STREQUAL 'arm64')
        retrieve_lib("https://github.com/mlverse/libtorch-mac-m1/releases/download/LibTorch-for-R/libtorch-v1.12.1.zip" "libtorch")
      else()
        message(FATAL_ERROR, "Arch not supported")
      endif()
    endif()
elseif(WIN32)
    if(DEFINED ENV{CUDA} AND '$ENV{CUDA}' STREQUAL '10.1')
        message(FATAL_ERROR, "CUDA 10.1 is no longer supported")
    elseif(DEFINED ENV{CUDA} AND '$ENV{CUDA}' STREQUAL '10.2')
        message(FATAL_ERROR, "CUDA 10.2 is no longer supported")
    elseif(DEFINED ENV{CUDA} AND '$ENV{CUDA}' STREQUAL '11.1')
        message(FATAL_ERROR, "CUDA 11.1 is no longer supported")
    elseif(DEFINED ENV{CUDA} AND '$ENV{CUDA}' STREQUAL '11.3')
        add_compile_definitions(CUDA113)
        retrieve_lib("https://download.pytorch.org/libtorch/cu113/libtorch-win-shared-with-deps-1.12.1%2Bcu113.zip" "libtorch")
    else()
        retrieve_lib("https://download.pytorch.org/libtorch/cpu/libtorch-win-shared-with-deps-1.12.1%2Bcpu.zip" "libtorch")
    endif()
elseif(UNIX)
    if (DEFINED ENV{PRECXX11ABI} AND '$ENV{PRECXX11ABI}' STREQUAL '1')
        message("-------- Building pre cxx11 ABI ----------")
        add_definitions(-D_GLIBCXX_USE_CXX11_ABI=0)

        if(DEFINED ENV{CUDA} AND '$ENV{CUDA}' STREQUAL '9.2')
            message(FATAL_ERROR, "CUDA 9.2 is no longer supported")
        elseif(DEFINED ENV{CUDA} AND '$ENV{CUDA}' STREQUAL '10.1')
            message(FATAL_ERROR, "CUDA 10.1 is no longer supported")
        elseif(DEFINED ENV{CUDA} AND '$ENV{CUDA}' STREQUAL '10.2')
            add_compile_definitions(CUDA102)
            retrieve_lib("https://download.pytorch.org/libtorch/cu102/libtorch-shared-with-deps-1.12.1%2Bcu102.zip" "libtorch")
        elseif(DEFINED ENV{CUDA} AND '$ENV{CUDA}' STREQUAL '11.0')
            message(FATAL_ERROR, "CUDA 11.0 is no longer supported")
        elseif(DEFINED ENV{CUDA} AND '$ENV{CUDA}' STREQUAL '11.1')
            message(FATAL_ERROR, "CUDA 11.1 is no longer supported")
        elseif(DEFINED ENV{CUDA} AND '$ENV{CUDA}' STREQUAL '11.3')
            add_compile_definitions(CUDA113)
            retrieve_lib("https://download.pytorch.org/libtorch/cu113/libtorch-shared-with-deps-1.12.1%2Bcu113.zip" "libtorch")
        elseif(DEFINED ENV{CUDA} AND '$ENV{CUDA}' STREQUAL '11.6')
            add_compile_definitions(CUDA116)
            retrieve_lib("https://download.pytorch.org/libtorch/cu116/libtorch-shared-with-deps-1.12.1%2Bcu116.zip" "libtorch")
        else()
            retrieve_lib("https://download.pytorch.org/libtorch/cpu/libtorch-shared-with-deps-1.12.1%2Bcpu.zip" "libtorch")
        endif()
    else()
        if(DEFINED ENV{CUDA} AND '$ENV{CUDA}' STREQUAL '9.2')
            message(FATAL_ERROR, "CUDA 9.2 is no longer supported")
        elseif(DEFINED ENV{CUDA} AND '$ENV{CUDA}' STREQUAL '10.1')
            message(FATAL_ERROR, "CUDA 10.1 is no longer supported")
        elseif(DEFINED ENV{CUDA} AND '$ENV{CUDA}' STREQUAL '10.2')
            add_compile_definitions(CUDA102)
            retrieve_lib("https://download.pytorch.org/libtorch/cu102/libtorch-cxx11-abi-shared-with-deps-1.12.1%2Bcu102.zip" "libtorch")
        elseif(DEFINED ENV{CUDA} AND '$ENV{CUDA}' STREQUAL '11.0')
            message(FATAL_ERROR, "CUDA 11.0 is no longer supported")
        elseif(DEFINED ENV{CUDA} AND '$ENV{CUDA}' STREQUAL '11.1')
            message(FATAL_ERROR, "CUDA 11.1 is no longer supported")
        elseif(DEFINED ENV{CUDA} AND '$ENV{CUDA}' STREQUAL '11.3')
            add_compile_definitions(CUDA113)
            retrieve_lib("https://download.pytorch.org/libtorch/cu113/libtorch-cxx11-abi-shared-with-deps-1.12.1%2Bcu113.zip" "libtorch")
        elseif(DEFINED ENV{CUDA} AND '$ENV{CUDA}' STREQUAL '11.6')
            add_compile_definitions(CUDA116)
            retrieve_lib("https://download.pytorch.org/libtorch/cu116/libtorch-cxx11-abi-shared-with-deps-1.12.1%2Bcu116.zip" "libtorch")
        elseif(HIP_FOUND)
            add_definitions(-D_GLIBCXX_USE_CXX11_ABI=1)
            add_compile_definitions(ROCM)
            find_package(ROCM REQUIRED)
            # -- may require merge w part of https://github.com/pytorch/pytorch/blob/fbd08fb358b643386edd4dd28b9c747aab4ba8c1/cmake/Dependencies.cmake
            # this leads to error at make time on  [ 31%] Building CXX object CMakeFiles/lantern.dir/src/TensorList.cpp.o
            # /home/creg/R/_packages/torch/lantern/tests/main.cpp:1:10: fatal error: 'fstream' file not found  
            # set(CMAKE_CXX_COMPILER "${HIP_PATH}/../bin/amdclang++")

            # this leads to error at make time on  [ 36%] Building CXX object CMakeFiles/lantern.dir/src/Dimname.cpp.o                                                     
            # In file included from <built-in>:1:                                                                                     
            # /opt/rocm-5.3.3/llvm/lib/clang/15.0.0/include/__clang_hip_runtime_wrapper.h:50:10: fatal error: 'cmath' file not foundIn file included from <built-in>
            # set(CMAKE_CXX_COMPILER "${HIP_PATH}/../bin/hipcc")
            
            set(CMAKE_CXX_COMPILER "/usr/lib/aomp/bin/hipcc")
            file(GLOB HIP_CLANGRT_LIB_SEARCH_PATHS "${HIP_PATH}/*/lib/*")
            find_library(CLANGRT_BUILTINS
                NAMES
                  clang_rt.builtins
                  clang_rt.builtins-x86_64
                PATHS
                  ${HIP_CLANGRT_LIB_SEARCH_PATHS}
                  ${HIP_CLANG_INCLUDE_PATH}/../lib/linux)
            list(APPEND CMAKE_PREFIX_PATH "/opt/rocm/lib/cmake/rocblas")
            list(APPEND CMAKE_PREFIX_PATH "/opt/rocm/lib/cmake/hipfft")
            list(APPEND CMAKE_PREFIX_PATH "/opt/rocm/lib/cmake/hiprand")
            list(APPEND CMAKE_PREFIX_PATH "/opt/rocm/lib/cmake/hipsparse")
            list(APPEND CMAKE_PREFIX_PATH "/opt/rocm/lib/cmake/rocthrust")
            message("from HIP_PATH we get CMAKE_CXX_COMPILER: (${HIP_PATH}) : ${CMAKE_CXX_COMPILER}")
            message("now using CMAKE_PREFIX_PATH: ${CMAKE_PREFIX_PATH}")
            message("now using CMAKE_MODULE_PATH: ${CMAKE_MODULE_PATH}")
            message("now using HIP_CLANGRT_LIB_SEARCH_PATHS: ${HIP_CLANGRT_LIB_SEARCH_PATHS}")
            message("detected ROCM_VERSION_DEV: ${ROCM_VERSION_DEV}")

            if(ROCM_VERSION_DEV_MAJOR VERSION_EQUAL "5")
              if(ROCM_VERSION_DEV_MINOR VERSION_EQUAL "3")
                retrieve_lib("https://download.pytorch.org/libtorch/nightly/rocm5.3/libtorch-cxx11-abi-shared-with-deps-latest.zip" "libtorch")
              elseif(ROCM_VERSION_DEV_MINOR VERSION_EQUAL "2")
                  retrieve_lib("https://download.pytorch.org/libtorch/nightly/rocm5.2/libtorch-cxx11-abi-shared-with-deps-latest.zip" "libtorch")
              elseif(ROCM_VERSION_DEV_MINOR VERSION_EQUAL "1")
                  retrieve_lib("https://download.pytorch.org/libtorch/rocm5.1.1/libtorch-cxx11-abi-shared-with-deps-1.12.1%2Brocm5.1.1.zip" "libtorch")
              endif()
           endif()
        else()
            retrieve_lib("https://download.pytorch.org/libtorch/cpu/libtorch-cxx11-abi-shared-with-deps-1.12.1%2Bcpu.zip" "libtorch")
        endif()
    endif()
else()
    message(FATAL_ERROR, "OS not supported.")
endif()

set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${CMAKE_CURRENT_BINARY_DIR}/libtorch/share/cmake/Torch")


find_program(CCACHE_FOUND ccache)
if(CCACHE_FOUND)
    message("-- Using ccache")
    set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ccache)
    set_property(GLOBAL PROPERTY RULE_LAUNCH_LINK ccache)
else()
    message("-- ccache not found.")
endif(CCACHE_FOUND)
set(USE_MKL 0)
find_package(Torch REQUIRED)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${TORCH_CXX_FLAGS}")

############################################################
# Library
############################################################

set(LANTERN_SRC 
    src/lantern.cpp
    src/TensorOptions.cpp
    src/Dtype.cpp
    src/Tensor.cpp
    src/Device.cpp
    src/utils.cpp
    src/MemoryFormat.cpp
    src/Generator.cpp
    src/QScheme.cpp
    src/TensorList.cpp
    src/Scalar.cpp
    src/Dimname.cpp
    src/Delete.cpp
    src/Reduction.cpp
    src/Quantization.cpp
    src/Autograd.cpp
    src/Function.cpp
    src/Layout.cpp
    src/Indexing.cpp
    src/Cuda.cpp
    src/NNUtilsRnn.cpp
    src/Storage.cpp
    src/Save.cpp
    src/Contrib/Sparsemax.cpp
    src/Threads.cpp
    src/Trace.cpp
    src/Stack.cpp
    src/Allocator.cpp
    src/Backends.cpp
    src/JITTypes.cpp
    src/ScriptModule.cpp
    src/IValue.cpp
    src/Compile.cpp
)

if(APPLE)
  if('${CMAKE_HOST_SYSTEM_PROCESSOR}' STREQUAL 'arm64')
    set(LANTERN_SRC ${LANTERN_SRC} src/AllocatorMPS.cpp)
  endif()
endif()

if(DEFINED ENV{CUDA} AND NOT '$ENV{CUDA}' STREQUAL '')
 
  set(LANTERN_SRC 
      ${LANTERN_SRC} 
      src/AllocatorCuda.cpp
      src/Contrib/SortVertices/sort_vert.cpp
      src/Contrib/SortVertices/sort_vert_kernel.cu
  )
  
  set_source_files_properties(src/Cuda.cpp PROPERTIES COMPILE_DEFINITIONS __NVCC__)
  
  add_library(lantern SHARED ${LANTERN_SRC})

  if (WIN32)  
    set_property(TARGET lantern PROPERTY CUDA_SEPARABLE_COMPILATION ON)
    set_property(TARGET lantern PROPERTY CUDA_STANDARD 17)
  else()
    set_property(TARGET lantern PROPERTY CUDA_STANDARD 14)
  endif()
  
else()
  set(LANTERN_SRC 
      ${LANTERN_SRC} 
      src/Contrib/SortVertices/sort_vert_cpu.cpp
  )
  add_library(lantern SHARED ${LANTERN_SRC})
  if (HIP_FOUND)
    message("-- Using HIP for ROCm ${ROCM_VERSION_DEV}")
    find_package(rocBLAS REQUIRED)
    target_link_libraries(roc::rocblas)
    find_package(hipFFT REQUIRED)
    target_link_libraries(hip::hipfft)
    find_package(hipRAND REQUIRED)
    target_link_libraries(hip::hiprand)
    find_package(hipSPARSE REQUIRED)
    target_link_libraries(roc::hipsparse)
    find_package(rocTHRUST REQUIRED)

    caffe2_update_option(USE_ROCM OFF)
    list(APPEND Caffe2_PUBLIC_HIP_DEPENDENCY_LIBS roc::rocblas hip::hipfft hip::hiprand roc::hipsparse)
    add_definitions(-DROCM_VERSION=${ROCM_VERSION_DEV_INT})
    add_definitions(-DTORCH_HIP_VERSION=${TORCH_HIP_VERSION})
    message("TORCH_HIP_VERSION=${TORCH_HIP_VERSION} is added as a compiler defines")
    
    # set(CMAKE_CXX_STANDARD 17)
    # set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17 -stdlib=libc++")
    set_source_files_properties(src/Cuda.cpp PROPERTIES COMPILE_DEFINITIONS __HIPCC__)
  endif()
endif()

add_library(lantern::library ALIAS lantern)

target_include_directories(lantern PUBLIC ${PROJECT_SOURCE_DIR}/include)

target_link_libraries(lantern ${TORCH_LIBRARIES})

set_property(TARGET lantern PROPERTY CXX_STANDARD 17)

############################################################
# Tests
############################################################

add_executable(lanterntest
    tests/init.cpp
    tests/main.cpp
)

target_include_directories(lanterntest PUBLIC
    ${PROJECT_SOURCE_DIR}/include
    tests/
)

target_link_libraries(lanterntest ${CMAKE_DL_LIBS})
