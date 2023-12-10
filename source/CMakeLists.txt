include_directories(${CMAKE_BINARY_DIR}/source/)

add_executable(NyquistAndBode2000
  nyquist_and_bode.f90
  func.f90
  main.f90
  )
