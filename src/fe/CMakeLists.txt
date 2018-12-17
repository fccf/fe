# library of fe
file(GLOB src ${CMAKE_CURRENT_SOURCE_DIR}/element/type/*.f90
${CMAKE_CURRENT_SOURCE_DIR}/element/*.f90)

add_library(fe STATIC ${src})

if(WITH_NO_TEST)
  set(enable_test_fe OFF CACHE BOOL OFF)
else()
  set(enable_test_fe ON CACHE BOOL OFF)
endif()

if(enable_test_fe)
  add_subdirectory(test)
endif()
