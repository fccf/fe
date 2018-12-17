
include_directories(${CMAKE_CURRENT_BINARY_DIR}/../) # .mod file
# List the names of all the tests (same as subdirectory)
set(test_names
  test_element
)

foreach(test ${test_names})
  file(GLOB src ${CMAKE_CURRENT_SOURCE_DIR}/${test}/*.f90)
  add_executable(${test} ${src})
  target_link_libraries(${test} fe)
  add_test(${test} ${test})
endforeach()
