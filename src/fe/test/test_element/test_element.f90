program test_element
  use element
  implicit none

  class(ele_t), pointer :: ele

  call choose_element(ele, type='tri', degree = 1)

  call ele%info(6)


end program test_element
