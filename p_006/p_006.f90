!
! Sum square difference
!
! Problem 6
!
! The sum of the squares of the first ten natural numbers is,
!   1^2 + 2^2 + ... + 10^2 = 385
! The square of the sum of the first ten natural numbers is,
!   (1 + 2 + ... + 10)^2 = 55^2 = 3025
!
! Hence the difference between the sum of the squares of the first ten natural
! numbers and the square of the sum is 3025 − 385 = 2640.
! Find the difference between the sum of the squares of the first one hundred
! natural numbers and the square of the sum.
!
program p_006
  use, intrinsic :: iso_fortran_env
  implicit none
  integer :: a, b, i, r
  a = 0
  b = 0
  do i = 1, 100
     a = a + i ** 2
     b = b + i
  end do
  r = b ** 2 - a
  write(output_unit, *) a, b, r
  write(output_unit, '("solution: ",g0)') r
end program p_006
