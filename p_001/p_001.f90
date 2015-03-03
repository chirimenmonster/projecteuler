!
! Multiples of 3 and 5
!
! Problem 1
!
! If we list all the natural numbers below 10 that are multiples of 3 or 5,
! we get 3, 5, 6 and 9. The sum of these multiples is 23.
! Find the sum of all the multiples of 3 or 5 below 1000.
!
program p_001
  use, intrinsic :: iso_fortran_env
  implicit none
  integer :: i, sum
  sum = 0
  do i = 1, 999
     if (mod(i, 3) == 0 .or. mod(i, 5) == 0) then
        sum = sum + i
     end if
  end do
  write(output_unit, '("solution: ",g0)') sum
end program p_001
