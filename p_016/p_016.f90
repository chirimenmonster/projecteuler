!
! Power digit sum
!
! Problem 16
!
! 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
!
! What is the sum of the digits of the number 2^1000?
!
program p_016
  use, intrinsic :: iso_fortran_env
  implicit none
  integer :: d(400), c, a, i, j, n
  d(:) = 0
  d(1) = 1
  c = 0
  n = 2
  do j = 1, 1000
     do i = 1, n
        d(i) = d(i) * 2 + c
        c = 0
        if (d(i) >= 10) then
           d(i) = d(i) - 10
           c = 1
        end if
     end do
     if (d(n) /= 0) n = n + 1
     !write(output_unit, '("2^",g0,"=",*(g0))') j, (d(i), i = n - 1, 1, -1)
  end do
  a = 0
  do i = 1, n - 1
     a = a + d(i) 
  end do
  write(output_unit, '("solution: ",g0)') a
end program p_016
