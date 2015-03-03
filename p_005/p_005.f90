!
! Smallest multiple
!
! Problem 5
!
! 2520 is the smallest number that can be divided by each of the numbers
! from 1 to 10 without any remainder.
! What is the smallest positive number that is evenly divisible by all of
! the numbers from 1 to 20?
!
program p_005
  use, intrinsic :: iso_fortran_env
  implicit none
  integer :: p(20), e(20), i, j, n, x
  n = 1
  p(1) = 2
  do i = 3, 20
     do j = 1, n
        if (mod(i, p(j)) == 0) exit
     end do
     if (j > n) then
        n = n + 1
        p(n) = i
     end if
  end do
  do i = 1, n
     j = 1
     do while (p(i) ** j < 20)
        j = j + 1
     end do
     e(i) = p(i) ** (j - 1)
  end do
  x = 1
  do i = 1, n
     x = x * e(i)
     write(output_unit, '("e = ",g0)') e(i)
  end do
  write(output_unit, '("solution: ",g0)') x
end program p_005
