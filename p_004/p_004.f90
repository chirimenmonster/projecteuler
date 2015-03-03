!
! Largest palindrome product
!
! Problem 4
!
! A palindromic number reads the same both ways. The largest palindrome made
! from the product of two 2-digit numbers is 9009 = 91 × 99.
! Find the largest palindrome made from the product of two 3-digit numbers.
!
program p_004
  use, intrinsic :: iso_fortran_env
  implicit none
  integer :: n(6), p, i, j, a, b, x, y
  do a = 999, 100, -1
     do b = 999, a, -1
        x = a * b
        p = x
        do i = 1, 6
           n(i) = mod(p, 10)
           p = p / 10
           if (p == 0) exit
        end do
        do j = 1, i / 2
           if (n(j) /= n(i - (j -1))) exit
        end do
        if (j == i / 2 + 1) then
           write(output_unit, '("find: ",g0," = ",g0," x ",g0)') x, a, b
           y = max(y, x)
           exit
        end if
     end do
     if (y >= a * 1000) exit
  end do
  write(output_unit, '("solution: ",g0)') y
end program p_004
