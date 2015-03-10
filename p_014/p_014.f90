!
! Longest Collatz sequence
!
! Problem 14
!
! The following iterative sequence is defined for the set of positive integers:
!
!   n → n/2 (n is even)
!   n → 3n + 1 (n is odd)
!
! Using the rule above and starting with 13, we generate the following sequence:
!   13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
!
! It can be seen that this sequence (starting at 13 and finishing at 1)
! contains 10 terms. Although it has not been proved yet (Collatz Problem),
! it is thought that all starting numbers finish at 1.
!
! Which starting number, under one million, produces the longest chain?
!
! NOTE: Once the chain starts the terms are allowed to go above one million.
!
program p_014
  use, intrinsic :: iso_fortran_env
  implicit none
  integer(int64) :: n, m, b
  integer :: i, p, a(1000000)
  a(:) = 0
  p = 0
  do n = 1, 1000000
     m = n
     i = 1
     do while (m /= 1)
        if (mod(m, 2) == 0) then
           m = m / 2
           if (m <= 1000000 .and. a(m) /= 0) then
              i = i + a(m)
              exit
           end if
        else
           m = m * 3 + 1
        end if
        i = i + 1
     end do
     a(n) = i
     if (i > p) then
        b = n
        p = i
     end if
  end do
  write(output_unit, '("solution: ",g0,", count=",g0)') b, p
end program p_014
