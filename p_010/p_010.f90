!
! Summation of primes
!
! Problem 10
!
! The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
! Find the sum of all the primes below two million.
!
program p_009
  use, intrinsic :: iso_fortran_env
  implicit none
  integer(int64) :: i, n, sum
  sum = 2
  loop: do n = 3, 2000000, 2
     do i = 3, int(sqrt(real(n))), 2
        if (mod(n, i) == 0) cycle loop
     end do
     ! write(output_unit, '(g0,",")', advance="no") n
     sum = sum + n
  end do loop
  ! write(output_unit, '("")')
  write(output_unit, '("solution: ",g0)') sum
end program p_009
