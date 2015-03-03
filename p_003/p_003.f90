!
! Largest prime factor
!
! Problem 3
!
! The prime factors of 13195 are 5, 7, 13 and 29.
! What is the largest prime factor of the number 600851475143 ?
!
program p_003
  use, intrinsic :: iso_fortran_env
  implicit none
  integer(int64) :: p, q
  q = 600851475143_8
  p = 3
  do
     if (p ** 2 > q) exit
     if (mod(q, p) /= 0) then
        p = p + 2
        cycle
     end if
     q = q / p
  end do
  write(output_unit, '("solution: ",g0)') q
end program p_003
