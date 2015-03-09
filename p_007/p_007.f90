!
! 10001st prime
!
! Problem 7
!
! By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
! that the 6th prime is 13.
! What is the 10001st prime number?
!
program p_007
  use, intrinsic :: iso_fortran_env
  implicit none
  integer :: prime(10000), num_prime, n, i
  prime(1) = 2
  num_prime = 1
  n = 3
  write(output_unit, '(i0)') 2
  do
     do i = 1, num_prime
        if (mod(n, prime(i)) == 0) exit
     end do
     if (i > num_prime) then
        if (num_prime == 10000) exit
        num_prime = num_prime + 1
        prime(num_prime) = n
        write(output_unit, '(", "i0)', advance='no') n
     end if
     n = n + 2
  end do
  write(output_unit, '("")')
  write(output_unit, '("solution: ",g0)') n
end program p_007
