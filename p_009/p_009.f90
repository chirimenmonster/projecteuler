!
! Special Pythagorean triplet
!
! Problem 9
!
! A Pythagorean triplet is a set of three natural numbers, a < b < c,
! for which,
!
!   a^2 + b^2 = c^2
!
! For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
!
! There exists exactly one Pythagorean triplet for which a + b + c = 1000.
! Find the product abc.
!
program p_009
  use, intrinsic :: iso_fortran_env
  implicit none
  integer :: a, b, c
  loop: do a = 1, 333
     do b = a + 1, (1000 - a) / 2
        c = 1000 - (a + b)
        if (a ** 2 + b ** 2 == c ** 2) exit loop
     end do
  end do loop
  write(output_unit, '("a=",g0,", b=",g0,", c=",g0)') a, b, c
  write(output_unit, '("solution: ",g0)') a * b * c
end program p_009
