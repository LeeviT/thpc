program p01a
  implicit none

  integer, parameter :: n = 10000000
  integer :: i
  real :: a(n), b(n), t1, t2

  call random_number(a)
  call random_number(b)

  ! original, non-optimized loop
  call cpu_time(t1)
  do i = 1, n-1
    if (i<500) then
      a(i) = 4.0*b(i) + b(i+1)
    else
      a(i) = 4.0*b(i+1) + b(i)
    end if
  end do
  call cpu_time(t2)
  write (*,*) 'CPU time spent in non-optimized loop', t2-t1
  ! original, non-optimized loop

  call random_number(a)
  call random_number(b)

  ! optimized loop
  call cpu_time(t1)
  do i = 1, 499
    a(i) = 4.0*b(i) + b(i+1)
  end do
  do i = 500, n-1, 5
    a(i) = 4.0*b(i+1) + b(i)
    a(i+1) = 4.0*b(i+2) + b(i+1)
    a(i+2) = 4.0*b(i+3) + b(i+2)
    a(i+3) = 4.0*b(i+4) + b(i+3)
    a(i+4) = 4.0*b(i+5) + b(i+4)
  end do
  call cpu_time(t2)
  write (*,*) 'CPU time spent in optimized loop', t2-t1
  ! optimized loop

end program p01a
