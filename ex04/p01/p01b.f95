program p01b
  implicit none

  integer, parameter :: n = 3000
  integer :: i, j
  real :: a(n,n), b(n,n), c(n), t1, t2

  call random_number(a)
  call random_number(b)
  call random_number(c)

  ! original, non-optimized loop
  call cpu_time(t1)
  do i = 1, n-1
    do j = 1, n
      a(i,j) = b(i,j)/c(i)
    end do
  end do
  call cpu_time(t2)
  write (*,*) 'CPU time spent in non-optimized loop', t2-t1
  ! original, non-optimized loop

  call random_number(a)
  call random_number(b)
  call random_number(c)

  ! optimized loop
  call cpu_time(t1)
  forall (i = 1:n-1, j = 1:n)
    a(i,j) = b(i,j)/c(i)
  end forall
  call cpu_time(t2)
  write (*,*) 'CPU time spent in optimized loop', t2-t1
  ! optimized loop

end program
