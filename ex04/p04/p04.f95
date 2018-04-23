program p04
  implicit none

  integer(16), parameter :: n = 1200
  integer :: i, j
  real :: a(n,n), b(n,n), c(n,n), t1, t2

  call random_number(a)
  call random_number(b)

  c = 0.d0
  call cpu_time(t1)
  do i = 1, n
    do j = 1, n
      c(i,j) = c(i,j) + a(i,j)*b(i,j)
      a(i,j) = a(i,j) + 0.1*b(j,i)
      b(i,j) = b(i,j) + 0.2
    end do
  end do
  call cpu_time(t2)
  write (*,*) 'CPU time spent in loop', t2-t1

end program p04
