program p02
  implicit none

  integer, parameter :: n = 1000
  integer :: i, j, k
  real :: a(n,n), b(n,n), c(n,n), tmp, t1, t2

  call random_number(a)
  call random_number(b)

  ! C = A*B
  c = 0.d0
  call cpu_time(t1)
  do i = 1, n
    do j = 1, n
      tmp = 0.0
      do k = 1, n
        tmp = tmp + a(i,k)*b(k,j)
      end do
      c(i,j) = tmp
    end do
  end do
  call cpu_time(t2)
  write (*,*) 'CPU time spent in C=A*B product calculation', t2-t1
  ! C = A*B

  ! C = A^T*B
  c = 0.d0
  call cpu_time(t1)
  do i = 1, n
    do j = 1, n
      tmp = 0.0
      do k = 1, n
        tmp = tmp + a(k,i)*b(k,j)
      end do
      c(i,j) = tmp
    end do
  end do
  call cpu_time(t2)
  write (*,*) 'CPU time spent in C=A^T*B product calculation', t2-t1
  ! C = A^T*B

  ! C = A*B^T
  c = 0.d0
  call cpu_time(t1)
  do i = 1, n
    do j = 1, n
      tmp = 0.0
      do k = 1, n
        tmp = tmp + a(i,k)*b(j,k)
      end do
      c(i,j) = tmp
    end do
  end do
  call cpu_time(t2)
  write (*,*) 'CPU time spent in C=A*B^T product calculation', t2-t1
  ! C = A*B^T

end program
