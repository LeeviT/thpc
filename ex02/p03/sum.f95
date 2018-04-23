program sumprog
  implicit none

  real(kind=16) :: t1, t2, sum
  integer :: k, n

  sum = 0
  n = 100000000

  call cpu_time(t1)

  do k = 0, n
    sum = sum + exp(sin(real(k, 16)/100000.0))
  end do

  call cpu_time(t2)

  write (*, *) t2-t1

end program sumprog
