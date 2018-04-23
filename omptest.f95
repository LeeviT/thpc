program omptest
  use omp_lib
  implicit none

  integer :: i, n, c1, c2, cr, cm
  real :: rate, sum, sum2
  ! OpenMP-related variables
  integer :: threadid, threads

  n = 10000000
  sum = 0
  sum2 = 0
  call system_clock(count_rate=cr)
  call system_clock(count_max=cm)
  rate = real(cr)

  ! OpenMP initialization
  threads = 4
  call omp_set_num_threads(threads)

  call system_clock(c1)
  !$omp parallel do reduction(+:sum, sum2)
  do i=1, n
     sum = sum + 1
     sum2 = sum2 + 1
  end do
  !$omp end parallel do
  call system_clock(c2)

  write (*,*) sum, sum2, real(c2-c1)/rate

end program
