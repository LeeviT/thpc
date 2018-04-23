program p04
  use mpi
  implicit none
  !include 'mpif.h'
  integer, parameter :: tag = 50
  integer :: ntasks, id, rc, i, n, loopmax, t1, t2, t3, t4, clock_rate, clock_max
  integer, dimension(mpi_status_size) :: status
  real, allocatable :: a(:)
  real :: latency, bandwidth

  call mpi_init(rc)
  if (rc .ne. mpi_success) then
    write (*,*) 'MPI initialization failed.'
    stop
  end if

  call mpi_comm_size(mpi_comm_world, ntasks, rc)
  call mpi_comm_rank(mpi_comm_world, id, rc)

  loopmax = 10000000
  n = 1
  allocate(a(n))
  call random_number(a)
  clock_rate = 10
  clock_max = 1000000

  ! to determine the latency
  call system_clock(t1, clock_rate, clock_max)
  do i=1, loopmax
    if (id .eq. 0) call mpi_send(a, n, mpi_double_precision, 1, &
                                 tag, mpi_comm_world, rc)
    if (id .eq. 1) call mpi_recv(a, n, mpi_double_precision, 0, &
                                 tag, mpi_comm_world, status, rc)
    call mpi_barrier(mpi_comm_world, rc)
    if (id .eq. 1) call mpi_send(a, n, mpi_double_precision, 0, &
                                 tag, mpi_comm_world, rc)
    if (id .eq. 0) call mpi_recv(a, n, mpi_double_precision, 1, &
                                 tag, mpi_comm_world, status, rc)
    call mpi_barrier(mpi_comm_world, rc)
  end do
  call system_clock(t2, clock_rate, clock_max)
  if (id .eq. 0) write (*,*) 'latency:', (real(t2-t1)/real(clock_rate))/real(loopmax), 'seconds'

  deallocate(a)

  loopmax = 500
  n = 10000
  allocate(a(n))
  call random_number(a)

  ! to determine the bandwidth
  call system_clock(t3, clock_rate, clock_max)
  do i=1, loopmax
    if (id .eq. 0) call mpi_send(a, n, mpi_double_precision, 1, &
                                 tag, mpi_comm_world, rc)
    if (id .eq. 1) call mpi_recv(a, n, mpi_double_precision, 0, &
                                 tag, mpi_comm_world, status, rc)
    if (id .eq. 1) call mpi_send(a, n, mpi_double_precision, 0, &
                                 tag, mpi_comm_world, rc)
    if (id .eq. 0) call mpi_recv(a, n, mpi_double_precision, 1, &
                                 tag, mpi_comm_world, status, rc)
  end do
  call system_clock(t4, clock_rate, clock_max)
  bandwidth = 1/((real(t4-t3)/real(clock_rate)-latency)/real(loopmax))
  if (id .eq. 0) write (*,*) 'bandwidth:', bandwidth, 'data units/second'

  deallocate(a)
  call mpi_finalize(rc)

end program p04
