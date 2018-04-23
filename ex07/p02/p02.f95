program p02
  use mpi
  implicit none
  !include 'mpif.h'

  integer, parameter :: seed = 24317
  integer(16) :: nall = 0
  integer :: n, i, nin, id, ntasks, rc
  real(16) :: x, y

  call mpi_init(rc)
  if (rc .ne. mpi_success) then
    write (*,*) 'MPI initialization failed.'
    stop
  end if

  call mpi_comm_size(mpi_comm_world, ntasks, rc)
  call mpi_comm_rank(mpi_comm_world, id, rc)

  n = 10000000
  nin = 0

  if (id .ne. 0) then
    do i=1, n
      call srand(seed+i*id)
      x = rand()
      y = rand()
      if (1.0 .ge. x*x + y*y) then
        nin = nin + 1
      end if
    end do
  end if

  call mpi_reduce(nin, nall, 1, mpi_integer, mpi_sum, 0, mpi_comm_world, rc)
  if (id .eq. 0) write (*,*) 'Approximate value for pi:', &
                             4.0*(real(nall)/real((ntasks-1)*n))

  call mpi_finalize(rc)

end program p02
