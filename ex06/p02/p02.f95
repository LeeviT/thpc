program p02
  use mpi
  implicit none
  !include 'mpif.h'
  integer, parameter :: tag = 50
  integer :: id, ntasks, rc, i, ierr
  integer, dimension(mpi_status_size) :: status
  integer, dimension(2) :: msg

  call mpi_init(rc)
  if (rc .ne. mpi_success) then
    write (*,*) 'MPI initialization failed.'
    stop
  end if

  call mpi_comm_size(mpi_comm_world, ntasks, rc)
  call mpi_comm_rank(mpi_comm_world, id, rc)

  msg = 0
  msg(1) = id

  if (id .ne. ntasks-1) write (*,*) 'sending process id:', id, &
                                    'receiving process id:', id+1
  if (id .ne. ntasks-1) call mpi_send(msg, 1, mpi_integer, &
                                      id+1, tag, mpi_comm_world,rc)
  call mpi_barrier(mpi_comm_world,ierr)
  if (id .ne. 0) call mpi_recv(msg, 1, mpi_integer, mpi_any_source, tag, &
                               mpi_comm_world, status, rc)
  if (id .ne. 0) write (*,*) 'sending process id:', status(mpi_source), &
                             'first element of msg array:', msg(1)

  call mpi_finalize(rc)


end program p02
