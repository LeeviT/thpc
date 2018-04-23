program p01
  use mpi
  implicit none
  !include 'mpif.h'

  integer, parameter :: tag = 50
  integer :: n, i, nin, id, ntasks, rc, x
  integer, dimension(mpi_status_size) :: status

  call mpi_init(rc)
  if (rc .ne. mpi_success) then
    write (*,*) 'MPI initialization failed.'
    stop
  end if

  call mpi_comm_size(mpi_comm_world, ntasks, rc)
  call mpi_comm_rank(mpi_comm_world, id, rc)

  if (id .ne. 0) then
    do i=1, 100
      call mpi_send(id, 1, mpi_real, 0, tag+id, mpi_comm_world, rc)
    end do
  end if

  if (id .eq. 0) then
    do i=1, 50
      call mpi_recv(x, 1, mpi_real, mpi_any_source, mpi_any_tag, &
                    mpi_comm_world, status, rc)
      write (*,*) x
    end do
  end if

  call mpi_finalize(rc)
end program p01        
