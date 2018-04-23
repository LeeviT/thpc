program p04
  use mpi
  implicit none
  !include 'mpif.h'
  integer,parameter :: tag = 50
  integer :: id,ntasks,source_id,dest_id,rc,i,nlen,incint
  integer,dimension(mpi_status_size) :: status
  integer,dimension(2) :: msg

  call mpi_init(rc)
  if (rc/=mpi_success) then
     print *,'MPI initialization failed.'
     stop
  end if

  call mpi_comm_size(mpi_comm_world,ntasks,rc)
  call mpi_comm_rank(mpi_comm_world,id,rc)

  incint=0
  
  do i=1, 2
    if (id .eq. 0) call mpi_send(incint,1,mpi_integer,1,tag,mpi_comm_world,rc)
    if (id .eq. 1) call mpi_recv(incint,1,mpi_integer,0,tag,mpi_comm_world,status,rc)
    if (id .eq. 1) incint=incint+1
    if (id .eq. 1) call mpi_send(incint,1,mpi_integer,0,tag,mpi_comm_world,rc)
    if (id .eq. 0) call mpi_recv(incint,1,mpi_integer,1,tag,mpi_comm_world,status,rc)
    if (id .eq. 0) incint=incint+1
  end do

  if (id .eq. 0) write (*,*) incint


  call mpi_finalize(rc)
end program p04
