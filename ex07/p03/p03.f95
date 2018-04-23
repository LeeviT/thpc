program p03
  use mpi
  implicit none
  !include 'mpif.h'

  integer, parameter :: tag = 50
  integer, parameter :: seed = 24317
  integer :: id, ntasks, rc, n, i, remainder
  integer, allocatable, dimension(:) :: sendcounts, displs
  integer, dimension(mpi_status_size) :: status
  real, allocatable, dimension(:) :: randarray, randrec
  real :: avg, var, avgtot, vartot
  character(len=80) :: filename

  call mpi_init(rc)
  if (rc .ne. mpi_success) then
    write (*,*) 'MPI initialization failed.'
    stop
  end if

  call mpi_comm_size(mpi_comm_world, ntasks, rc)
  call mpi_comm_rank(mpi_comm_world, id, rc)

  filename = "randoms.dat"
  n = 30011
  allocate(sendcounts(ntasks), displs(ntasks))

  if (id .eq. 0) then
    allocate(randarray(n))
    ! generate content for the file
    open(unit=2, file=filename, action="write", status="replace")
    call random_number(randarray)
    randarray = randarray*10
    write (2,*) randarray
    randarray = 0
    close(2)

    ! read content from the file
    open(unit=3, file=filename, action="read")
    read (3,*) randarray
    close(3)

    ! compute the remainder of random number array size divided by ntasks
    ! and construct sendcounts and displs arrays
    remainder = mod(n, ntasks)
    sendcounts = (n - remainder)/ntasks
    if (remainder .ne. 0) sendcounts(ntasks) = sendcounts(1) + remainder
    displs(0) = 0
    do i=1, ntasks-1
      displs(i) = displs(i-1) + sendcounts(i-1)
    end do
    do i=1, ntasks-1
      call mpi_send(sendcounts, ntasks, mpi_integer, i, tag, &
                    mpi_comm_world, rc)
    end do
  end if

  if (id .ne. 0) call mpi_recv(sendcounts, ntasks, mpi_integer, mpi_any_source, &
                               tag, mpi_comm_world, status, rc)
  allocate(randrec(sendcounts(id+1)))
  randrec = 0
  ! distribute array over all the processes
  call mpi_scatterv(randarray, sendcounts, displs, &
                    mpi_real, randrec, sendcounts(id+1), &
                    mpi_real, 0, mpi_comm_world, rc)

  ! calculate average in every process
  avg = sum(randrec)/real(size(randrec))
  !write (*,*) 'Average of sub-array ', avg, 'in process ', id

  ! calculate variance in every process
  var = 0
  do i=1, size(randrec)
  var = var + ((randrec(i)-avg)**2)/real(size(randrec))
  end do
  !write (*,*) 'Variance of sub-array ', var, 'in process ', id

  call mpi_reduce(avg, avgtot, 1, mpi_real, mpi_sum, 0, mpi_comm_world, rc)
  call mpi_reduce(var, vartot, 1, mpi_real, mpi_sum, 0, mpi_comm_world, rc)

  if (id .eq. 0) write (*,*) 'Average value of the whole array: ', avgtot/real(ntasks)
  if (id .eq. 0) write (*,*) 'Variance of the whole array: ', vartot/real(ntasks)

  call mpi_finalize(rc)
end program p03
