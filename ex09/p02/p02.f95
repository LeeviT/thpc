program p02
  use mpi
  implicit none

  integer :: n, i, j, k, t, nperproc
  real, dimension(:,:), allocatable :: a, atmp
  real, dimension(:), allocatable :: sendarri, recarri, sendarrj, recarrj
  real :: t1, t2
  ! MPI-related variables
  integer, parameter :: tag = 50, ndims = 2
  integer :: id, ntasks, rc, comm, dims(ndims), source, dest, sbuf
  integer, dimension(mpi_status_size) :: status
  logical :: periodic(ndims), reorder = .true.

  ! Initialize MPI
  call mpi_init(rc)
  if (rc .ne. mpi_success) then
    write (*,*) 'MPI initialization failed.'
    stop
  end if
  call mpi_comm_size(mpi_comm_world, ntasks, rc)
  call mpi_comm_rank(mpi_comm_world, id, rc)

  call cpu_time(t1)

  n = 640
  periodic = .true.
  dims = [2, 0]
  call mpi_dims_create(ntasks, ndims, dims, rc)
  call mpi_cart_create(mpi_comm_world, ndims, dims, periodic, reorder, comm, rc)

  nperproc = n/ntasks
  allocate(a(nperproc, nperproc), atmp(nperproc, nperproc))
  allocate(sendarri(nperproc), recarri(nperproc), &
           sendarrj(nperproc), recarrj(nperproc))

  ! fill matrix using initial condition
  do i=(id*nperproc + 1), (id + 1)*nperproc
    do j=(id*nperproc + 1), (id + 1)*nperproc
      a(i - id*nperproc, j - id*nperproc) = i + j
    end do
  end do

  timestep: do i=1, 10
    atmp = a
    ! construct arrays that will be send to neighbouring sub-matrix
    sendarri = a((id + 1)*nperproc,:)
    sendarrj = a(:,(id + 1)*nperproc)
    ! send and receive arrays
    call mpi_cart_shift(comm, 1, 1, source, dest, rc)
    call mpi_sendrecv(sendarri, nperproc, mpi_real, dest, tag, recarri, &
                      nperproc, mpi_real, source, tag, comm, status, rc)
    call mpi_sendrecv(sendarrj, nperproc, mpi_real, dest, tag, recarrj, &
                      nperproc, mpi_real, source, tag, comm, status, rc)
    ! calculate x_i,j using formula
    call singleiter(a, atmp, nperproc, recarri, recarrj)
  end do timestep

  call cpu_time(t2)

  if (id .eq. 0) write (*,*) t2-t1

  call mpi_finalize(rc)

contains

  subroutine singleiter(a, atmp, n, recarri, recarrj)
    implicit none

    integer, intent(in) :: n
    real, intent(inout) :: a(n, n), atmp(n, n)
    real, intent(inout) :: recarri(n), recarrj(n)
    integer :: i, j

    do i=1, n
      do j=1, n
        if (i .eq. 1 .and. i .ne. n .and. j .ne. 1 .and. j .ne. n) then
          a(i, j) = 0.125*(recarri(j)+atmp(i, j-1)+atmp(i+1, j)+atmp(i, j+1)) + atmp(i, j)
        else if (i .ne. 1 .and. i .ne. n .and. j .eq. 1 .and. j .ne. n) then
          a(i, j) = 0.125*(atmp(i-1, j)+recarrj(i)+atmp(i+1, j)+atmp(i, j+1)) + atmp(i, j)
        else if (i .ne. 1 .and. i .eq. n .and. j .ne. 1 .and. j .ne. n) then
          a(i, j) = 0.125*(atmp(i-1, j)+atmp(i, j-1)+recarri(j)+atmp(i, j+1)) + atmp(i, j)
        else if (i .ne. 1 .and. i .ne. n .and. j .ne. 1 .and. j .eq. n) then
          a(i, j) = 0.125*(atmp(i-1, j)+atmp(i, j-1)+atmp(i+1, j)+recarrj(i)) + atmp(i, j)
        else if (i .eq. 1 .and. i .ne. n .and. j .eq. 1 .and. j .ne. n) then
          a(i, j) = 0.125*(recarri(j)+recarrj(i)+atmp(i+1, j)+atmp(i, j+1)) + atmp(i, j)
        else if (i .ne. 1 .and. i .eq. n .and. j .ne. 1 .and. j .eq. n) then
          a(i, j) = 0.125*(atmp(i-1, j)+atmp(i, j-1)+recarri(j)+recarrj(i)) + atmp(i, j)
        else
          a(i, j) = 0.125*(atmp(i-1, j)+atmp(i, j-1)+atmp(i+1, j)+atmp(i, j+1)) + atmp(i, j)
        end if
      end do
    end do

    return

  end subroutine

end program p02
