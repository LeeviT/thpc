program indices

  implicit none

  integer, allocatable :: a(:)
  integer :: i

  allocate(a(10))

  do i = 1, 10
    a(i) = i
  end do

  do i = 1, 200000
    write (*, *) a(i), i
  end do

end program indices
