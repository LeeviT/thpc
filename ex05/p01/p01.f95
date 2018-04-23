program p01
  implicit none

  real(4) :: a(10000000), t1, t2
  integer :: i

  call random_number(a)

  ! unformatted output to file unform.datf
  call cpu_time(t1)
  open(2, file='unform.dat', form='unformatted', access='stream', &
       status='replace')
  do i=1, size(a)
    write (2) a(i)
  end do
  call cpu_time(t2)
  write (*,*) 'CPU time consumed, unformatted output:', t2-t1
  close(2)
  ! unformatted output to file unform.datf

  ! unformatted output to file form.datf
  call cpu_time(t1)
  open(3, file='form.dat', form='formatted', access='stream', &
       status='replace')
  do i=1, size(a)
    write (3,'(g0.10)') a(i)
  end do
  call cpu_time(t2)
  write (*,*) 'CPU time consumed, formatted output:', t2-t1
  close(3)
  ! unformatted output to file form.datf

end program p01
