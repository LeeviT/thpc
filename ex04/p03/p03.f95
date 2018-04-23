program p03
  implicit none

  integer, parameter :: n = 100000
  integer(4) :: a(n), i
  real :: t1, t2

  a = 0
  call cpu_time(t1)
  do i = 1, n
    a(i) = 100 + i**2
  end do
  call cpu_time(t2)
  write (*,*) 'CPU time spent in loop, no unrolling:', t2-t1

  a = 0
  call cpu_time(t1)
  do i = 1, n, 2
    a(i) = 100 + i**2
    a(i+1) = 100 + (i+1)**2
  end do
  call cpu_time(t2)
  write (*,*) 'CPU time spent in loop, unrolling level 2:', t2-t1

  a = 0
  call cpu_time(t1)
  do i = 1, n, 4
    a(i) = 100 + i**2
    a(i+1) = 100 + (i+1)**2
    a(i+2) = 100 + (i+2)**2
    a(i+3) = 100 + (i+3)**2
  end do
  call cpu_time(t2)
  write (*,*) 'CPU time spent in loop, unrolling level 4:', t2-t1

  a = 0
  call cpu_time(t1)
  do i = 1, n, 8
    a(i) = 100 + i**2
    a(i+1) = 100 + (i+1)**2
    a(i+2) = 100 + (i+2)**2
    a(i+3) = 100 + (i+3)**2
    a(i+4) = 100 + (i+4)**2
    a(i+5) = 100 + (i+5)**2
    a(i+6) = 100 + (i+6)**2
    a(i+7) = 100 + (i+7)**2
  end do
  call cpu_time(t2)
  write (*,*) 'CPU time spent in loop, unrolling level 8:', t2-t1

  a = 0
  call cpu_time(t1)
  do i = 1, n, 16
    a(i) = 100 + i**2
    a(i+1) = 100 + (i+1)**2
    a(i+2) = 100 + (i+2)**2
    a(i+3) = 100 + (i+3)**2
    a(i+4) = 100 + (i+4)**2
    a(i+5) = 100 + (i+5)**2
    a(i+6) = 100 + (i+6)**2
    a(i+7) = 100 + (i+7)**2
    a(i+8) = 100 + (i+8)**2
    a(i+9) = 100 + (i+9)**2
    a(i+10) = 100 + (i+10)**2
    a(i+11) = 100 + (i+11)**2
    a(i+12) = 100 + (i+12)**2
    a(i+13) = 100 + (i+13)**2
    a(i+14) = 100 + (i+14)**2
    a(i+15) = 100 + (i+15)**2
  end do
  call cpu_time(t2)
  write (*,*) 'CPU time spent in loop, unrolling level 16:', t2-t1

  a = 0
  call cpu_time(t1)
  do i = 1, n, 32
    a(i) = 100 + i**2
    a(i+1) = 100 + (i+1)**2
    a(i+2) = 100 + (i+2)**2
    a(i+3) = 100 + (i+3)**2
    a(i+4) = 100 + (i+4)**2
    a(i+5) = 100 + (i+5)**2
    a(i+6) = 100 + (i+6)**2
    a(i+7) = 100 + (i+7)**2
    a(i+8) = 100 + (i+8)**2
    a(i+9) = 100 + (i+9)**2
    a(i+10) = 100 + (i+10)**2
    a(i+11) = 100 + (i+11)**2
    a(i+12) = 100 + (i+12)**2
    a(i+13) = 100 + (i+13)**2
    a(i+14) = 100 + (i+14)**2
    a(i+15) = 100 + (i+15)**2
    a(i+16) = 100 + (i+16)**2
    a(i+17) = 100 + (i+17)**2
    a(i+18) = 100 + (i+18)**2
    a(i+19) = 100 + (i+19)**2
    a(i+20) = 100 + (i+20)**2
    a(i+21) = 100 + (i+21)**2
    a(i+22) = 100 + (i+22)**2
    a(i+23) = 100 + (i+23)**2
    a(i+24) = 100 + (i+24)**2
    a(i+25) = 100 + (i+25)**2
    a(i+26) = 100 + (i+26)**2
    a(i+27) = 100 + (i+27)**2
    a(i+28) = 100 + (i+28)**2
    a(i+29) = 100 + (i+29)**2
    a(i+30) = 100 + (i+30)**2
    a(i+31) = 100 + (i+31)**2
  end do
  call cpu_time(t2)
  write (*,*) 'CPU time spent in loop, unrolling level 32:', t2-t1

end program p03
