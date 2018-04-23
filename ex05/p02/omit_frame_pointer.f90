
real function f1(x)
  real,intent(in) :: x
  f1=x/1e10
  return
end function f1

real function f2(x)
  real,intent(in) :: x
  f2=f1(x)*f1(x)
  return
end function f2

real function f3(x)
  real,intent(in) :: x
  f3=f2(x)+f2(x)
  return
end function f3

real function f4(x)
  real,intent(in) :: x
  f4=f3(x-1.0)*f3(x+1.0)
  return
end function f4

real function f5(x)
  real,intent(in) :: x
  f5=f4(x*2.0)*f3(x/2.0)
  return
end function f5

real function f6(x)
  real,intent(in) :: x
  f6=2.0*f5(x)
  return
end function f6

program omit_frame_pointer
  implicit none
  integer :: n,i
  real,allocatable :: a(:)
  real :: f1,f2,f3,f4,f5,f6
  real :: t1,t2

  n=10000000
  allocate(a(n))

  call cpu_time(t1)
  do i=1,n
     a(i)=f6(0.1*n)
  end do
  call cpu_time(t2)

  print *,sum(a),t2-t1

end program omit_frame_pointer

