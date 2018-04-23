program infloop
  implicit none

  integer(kind=16) :: i, a, b, c

  i = 1
  a = 2
  b = 3
  c = 4

  do
    i = i + 1
    a = a*1
    b = b + a
    c = c + a + b + i
  end do

end program infloop
