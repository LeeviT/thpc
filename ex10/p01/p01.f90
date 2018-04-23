! Try e.g.
! ./md1d 10000 0.001 100000 10 1 | \
!  gawk '{print $1,$2 > "etot";print $1,$3 > "epot"; print $1,$4 > "ekin"}'; xgraph etot epot ekin



module sizes
  integer,parameter :: rk0=selected_real_kind(5,10)    ! single precision
  integer,parameter :: rk=selected_real_kind(10,40)    ! double precision
  integer,parameter :: rk2=selected_real_kind(30,200)  ! quadruple precision
  integer,parameter :: MAXBUF=200
end module sizes


program md1d
  use omp_lib
  use sizes
  implicit none

  real(rk),allocatable :: x(:)   ! atom positions
  real(rk),allocatable :: v(:)   !    velocities
  real(rk),allocatable :: v0(:)  !    previous veloocities (leap frog needs them)
  real(rk),allocatable :: a(:)   !    accelerations
  real(rk),allocatable :: ep(:)  !    potential energies
  real(rk),allocatable :: ek(:)  !    kinetic energies

  real(rk) :: epsum,eksum        ! system energies

  real(rk) :: dt                 ! time step
  real(rk) :: vsc                ! mean initial velocity
  real(rk) :: box                ! system size
  real(rk) :: xtmp, vtmp
  integer :: nat                 ! number of atoms
  integer :: nperthread          ! number of atoms per thread
  integer :: maxt                ! number of time steps simulated
  integer :: eout                ! energy output interval
  integer :: cout                ! coordinate output interval (lot of data, beware!)

  integer :: i,n,ia
  character(len=MAXBUF) :: arg

  ! clock variables
  integer :: c1, c2, c3, c4, cr, cm
  real :: rate

  ! OpenMP-related variables
  integer :: threadid, threads

  ! OpenMP initialization
  threads = 4
  call omp_set_num_threads(threads)

  ! clock initialization
  cr = 100000
  call system_clock(count_rate=cr)
  call system_clock(count_max=cm)
  rate = real(cr)
  call system_clock(c1)

  ! Get number of atoms, time step and simulation length from command line
  ia=command_argument_count()
  if (ia<4.or.ia>6) then
     call get_command_argument(0,arg)
     write(6,'(/,a,a,a)') 'usage: ',trim(arg),' nat dt maxt vsc [eout [cout]]'
     write(6,'(a)')       '    nat  = number of atoms'
     write(6,'(a)')       '    dt   = time step'
     write(6,'(a)')       '    maxt = number of time steps in simulation'
     write(6,'(a)')       '    vsc = mean velocity of atoms in the beginning (''temperature'')'
     write(6,'(a)')       '    eout = interval for printing energies to stdout'
     write(6,'(a,/)')     '    cout = interval for printing coordinates to ''fort.10'''
     stop
  end if
  cout=0
  eout=1
  call get_command_argument(1,arg); read(arg,*) nat
  call get_command_argument(2,arg); read(arg,*) dt
  call get_command_argument(3,arg); read(arg,*) maxt
  call get_command_argument(4,arg); read(arg,*) vsc
  if (ia>4) then
     call get_command_argument(5,arg); read(arg,*) eout
  end if
  if (ia>5) then
     call get_command_argument(6,arg); read(arg,*) cout
  end if

  allocate(x(nat),v(nat),v0(nat),a(nat),ep(nat),ek(nat))

  ! Initialize atoms positions and give them random velocities
  box=nat
  nperthread = nat/threads
  !$omp parallel workshare
  x=[(real(i,rk),i=0,nat-1)]
  !$omp end parallel workshare
  call random_number(v)
  v=vsc*v

  ! Remove center of mass velocity
  !$omp parallel workshare
  v=(v-sum(v)/nat)
  !$omp end parallel workshare
  n=0

  ! If the user wants calculate initial energy and print initial coords
  if (cout>0) then
    !$omp parallel do private(i)
     do i=1,nat
       call accel(i,ep(i),a(i))
     end do
     !$omp end parallel do
     call printcoords()
  end if

  ! Simulation proper
  time_loop: do n=1,maxt

     v0=v

     !$omp parallel do private(i)
     atom_loop1: do i=1, nat
        ! New potential energy and acceleration
        call accel(i,ep(i),a(i))
     end do atom_loop1
     !$omp end parallel do

     !$omp parallel do reduction(+:vtmp, xtmp)  private(i)
     atom_loop2: do i=1, nat
        ! Leap frog integration algorithm: update position and velocity
        vtmp=v(i)+dt*a(i)
        v(i)=vtmp
        xtmp=x(i)+dt*v(i)
        x(i)=xtmp

        ! Check periodic boundary conditions
        if (x(i)<0.0 ) x(i) = x(i) + box
        if (x(i)>=box) x(i) = x(i) - box

        ! Calculate kinetic energy (note: mass=1)
        ek(i)=0.5*((v0(i) + v(i))/2.0)**2

     end do atom_loop2
     !$omp end parallel do

     ! Calculate and print total potential end kinetic energies
     ! and their sum that should be conserved.
     !$omp parallel workshare
     epsum=sum(ep)
     eksum=sum(ek)
     !$omp end parallel workshare
     if (eout>0) then
        if (mod(n,eout)==0) print '(4g20.10)',dt*n,epsum+eksum,epsum,eksum
     end if
     if (cout>0) then
        if (mod(n,cout)==0) call printcoords()
     end if

  end do time_loop

  call system_clock(c2)
  write (*,*) 'wall clock time elapsed:', real(c2-c1)/rate

  stop

  contains

    subroutine accel(i,u,a)
      ! Calculate the potential energy u
      ! and acceleration a of atom i.
      integer,intent(in) :: i
      real(rk),intent(out) :: u,a
      real(rk),parameter :: d=1.0,k1=1.0,k2=0.1
      integer :: j,k
      real(rk) :: dxl,dxr

      j=i-1; if (j<1) j=nat
      k=i+1; if (k>nat) k=1

      dxl=x(i)-x(j)
      dxr=x(k)-x(i)
      if (dxl<-box/2.0) dxl=dxl+box
      if (dxl>=box/2.0) dxl=dxl-box
      if (dxr<-box/2.0) dxr=dxr+box
      if (dxr>=box/2.0) dxr=dxr-box
      dxl=dxl-d;
      dxr=dxr-d;

      u=(k1*(dxl**2+dxr**2)+k2*(dxl**3+dxr**3))/2.0
      a=-(2.0*k1*(dxl-dxr)+3.0*k2*(dxl**2-dxr**2))

      return
    end subroutine accel

    subroutine printcoords()
      integer :: ia
      real(rk),parameter :: xsc=2.35
      write(10,*) nat
      write(10,'(a,x,i0,x,i0,x,a,3f14.4)') ' Frame number ',n,n,' fs boxsize',box,10.0,10.0
      do ia=1,nat
         write(10,'(a,x,4g20.10)') 'Fe',xsc*x(ia),0.0,0.0,ep(ia)
      end do
      return
    end subroutine printcoords

  end program md1d
