      SUBROUTINE DRSCL( N, SA, SX, INCX )
!
!  -- LAPACK auxiliary routine (version 3.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     September 30, 1994
!
!     .. Scalar Arguments ..
      INTEGER            INCX, N
      DOUBLE PRECISION   SA
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   SX( * )
!     ..
!
!  Purpose
!  =======
!
!  DRSCL multiplies an n-element real vector x by the real scalar 1/a.
!  This is done without overflow or underflow as long as
!  the final result x/a does not overflow or underflow.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The number of components of the vector x.
!
!  SA      (input) DOUBLE PRECISION
!          The scalar a which is used to divide each component of x.
!          SA must be >= 0, or the subroutine will divide by zero.
!
!  SX      (input/output) DOUBLE PRECISION array, dimension
!                         (1+(N-1)*abs(INCX))
!          The n-element vector x.
!
!  INCX    (input) INTEGER
!          The increment between successive values of the vector SX.
!          > 0:  SX(1) = X(1) and SX(1+(i-1)*INCX) = x(i),     1< i<= n
!
! =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            DONE
      DOUBLE PRECISION   BIGNUM, CDEN, CDEN1, CNUM, CNUM1, MUL, SMLNUM
!     ..
!     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
!     ..
!     .. External Subroutines ..
      EXTERNAL           DSCAL
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS
!     ..
!     .. Executable Statements ..
!
!     Quick return if possible
!
      if ( N.LE.0 ) &
         RETURN
!
!     Get machine parameters
!
      SMLNUM = DLAMCH( 'S' )
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
!
!     Initialize the denominator to SA and the numerator to 1.
!
      CDEN = SA
      CNUM = ONE
!
   10 CONTINUE
      CDEN1 = CDEN*SMLNUM
      CNUM1 = CNUM / BIGNUM
      if ( ABS( CDEN1 ).GT.ABS( CNUM ) .AND. CNUM.NE.ZERO ) then
!
!        Pre-multiply X by SMLNUM if CDEN is large compared to CNUM.
!
         MUL = SMLNUM
         DONE = .FALSE.
         CDEN = CDEN1
      else if ( ABS( CNUM1 ).GT.ABS( CDEN ) ) then
!
!        Pre-multiply X by BIGNUM if CDEN is small compared to CNUM.
!
         MUL = BIGNUM
         DONE = .FALSE.
         CNUM = CNUM1
      ELSE
!
!        Multiply X by CNUM / CDEN and return.
!
         MUL = CNUM / CDEN
         DONE = .TRUE.
      end if
!
!     Scale the vector X by MUL
!
      CALL DSCAL( N, MUL, SX, INCX )
!
      if ( .NOT.DONE ) &
         GO TO 10
!
      RETURN
!
!     End of DRSCL
!
      END
