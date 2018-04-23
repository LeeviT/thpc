      SUBROUTINE DPTSV( N, NRHS, D, E, B, LDB, INFO )
!
!  -- LAPACK routine (version 3.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 25, 1997
!
!     .. Scalar Arguments ..
      INTEGER            INFO, LDB, N, NRHS
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   B( LDB, * ), D( * ), E( * )
!     ..
!
!  Purpose
!  =======
!
!  DPTSV computes the solution to a real system of linear equations
!  A*X = B, where A is an N-by-N symmetric positive definite tridiagonal
!  matrix, and X and B are N-by-NRHS matrices.
!
!  A is factored as A = L*D*L**T, and the factored form of A is then
!  used to solve the system of equations.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.
!
!  NRHS    (input) INTEGER
!          The number of right hand sides, i.e., the number of columns
!          of the matrix B.  NRHS >= 0.
!
!  D       (input/output) DOUBLE PRECISION array, dimension (N)
!          On entry, the n diagonal elements of the tridiagonal matrix
!          A.  On exit, the n diagonal elements of the diagonal matrix
!          D from the factorization A = L*D*L**T.
!
!  E       (input/output) DOUBLE PRECISION array, dimension (N-1)
!          On entry, the (n-1) subdiagonal elements of the tridiagonal
!          matrix A.  On exit, the (n-1) subdiagonal elements of the
!          unit bidiagonal factor L from the L*D*L**T factorization of
!          A.  (E can also be regarded as the superdiagonal of the unit
!          bidiagonal factor U from the U**T*D*U factorization of A.)
!
!  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
!          On entry, the N-by-NRHS right hand side matrix B.
!          On exit, if INFO = 0, the N-by-NRHS solution matrix X.
!
!  LDB     (input) INTEGER
!          The leading dimension of the array B.  LDB >= max(1,N).
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!          > 0:  if INFO = i, the leading minor of order i is not
!                positive definite, and the solution has not been
!                computed.  The factorization has not been completed
!                unless i = N.
!
!  =====================================================================
!
!     .. External Subroutines ..
      EXTERNAL           DPTTRF, DPTTRS, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      if ( N < 0 ) then
         INFO = -1
      else if ( NRHS < 0 ) then
         INFO = -2
      else if ( LDB < MAX( 1, N ) ) then
         INFO = -6
      end if
      if ( INFO.NE.0 ) then
         CALL XERBLA( 'DPTSV ', -INFO )
         RETURN
      end if
!
!     Compute the L*D*L' (or U'*D*U) factorization of A.
!
      CALL DPTTRF( N, D, E, INFO )
      if ( INFO == 0 ) then
!
!        Solve the system A*X = B, overwriting B with X.
!
         CALL DPTTRS( N, NRHS, D, E, B, LDB, INFO )
      end if
      RETURN
!
!     End of DPTSV
!
      END
