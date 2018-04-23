      SUBROUTINE DPTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
!
!  -- LAPACK routine (version 3.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     October 31, 1999
!
!     .. Scalar Arguments ..
      CHARACTER          COMPZ
      INTEGER            INFO, LDZ, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * ), WORK( * ), Z( LDZ, * )
!     ..
!
!  Purpose
!  =======
!
!  DPTEQR computes all eigenvalues and, optionally, eigenvectors of a
!  symmetric positive definite tridiagonal matrix by first factoring the
!  matrix using DPTTRF, and then calling DBDSQR to compute the singular
!  values of the bidiagonal factor.
!
!  This routine computes the eigenvalues of the positive definite
!  tridiagonal matrix to high relative accuracy.  This means that if the
!  eigenvalues range over many orders of magnitude in size, then the
!  small eigenvalues and corresponding eigenvectors will be computed
!  more accurately than, for example, with the standard QR method.
!
!  The eigenvectors of a full or band symmetric positive definite matrix
!  can also be found if DSYTRD, DSPTRD, or DSBTRD has been used to
!  reduce this matrix to tridiagonal form. (The reduction to tridiagonal
!  form, however, may preclude the possibility of obtaining high
!  relative accuracy in the small eigenvalues of the original matrix, if
!  these eigenvalues range over many orders of magnitude.)
!
!  Arguments
!  =========
!
!  COMPZ   (input) CHARACTER*1
!          = 'N':  Compute eigenvalues only.
!          = 'V':  Compute eigenvectors of original symmetric
!                  matrix also.  Array Z contains the orthogonal
!                  matrix used to reduce the original matrix to
!                  tridiagonal form.
!          = 'I':  Compute eigenvectors of tridiagonal matrix also.
!
!  N       (input) INTEGER
!          The order of the matrix.  N >= 0.
!
!  D       (input/output) DOUBLE PRECISION array, dimension (N)
!          On entry, the n diagonal elements of the tridiagonal
!          matrix.
!          On normal exit, D contains the eigenvalues, in descending
!          order.
!
!  E       (input/output) DOUBLE PRECISION array, dimension (N-1)
!          On entry, the (n-1) subdiagonal elements of the tridiagonal
!          matrix.
!          On exit, E has been destroyed.
!
!  Z       (input/output) DOUBLE PRECISION array, dimension (LDZ, N)
!          On entry, if COMPZ = 'V', the orthogonal matrix used in the
!          reduction to tridiagonal form.
!          On exit, if COMPZ = 'V', the orthonormal eigenvectors of the
!          original symmetric matrix;
!          if COMPZ = 'I', the orthonormal eigenvectors of the
!          tridiagonal matrix.
!          If INFO > 0 on exit, Z contains the eigenvectors associated
!          with only the stored eigenvalues.
!          If  COMPZ = 'N', then Z is not referenced.
!
!  LDZ     (input) INTEGER
!          The leading dimension of the array Z.  LDZ >= 1, and if
!          COMPZ = 'V' or 'I', LDZ >= max(1,N).
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension (4*N)
!
!  INFO    (output) INTEGER
!          = 0:  successful exit.
!          < 0:  if INFO = -i, the i-th argument had an illegal value.
!          > 0:  if INFO = i, and i is:
!                <= N  the Cholesky factorization of the matrix could
!                      not be performed because the i-th principal minor
!                      was not positive definite.
!                > N   the SVD algorithm failed to converge;
!                      if INFO = N+i, i off-diagonal elements of the
!                      bidiagonal factor did not converge to zero.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL           DBDSQR, DLASET, DPTTRF, XERBLA
!     ..
!     .. Local Arrays ..
      DOUBLE PRECISION   C( 1, 1 ), VT( 1, 1 )
!     ..
!     .. Local Scalars ..
      INTEGER            I, ICOMPZ, NRU
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX, SQRT
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
!
      if ( LSAME( COMPZ, 'N' ) ) then
         ICOMPZ = 0
      else if ( LSAME( COMPZ, 'V' ) ) then
         ICOMPZ = 1
      else if ( LSAME( COMPZ, 'I' ) ) then
         ICOMPZ = 2
      ELSE
         ICOMPZ = -1
      end if
      if ( ICOMPZ < 0 ) then
         INFO = -1
      else if ( N < 0 ) then
         INFO = -2
      else if ( ( LDZ < 1 ) .OR. ( ICOMPZ.GT.0 .AND. LDZ.LT.MAX( 1, &
               N ) ) ) then
         INFO = -6
      end if
      if ( INFO.NE.0 ) then
         CALL XERBLA( 'DPTEQR', -INFO )
         RETURN
      end if
!
!     Quick return if possible
!
      if ( N == 0 ) &
         RETURN
!
      if ( N == 1 ) then
         if ( ICOMPZ.GT.0 ) &
            Z( 1, 1 ) = ONE
         RETURN
      end if
      if ( ICOMPZ == 2 ) &
         CALL DLASET( 'Full', N, N, ZERO, ONE, Z, LDZ )
!
!     Call DPTTRF to factor the matrix.
!
      CALL DPTTRF( N, D, E, INFO )
      if ( INFO.NE.0 ) &
         RETURN
      DO 10 I = 1, N
         D( I ) = SQRT( D( I ) )
   10 CONTINUE
      DO 20 I = 1, N - 1
         E( I ) = E( I )*D( I )
   20 CONTINUE
!
!     Call DBDSQR to compute the singular values/vectors of the
!     bidiagonal factor.
!
      if ( ICOMPZ.GT.0 ) then
         NRU = N
      ELSE
         NRU = 0
      end if
      CALL DBDSQR( 'Lower', N, 0, NRU, 0, D, E, VT, 1, Z, LDZ, C, 1, &
                   WORK, INFO )
!
!     Square the singular values.
!
      if ( INFO == 0 ) then
         DO 30 I = 1, N
            D( I ) = D( I )*D( I )
   30    CONTINUE
      ELSE
         INFO = N + INFO
      end if
!
      RETURN
!
!     End of DPTEQR
!
      END
