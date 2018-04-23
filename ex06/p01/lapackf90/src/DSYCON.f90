      SUBROUTINE DSYCON( UPLO, N, A, LDA, IPIV, ANORM, RCOND, WORK, &
                         IWORK, INFO )
!
!  -- LAPACK routine (version 3.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     March 31, 1993
!
!     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDA, N
      DOUBLE PRECISION   ANORM, RCOND
!     ..
!     .. Array Arguments ..
      INTEGER            IPIV( * ), IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  DSYCON estimates the reciprocal of the condition number (in the
!  1-norm) of a real symmetric matrix A using the factorization
!  A = U*D*U**T or A = L*D*L**T computed by DSYTRF.
!
!  An estimate is obtained for norm(inv(A)), and the reciprocal of the
!  condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
!
!  Arguments
!  =========
!
!  UPLO    (input) CHARACTER*1
!          Specifies whether the details of the factorization are stored
!          as an upper or lower triangular matrix.
!          = 'U':  Upper triangular, form is A = U*D*U**T;
!          = 'L':  Lower triangular, form is A = L*D*L**T.
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.
!
!  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
!          The block diagonal matrix D and the multipliers used to
!          obtain the factor U or L as computed by DSYTRF.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).
!
!  IPIV    (input) INTEGER array, dimension (N)
!          Details of the interchanges and the block structure of D
!          as determined by DSYTRF.
!
!  ANORM   (input) DOUBLE PRECISION
!          The 1-norm of the original matrix A.
!
!  RCOND   (output) DOUBLE PRECISION
!          The reciprocal of the condition number of the matrix A,
!          computed as RCOND = 1/(ANORM * AINVNM), where AINVNM is an
!          estimate of the 1-norm of inv(A) computed in this routine.
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension (2*N)
!
!  IWORK    (workspace) INTEGER array, dimension (N)
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            I, KASE
      DOUBLE PRECISION   AINVNM
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL           DLACON, DSYTRS, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      if ( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) then
         INFO = -1
      else if ( N < 0 ) then
         INFO = -2
      else if ( LDA < MAX( 1, N ) ) then
         INFO = -4
      else if ( ANORM < ZERO ) then
         INFO = -6
      end if
      if ( INFO.NE.0 ) then
         CALL XERBLA( 'DSYCON', -INFO )
         RETURN
      end if
!
!     Quick return if possible
!
      RCOND = ZERO
      if ( N == 0 ) then
         RCOND = ONE
         RETURN
      else if ( ANORM.LE.ZERO ) then
         RETURN
      end if
!
!     Check that the diagonal matrix D is nonsingular.
!
      if ( UPPER ) then
!
!        Upper triangular storage: examine D from bottom to top
!
         DO 10 I = N, 1, -1
            if ( IPIV( I ).GT.0 .AND. A( I, I ) == ZERO ) &
               RETURN
   10    CONTINUE
      ELSE
!
!        Lower triangular storage: examine D from top to bottom.
!
         DO 20 I = 1, N
            if ( IPIV( I ).GT.0 .AND. A( I, I ) == ZERO ) &
               RETURN
   20    CONTINUE
      end if
!
!     Estimate the 1-norm of the inverse.
!
      KASE = 0
   30 CONTINUE
      CALL DLACON( N, WORK( N+1 ), WORK, IWORK, AINVNM, KASE )
      if ( KASE.NE.0 ) then
!
!        Multiply by inv(L*D*L') or inv(U*D*U').
!
         CALL DSYTRS( UPLO, N, 1, A, LDA, IPIV, WORK, N, INFO )
         GO TO 30
      end if
!
!     Compute the estimate of the reciprocal condition number.
!
      if ( AINVNM.NE.ZERO ) &
         RCOND = ( ONE / AINVNM ) / ANORM
!
      RETURN
!
!     End of DSYCON
!
      END