      SUBROUTINE DSYRFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB, &
                         X, LDX, FERR, BERR, WORK, IWORK, INFO )
!
!  -- LAPACK routine (version 3.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     September 30, 1994
!
!     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDA, LDAF, LDB, LDX, N, NRHS
!     ..
!     .. Array Arguments ..
      INTEGER            IPIV( * ), IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), AF( LDAF, * ), B( LDB, * ), &
                         BERR( * ), FERR( * ), WORK( * ), X( LDX, * )
!     ..
!
!  Purpose
!  =======
!
!  DSYRFS improves the computed solution to a system of linear
!  equations when the coefficient matrix is symmetric indefinite, and
!  provides error bounds and backward error estimates for the solution.
!
!  Arguments
!  =========
!
!  UPLO    (input) CHARACTER*1
!          = 'U':  Upper triangle of A is stored;
!          = 'L':  Lower triangle of A is stored.
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.
!
!  NRHS    (input) INTEGER
!          The number of right hand sides, i.e., the number of columns
!          of the matrices B and X.  NRHS >= 0.
!
!  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
!          The symmetric matrix A.  If UPLO = 'U', the leading N-by-N
!          upper triangular part of A contains the upper triangular part
!          of the matrix A, and the strictly lower triangular part of A
!          is not referenced.  If UPLO = 'L', the leading N-by-N lower
!          triangular part of A contains the lower triangular part of
!          the matrix A, and the strictly upper triangular part of A is
!          not referenced.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).
!
!  AF      (input) DOUBLE PRECISION array, dimension (LDAF,N)
!          The factored form of the matrix A.  AF contains the block
!          diagonal matrix D and the multipliers used to obtain the
!          factor U or L from the factorization A = U*D*U**T or
!          A = L*D*L**T as computed by DSYTRF.
!
!  LDAF    (input) INTEGER
!          The leading dimension of the array AF.  LDAF >= max(1,N).
!
!  IPIV    (input) INTEGER array, dimension (N)
!          Details of the interchanges and the block structure of D
!          as determined by DSYTRF.
!
!  B       (input) DOUBLE PRECISION array, dimension (LDB,NRHS)
!          The right hand side matrix B.
!
!  LDB     (input) INTEGER
!          The leading dimension of the array B.  LDB >= max(1,N).
!
!  X       (input/output) DOUBLE PRECISION array, dimension (LDX,NRHS)
!          On entry, the solution matrix X, as computed by DSYTRS.
!          On exit, the improved solution matrix X.
!
!  LDX     (input) INTEGER
!          The leading dimension of the array X.  LDX >= max(1,N).
!
!  FERR    (output) DOUBLE PRECISION array, dimension (NRHS)
!          The estimated forward error bound for each solution vector
!          X(j) (the j-th column of the solution matrix X).
!          If XTRUE is the true solution corresponding to X(j), FERR(j)
!          is an estimated upper bound for the magnitude of the largest
!          element in (X(j) - XTRUE) divided by the magnitude of the
!          largest element in X(j).  The estimate is as reliable as
!          the estimate for RCOND, and is almost always a slight
!          overestimate of the true error.
!
!  BERR    (output) DOUBLE PRECISION array, dimension (NRHS)
!          The componentwise relative backward error of each solution
!          vector X(j) (i.e., the smallest relative change in
!          any element of A or B that makes X(j) an exact solution).
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension (3*N)
!
!  IWORK   (workspace) INTEGER array, dimension (N)
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!
!  Internal Parameters
!  ===================
!
!  ITMAX is the maximum number of steps of iterative refinement.
!
!  =====================================================================
!
!     .. Parameters ..
      INTEGER            ITMAX
      PARAMETER          ( ITMAX = 5 )
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
      DOUBLE PRECISION   TWO
      PARAMETER          ( TWO = 2.0D+0 )
      DOUBLE PRECISION   THREE
      PARAMETER          ( THREE = 3.0D+0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            COUNT, I, J, K, KASE, NZ
      DOUBLE PRECISION   EPS, LSTRES, S, SAFE1, SAFE2, SAFMIN, XK
!     ..
!     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY, DLACON, DSYMV, DSYTRS, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, DLAMCH
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
      else if ( NRHS < 0 ) then
         INFO = -3
      else if ( LDA < MAX( 1, N ) ) then
         INFO = -5
      else if ( LDAF < MAX( 1, N ) ) then
         INFO = -7
      else if ( LDB < MAX( 1, N ) ) then
         INFO = -10
      else if ( LDX < MAX( 1, N ) ) then
         INFO = -12
      end if
      if ( INFO.NE.0 ) then
         CALL XERBLA( 'DSYRFS', -INFO )
         RETURN
      end if
!
!     Quick return if possible
!
      if ( N == 0 .OR. NRHS.EQ.0 ) then
         DO 10 J = 1, NRHS
            FERR( J ) = ZERO
            BERR( J ) = ZERO
   10    CONTINUE
         RETURN
      end if
!
!     NZ = maximum number of nonzero elements in each row of A, plus 1
!
      NZ = N + 1
      EPS = DLAMCH( 'Epsilon' )
      SAFMIN = DLAMCH( 'Safe minimum' )
      SAFE1 = NZ*SAFMIN
      SAFE2 = SAFE1 / EPS
!
!     Do for each right hand side
!
      DO 140 J = 1, NRHS
!
         COUNT = 1
         LSTRES = THREE
   20    CONTINUE
!
!        Loop until stopping criterion is satisfied.
!
!        Compute residual R = B - A * X
!
         CALL DCOPY( N, B( 1, J ), 1, WORK( N+1 ), 1 )
         CALL DSYMV( UPLO, N, -ONE, A, LDA, X( 1, J ), 1, ONE, &
                     WORK( N+1 ), 1 )
!
!        Compute componentwise relative backward error from formula
!
!        max(i) ( abs(R(i)) / ( abs(A)*abs(X) + abs(B) )(i) )
!
!        where abs(Z) is the componentwise absolute value of the matrix
!        or vector Z.  If the i-th component of the denominator is less
!        than SAFE2, then SAFE1 is added to the i-th components of the
!        numerator and denominator before dividing.
!
         DO 30 I = 1, N
            WORK( I ) = ABS( B( I, J ) )
   30    CONTINUE
!
!        Compute abs(A)*abs(X) + abs(B).
!
         if ( UPPER ) then
            DO 50 K = 1, N
               S = ZERO
               XK = ABS( X( K, J ) )
               DO 40 I = 1, K - 1
                  WORK( I ) = WORK( I ) + ABS( A( I, K ) )*XK
                  S = S + ABS( A( I, K ) )*ABS( X( I, J ) )
   40          CONTINUE
               WORK( K ) = WORK( K ) + ABS( A( K, K ) )*XK + S
   50       CONTINUE
         ELSE
            DO 70 K = 1, N
               S = ZERO
               XK = ABS( X( K, J ) )
               WORK( K ) = WORK( K ) + ABS( A( K, K ) )*XK
               DO 60 I = K + 1, N
                  WORK( I ) = WORK( I ) + ABS( A( I, K ) )*XK
                  S = S + ABS( A( I, K ) )*ABS( X( I, J ) )
   60          CONTINUE
               WORK( K ) = WORK( K ) + S
   70       CONTINUE
         end if
         S = ZERO
         DO 80 I = 1, N
            if ( WORK( I ).GT.SAFE2 ) then
               S = MAX( S, ABS( WORK( N+I ) ) / WORK( I ) )
            ELSE
               S = MAX( S, ( ABS( WORK( N+I ) )+SAFE1 ) / &
                   ( WORK( I )+SAFE1 ) )
            end if
   80    CONTINUE
         BERR( J ) = S
!
!        Test stopping criterion. Continue iterating if
!           1) The residual BERR(J) is larger than machine epsilon, and
!           2) BERR(J) decreased by at least a factor of 2 during the
!              last iteration, and
!           3) At most ITMAX iterations tried.
!
         if ( BERR( J ).GT.EPS .AND. TWO*BERR( J ).LE.LSTRES .AND. &
             COUNT.LE.ITMAX ) then
!
!           Update solution and try again.
!
            CALL DSYTRS( UPLO, N, 1, AF, LDAF, IPIV, WORK( N+1 ), N, &
                         INFO )
            CALL DAXPY( N, ONE, WORK( N+1 ), 1, X( 1, J ), 1 )
            LSTRES = BERR( J )
            COUNT = COUNT + 1
            GO TO 20
         end if
!
!        Bound error from formula
!
!        norm(X - XTRUE) / norm(X) .le. FERR =
!        norm( abs(inv(A))*
!           ( abs(R) + NZ*EPS*( abs(A)*abs(X)+abs(B) ))) / norm(X)
!
!        where
!          norm(Z) is the magnitude of the largest component of Z
!          inv(A) is the inverse of A
!          abs(Z) is the componentwise absolute value of the matrix or
!             vector Z
!          NZ is the maximum number of nonzeros in any row of A, plus 1
!          EPS is machine epsilon
!
!        The i-th component of abs(R)+NZ*EPS*(abs(A)*abs(X)+abs(B))
!        is incremented by SAFE1 if the i-th component of
!        abs(A)*abs(X) + abs(B) is less than SAFE2.
!
!        Use DLACON to estimate the infinity-norm of the matrix
!           inv(A) * diag(W),
!        where W = abs(R) + NZ*EPS*( abs(A)*abs(X)+abs(B) )))
!
         DO 90 I = 1, N
            if ( WORK( I ).GT.SAFE2 ) then
               WORK( I ) = ABS( WORK( N+I ) ) + NZ*EPS*WORK( I )
            ELSE
               WORK( I ) = ABS( WORK( N+I ) ) + NZ*EPS*WORK( I ) + SAFE1
            end if
   90    CONTINUE
!
         KASE = 0
  100    CONTINUE
         CALL DLACON( N, WORK( 2*N+1 ), WORK( N+1 ), IWORK, FERR( J ), &
                      KASE )
         if ( KASE.NE.0 ) then
            if ( KASE == 1 ) then
!
!              Multiply by diag(W)*inv(A').
!
               CALL DSYTRS( UPLO, N, 1, AF, LDAF, IPIV, WORK( N+1 ), N, &
                            INFO )
               DO 110 I = 1, N
                  WORK( N+I ) = WORK( I )*WORK( N+I )
  110          CONTINUE
            else if ( KASE == 2 ) then
!
!              Multiply by inv(A)*diag(W).
!
               DO 120 I = 1, N
                  WORK( N+I ) = WORK( I )*WORK( N+I )
  120          CONTINUE
               CALL DSYTRS( UPLO, N, 1, AF, LDAF, IPIV, WORK( N+1 ), N, &
                            INFO )
            end if
            GO TO 100
         end if
!
!        Normalize error.
!
         LSTRES = ZERO
         DO 130 I = 1, N
            LSTRES = MAX( LSTRES, ABS( X( I, J ) ) )
  130    CONTINUE
         if ( LSTRES.NE.ZERO ) &
            FERR( J ) = FERR( J ) / LSTRES
!
  140 CONTINUE
!
      RETURN
!
!     End of DSYRFS
!
      END
