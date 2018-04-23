      SUBROUTINE DGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, IPIV, &
                         EQUED, R, C, B, LDB, X, LDX, RCOND, FERR, BERR, &
                         WORK, IWORK, INFO )
!
!  -- LAPACK driver routine (version 3.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     June 30, 1999
!
!     .. Scalar Arguments ..
      CHARACTER          EQUED, FACT, TRANS
      INTEGER            INFO, LDA, LDAF, LDB, LDX, N, NRHS
      DOUBLE PRECISION   RCOND
!     ..
!     .. Array Arguments ..
      INTEGER            IPIV( * ), IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), AF( LDAF, * ), B( LDB, * ), &
                         BERR( * ), C( * ), FERR( * ), R( * ), &
                         WORK( * ), X( LDX, * )
!     ..
!
!  Purpose
!  =======
!
!  DGESVX uses the LU factorization to compute the solution to a real
!  system of linear equations
!     A * X = B,
!  where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
!
!  Error bounds on the solution and a condition estimate are also
!  provided.
!
!  Description
!  ===========
!
!  The following steps are performed:
!
!  1. If FACT = 'E', real scaling factors are computed to equilibrate
!     the system:
!        TRANS = 'N':  diag(R)*A*diag(C)     *inv(diag(C))*X = diag(R)*B
!        TRANS = 'T': (diag(R)*A*diag(C))**T *inv(diag(R))*X = diag(C)*B
!        TRANS = 'C': (diag(R)*A*diag(C))**H *inv(diag(R))*X = diag(C)*B
!     Whether or not the system will be equilibrated depends on the
!     scaling of the matrix A, but if equilibration is used, A is
!     overwritten by diag(R)*A*diag(C) and B by diag(R)*B (if TRANS='N')
!     or diag(C)*B (if TRANS = 'T' or 'C').
!
!  2. If FACT = 'N' or 'E', the LU decomposition is used to factor the
!     matrix A (after equilibration if FACT = 'E') as
!        A = P * L * U,
!     where P is a permutation matrix, L is a unit lower triangular
!     matrix, and U is upper triangular.
!
!  3. If some U(i,i)=0, so that U is exactly singular, then the routine
!     returns with INFO = i. Otherwise, the factored form of A is used
!     to estimate the condition number of the matrix A.  If the
!     reciprocal of the condition number is less than machine precision,
!     INFO = N+1 is returned as a warning, but the routine still goes on
!     to solve for X and compute error bounds as described below.
!
!  4. The system of equations is solved for X using the factored form
!     of A.
!
!  5. Iterative refinement is applied to improve the computed solution
!     matrix and calculate error bounds and backward error estimates
!     for it.
!
!  6. If equilibration was used, the matrix X is premultiplied by
!     diag(C) (if TRANS = 'N') or diag(R) (if TRANS = 'T' or 'C') so
!     that it solves the original system before equilibration.
!
!  Arguments
!  =========
!
!  FACT    (input) CHARACTER*1
!          Specifies whether or not the factored form of the matrix A is
!          supplied on entry, and if not, whether the matrix A should be
!          equilibrated before it is factored.
!          = 'F':  On entry, AF and IPIV contain the factored form of A.
!                  If EQUED is not 'N', the matrix A has been
!                  equilibrated with scaling factors given by R and C.
!                  A, AF, and IPIV are not modified.
!          = 'N':  The matrix A will be copied to AF and factored.
!          = 'E':  The matrix A will be equilibrated if necessary, then
!                  copied to AF and factored.
!
!  TRANS   (input) CHARACTER*1
!          Specifies the form of the system of equations:
!          = 'N':  A * X = B     (No transpose)
!          = 'T':  A**T * X = B  (Transpose)
!          = 'C':  A**H * X = B  (Transpose)
!
!  N       (input) INTEGER
!          The number of linear equations, i.e., the order of the
!          matrix A.  N >= 0.
!
!  NRHS    (input) INTEGER
!          The number of right hand sides, i.e., the number of columns
!          of the matrices B and X.  NRHS >= 0.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the N-by-N matrix A.  If FACT = 'F' and EQUED is
!          not 'N', then A must have been equilibrated by the scaling
!          factors in R and/or C.  A is not modified if FACT = 'F' or
!          'N', or if FACT = 'E' and EQUED = 'N' on exit.
!
!          On exit, if EQUED .ne. 'N', A is scaled as follows:
!          EQUED = 'R':  A := diag(R) * A
!          EQUED = 'C':  A := A * diag(C)
!          EQUED = 'B':  A := diag(R) * A * diag(C).
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).
!
!  AF      (input or output) DOUBLE PRECISION array, dimension (LDAF,N)
!          If FACT = 'F', then AF is an input argument and on entry
!          contains the factors L and U from the factorization
!          A = P*L*U as computed by DGETRF.  If EQUED .ne. 'N', then
!          AF is the factored form of the equilibrated matrix A.
!
!          If FACT = 'N', then AF is an output argument and on exit
!          returns the factors L and U from the factorization A = P*L*U
!          of the original matrix A.
!
!          If FACT = 'E', then AF is an output argument and on exit
!          returns the factors L and U from the factorization A = P*L*U
!          of the equilibrated matrix A (see the description of A for
!          the form of the equilibrated matrix).
!
!  LDAF    (input) INTEGER
!          The leading dimension of the array AF.  LDAF >= max(1,N).
!
!  IPIV    (input or output) INTEGER array, dimension (N)
!          If FACT = 'F', then IPIV is an input argument and on entry
!          contains the pivot indices from the factorization A = P*L*U
!          as computed by DGETRF; row i of the matrix was interchanged
!          with row IPIV(i).
!
!          If FACT = 'N', then IPIV is an output argument and on exit
!          contains the pivot indices from the factorization A = P*L*U
!          of the original matrix A.
!
!          If FACT = 'E', then IPIV is an output argument and on exit
!          contains the pivot indices from the factorization A = P*L*U
!          of the equilibrated matrix A.
!
!  EQUED   (input or output) CHARACTER*1
!          Specifies the form of equilibration that was done.
!          = 'N':  No equilibration (always true if FACT = 'N').
!          = 'R':  Row equilibration, i.e., A has been premultiplied by
!                  diag(R).
!          = 'C':  Column equilibration, i.e., A has been postmultiplied
!                  by diag(C).
!          = 'B':  Both row and column equilibration, i.e., A has been
!                  replaced by diag(R) * A * diag(C).
!          EQUED is an input argument if FACT = 'F'; otherwise, it is an
!          output argument.
!
!  R       (input or output) DOUBLE PRECISION array, dimension (N)
!          The row scale factors for A.  If EQUED = 'R' or 'B', A is
!          multiplied on the left by diag(R); if EQUED = 'N' or 'C', R
!          is not accessed.  R is an input argument if FACT = 'F';
!          otherwise, R is an output argument.  If FACT = 'F' and
!          EQUED = 'R' or 'B', each element of R must be positive.
!
!  C       (input or output) DOUBLE PRECISION array, dimension (N)
!          The column scale factors for A.  If EQUED = 'C' or 'B', A is
!          multiplied on the right by diag(C); if EQUED = 'N' or 'R', C
!          is not accessed.  C is an input argument if FACT = 'F';
!          otherwise, C is an output argument.  If FACT = 'F' and
!          EQUED = 'C' or 'B', each element of C must be positive.
!
!  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
!          On entry, the N-by-NRHS right hand side matrix B.
!          On exit,
!          if EQUED = 'N', B is not modified;
!          if TRANS = 'N' and EQUED = 'R' or 'B', B is overwritten by
!          diag(R)*B;
!          if TRANS = 'T' or 'C' and EQUED = 'C' or 'B', B is
!          overwritten by diag(C)*B.
!
!  LDB     (input) INTEGER
!          The leading dimension of the array B.  LDB >= max(1,N).
!
!  X       (output) DOUBLE PRECISION array, dimension (LDX,NRHS)
!          If INFO = 0 or INFO = N+1, the N-by-NRHS solution matrix X
!          to the original system of equations.  Note that A and B are
!          modified on exit if EQUED .ne. 'N', and the solution to the
!          equilibrated system is inv(diag(C))*X if TRANS = 'N' and
!          EQUED = 'C' or 'B', or inv(diag(R))*X if TRANS = 'T' or 'C'
!          and EQUED = 'R' or 'B'.
!
!  LDX     (input) INTEGER
!          The leading dimension of the array X.  LDX >= max(1,N).
!
!  RCOND   (output) DOUBLE PRECISION
!          The estimate of the reciprocal condition number of the matrix
!          A after equilibration (if done).  If RCOND is less than the
!          machine precision (in particular, if RCOND = 0), the matrix
!          is singular to working precision.  This condition is
!          indicated by a return code of INFO > 0.
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
!  WORK    (workspace/output) DOUBLE PRECISION array, dimension (4*N)
!          On exit, WORK(1) contains the reciprocal pivot growth
!          factor norm(A)/norm(U). The "max absolute element" norm is
!          used. If WORK(1) is much less than 1, then the stability
!          of the LU factorization of the (equilibrated) matrix A
!          could be poor. This also means that the solution X, condition
!          estimator RCOND, and forward error bound FERR could be
!          unreliable. If factorization fails with 0<INFO<=N, then
!          WORK(1) contains the reciprocal pivot growth factor for the
!          leading INFO columns of A.
!
!  IWORK   (workspace) INTEGER array, dimension (N)
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!          > 0:  if INFO = i, and i is
!                <= N:  U(i,i) is exactly zero.  The factorization has
!                       been completed, but the factor U is exactly
!                       singular, so the solution and error bounds
!                       could not be computed. RCOND = 0 is returned.
!                = N+1: U is nonsingular, but RCOND is less than machine
!                       precision, meaning that the matrix is singular
!                       to working precision.  Nevertheless, the
!                       solution and error bounds are computed because
!                       there are a number of situations where the
!                       computed solution can be more accurate than the
!                       value of RCOND would suggest.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            COLEQU, EQUIL, NOFACT, NOTRAN, ROWEQU
      CHARACTER          NORM
      INTEGER            I, INFEQU, J
      DOUBLE PRECISION   AMAX, ANORM, BIGNUM, COLCND, RCMAX, RCMIN, &
                         ROWCND, RPVGRW, SMLNUM
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANGE, DLANTR
      EXTERNAL           LSAME, DLAMCH, DLANGE, DLANTR
!     ..
!     .. External Subroutines ..
      EXTERNAL           DGECON, DGEEQU, DGERFS, DGETRF, DGETRS, DLACPY, &
                         DLAQGE, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
!     ..
!     .. Executable Statements ..
!
      INFO = 0
      NOFACT = LSAME( FACT, 'N' )
      EQUIL = LSAME( FACT, 'E' )
      NOTRAN = LSAME( TRANS, 'N' )
      if ( NOFACT .OR. EQUIL ) then
         EQUED = 'N'
         ROWEQU = .FALSE.
         COLEQU = .FALSE.
      ELSE
         ROWEQU = LSAME( EQUED, 'R' ) .OR. LSAME( EQUED, 'B' )
         COLEQU = LSAME( EQUED, 'C' ) .OR. LSAME( EQUED, 'B' )
         SMLNUM = DLAMCH( 'Safe minimum' )
         BIGNUM = ONE / SMLNUM
      end if
!
!     Test the input parameters.
!
      if ( .NOT.NOFACT .AND. .NOT.EQUIL .AND. .NOT.LSAME( FACT, 'F' ) ) &
           THEN
         INFO = -1
      else if ( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) .AND. .NOT. &
               LSAME( TRANS, 'C' ) ) then
         INFO = -2
      else if ( N < 0 ) then
         INFO = -3
      else if ( NRHS < 0 ) then
         INFO = -4
      else if ( LDA < MAX( 1, N ) ) then
         INFO = -6
      else if ( LDAF < MAX( 1, N ) ) then
         INFO = -8
      else if ( LSAME( FACT, 'F' ) .AND. .NOT. &
               ( ROWEQU .OR. COLEQU .OR. LSAME( EQUED, 'N' ) ) ) then
         INFO = -10
      ELSE
         if ( ROWEQU ) then
            RCMIN = BIGNUM
            RCMAX = ZERO
            DO J = 1, N
               RCMIN = MIN( RCMIN, R( J ) )
               RCMAX = MAX( RCMAX, R( J ) )
            end do
            if ( RCMIN.LE.ZERO ) then
               INFO = -11
            else if ( N.GT.0 ) then
               ROWCND = MAX( RCMIN, SMLNUM ) / MIN( RCMAX, BIGNUM )
            ELSE
               ROWCND = ONE
            end if
         end if
         if ( COLEQU .AND. INFO == 0 ) then
            RCMIN = BIGNUM
            RCMAX = ZERO
            DO J = 1, N
               RCMIN = MIN( RCMIN, C( J ) )
               RCMAX = MAX( RCMAX, C( J ) )
            end do
            if ( RCMIN.LE.ZERO ) then
               INFO = -12
            else if ( N.GT.0 ) then
               COLCND = MAX( RCMIN, SMLNUM ) / MIN( RCMAX, BIGNUM )
            ELSE
               COLCND = ONE
            end if
         end if
         if ( INFO == 0 ) then
            if ( LDB < MAX( 1, N ) ) then
               INFO = -14
            else if ( LDX < MAX( 1, N ) ) then
               INFO = -16
            end if
         end if
      end if
!
      if ( INFO.NE.0 ) then
         CALL XERBLA( 'DGESVX', -INFO )
         RETURN
      end if
!
      if ( EQUIL ) then
!
!        Compute row and column scalings to equilibrate the matrix A.
!
         CALL DGEEQU( N, N, A, LDA, R, C, ROWCND, COLCND, AMAX, INFEQU )
         if ( INFEQU == 0 ) then
!
!           Equilibrate the matrix.
!
            CALL DLAQGE( N, N, A, LDA, R, C, ROWCND, COLCND, AMAX, &
                         EQUED )
            ROWEQU = LSAME( EQUED, 'R' ) .OR. LSAME( EQUED, 'B' )
            COLEQU = LSAME( EQUED, 'C' ) .OR. LSAME( EQUED, 'B' )
         end if
      end if
!
!     Scale the right hand side.
!
      if ( NOTRAN ) then
         if ( ROWEQU ) then
            DO 40 J = 1, NRHS
               DO 30 I = 1, N
                  B( I, J ) = R( I )*B( I, J )
   30          CONTINUE
   40       CONTINUE
         end if
      else if ( COLEQU ) then
         DO 60 J = 1, NRHS
            DO 50 I = 1, N
               B( I, J ) = C( I )*B( I, J )
   50       CONTINUE
   60    CONTINUE
      end if
!
      if ( NOFACT .OR. EQUIL ) then
!
!        Compute the LU factorization of A.
!
         CALL DLACPY( 'Full', N, N, A, LDA, AF, LDAF )
         CALL DGETRF( N, N, AF, LDAF, IPIV, INFO )
!
!        Return if INFO is non-zero.
!
         if ( INFO.NE.0 ) then
            if ( INFO.GT.0 ) then
!
!              Compute the reciprocal pivot growth factor of the
!              leading rank-deficient INFO columns of A.
!
               RPVGRW = DLANTR( 'M', 'U', 'N', INFO, INFO, AF, LDAF, &
                        WORK )
               if ( RPVGRW == ZERO ) then
                  RPVGRW = ONE
               ELSE
                  RPVGRW = DLANGE( 'M', N, INFO, A, LDA, WORK ) / RPVGRW
               end if
               WORK( 1 ) = RPVGRW
               RCOND = ZERO
            end if
            RETURN
         end if
      end if
!
!     Compute the norm of the matrix A and the
!     reciprocal pivot growth factor RPVGRW.
!
      if ( NOTRAN ) then
         NORM = '1'
      ELSE
         NORM = 'I'
      end if
      ANORM = DLANGE( NORM, N, N, A, LDA, WORK )
      RPVGRW = DLANTR( 'M', 'U', 'N', N, N, AF, LDAF, WORK )
      if ( RPVGRW == ZERO ) then
         RPVGRW = ONE
      ELSE
         RPVGRW = DLANGE( 'M', N, N, A, LDA, WORK ) / RPVGRW
      end if
!
!     Compute the reciprocal of the condition number of A.
!
      CALL DGECON( NORM, N, AF, LDAF, ANORM, RCOND, WORK, IWORK, INFO )
!
!     Set INFO = N+1 if the matrix is singular to working precision.
!
      if ( RCOND < DLAMCH( 'Epsilon' ) ) &
         INFO = N + 1
!
!     Compute the solution matrix X.
!
      CALL DLACPY( 'Full', N, NRHS, B, LDB, X, LDX )
      CALL DGETRS( TRANS, N, NRHS, AF, LDAF, IPIV, X, LDX, INFO )
!
!     Use iterative refinement to improve the computed solution and
!     compute error bounds and backward error estimates for it.
!
      CALL DGERFS( TRANS, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB, X, &
                   LDX, FERR, BERR, WORK, IWORK, INFO )
!
!     Transform the solution matrix X to a solution of the original
!     system.
!
      if ( NOTRAN ) then
         if ( COLEQU ) then
            DO 80 J = 1, NRHS
               DO 70 I = 1, N
                  X( I, J ) = C( I )*X( I, J )
   70          CONTINUE
   80       CONTINUE
            DO 90 J = 1, NRHS
               FERR( J ) = FERR( J ) / COLCND
   90       CONTINUE
         end if
      else if ( ROWEQU ) then
         DO 110 J = 1, NRHS
            DO 100 I = 1, N
               X( I, J ) = R( I )*X( I, J )
  100       CONTINUE
  110    CONTINUE
         DO 120 J = 1, NRHS
            FERR( J ) = FERR( J ) / ROWCND
  120    CONTINUE
      end if
!
      WORK( 1 ) = RPVGRW
      RETURN
!
!     End of DGESVX
!
      END
