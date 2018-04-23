      SUBROUTINE DGBCON( NORM, N, KL, KU, AB, LDAB, IPIV, ANORM, RCOND, &
                         WORK, IWORK, INFO )
!
!  -- LAPACK routine (version 3.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     September 30, 1994
!
!     .. Scalar Arguments ..
      CHARACTER          NORM
      INTEGER            INFO, KL, KU, LDAB, N
      DOUBLE PRECISION   ANORM, RCOND
!     ..
!     .. Array Arguments ..
      INTEGER            IPIV( * ), IWORK( * )
      DOUBLE PRECISION   AB( LDAB, * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  DGBCON estimates the reciprocal of the condition number of a real
!  general band matrix A, in either the 1-norm or the infinity-norm,
!  using the LU factorization computed by DGBTRF.
!
!  An estimate is obtained for norm(inv(A)), and the reciprocal of the
!  condition number is computed as
!     RCOND = 1 / ( norm(A) * norm(inv(A)) ).
!
!  Arguments
!  =========
!
!  NORM    (input) CHARACTER*1
!          Specifies whether the 1-norm condition number or the
!          infinity-norm condition number is required:
!          = '1' or 'O':  1-norm;
!          = 'I':         Infinity-norm.
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.
!
!  KL      (input) INTEGER
!          The number of subdiagonals within the band of A.  KL >= 0.
!
!  KU      (input) INTEGER
!          The number of superdiagonals within the band of A.  KU >= 0.
!
!  AB      (input) DOUBLE PRECISION array, dimension (LDAB,N)
!          Details of the LU factorization of the band matrix A, as
!          computed by DGBTRF.  U is stored as an upper triangular band
!          matrix with KL+KU superdiagonals in rows 1 to KL+KU+1, and
!          the multipliers used during the factorization are stored in
!          rows KL+KU+2 to 2*KL+KU+1.
!
!  LDAB    (input) INTEGER
!          The leading dimension of the array AB.  LDAB >= 2*KL+KU+1.
!
!  IPIV    (input) INTEGER array, dimension (N)
!          The pivot indices; for 1 <= i <= N, row i of the matrix was
!          interchanged with row IPIV(i).
!
!  ANORM   (input) DOUBLE PRECISION
!          If NORM = '1' or 'O', the 1-norm of the original matrix A.
!          If NORM = 'I', the infinity-norm of the original matrix A.
!
!  RCOND   (output) DOUBLE PRECISION
!          The reciprocal of the condition number of the matrix A,
!          computed as RCOND = 1/(norm(A) * norm(inv(A))).
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension (3*N)
!
!  IWORK   (workspace) INTEGER array, dimension (N)
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0: if INFO = -i, the i-th argument had an illegal value
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            LNOTI, ONENRM
      CHARACTER          NORMIN
      INTEGER            IX, J, JP, KASE, KASE1, KD, LM
      DOUBLE PRECISION   AINVNM, SCALE, SMLNUM, T
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            IDAMAX
      DOUBLE PRECISION   DDOT, DLAMCH
      EXTERNAL           LSAME, IDAMAX, DDOT, DLAMCH
!     ..
!     .. External Subroutines ..
      EXTERNAL           DAXPY, DLACON, DLATBS, DRSCL, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MIN
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      ONENRM = NORM == '1' .OR. LSAME( NORM, 'O' )
      if ( .NOT.ONENRM .AND. .NOT.LSAME( NORM, 'I' ) ) then
         INFO = -1
      else if ( N < 0 ) then
         INFO = -2
      else if ( KL < 0 ) then
         INFO = -3
      else if ( KU < 0 ) then
         INFO = -4
      else if ( LDAB < 2*KL+KU+1 ) then
         INFO = -6
      else if ( ANORM < ZERO ) then
         INFO = -8
      end if
      if ( INFO.NE.0 ) then
         CALL XERBLA( 'DGBCON', -INFO )
         RETURN
      end if
!
!     Quick return if possible
!
      RCOND = ZERO
      if ( N == 0 ) then
         RCOND = ONE
         RETURN
      else if ( ANORM == ZERO ) then
         RETURN
      end if
!
      SMLNUM = DLAMCH( 'Safe minimum' )
!
!     Estimate the norm of inv(A).
!
      AINVNM = ZERO
      NORMIN = 'N'
      if ( ONENRM ) then
         KASE1 = 1
      ELSE
         KASE1 = 2
      end if
      KD = KL + KU + 1
      LNOTI = KL.GT.0
      KASE = 0
   10 CONTINUE
      CALL DLACON( N, WORK( N+1 ), WORK, IWORK, AINVNM, KASE )
      if ( KASE.NE.0 ) then
         if ( KASE == KASE1 ) then
!
!           Multiply by inv(L).
!
            if ( LNOTI ) then
               DO 20 J = 1, N - 1
                  LM = MIN( KL, N-J )
                  JP = IPIV( J )
                  T = WORK( JP )
                  if ( JP.NE.J ) then
                     WORK( JP ) = WORK( J )
                     WORK( J ) = T
                  end if
                  CALL DAXPY( LM, -T, AB( KD+1, J ), 1, WORK( J+1 ), 1 )
   20          CONTINUE
            end if
!
!           Multiply by inv(U).
!
            CALL DLATBS( 'Upper', 'No transpose', 'Non-unit', NORMIN, N, &
                         KL+KU, AB, LDAB, WORK, SCALE, WORK( 2*N+1 ), &
                         INFO )
         ELSE
!
!           Multiply by inv(U').
!
            CALL DLATBS( 'Upper', 'Transpose', 'Non-unit', NORMIN, N, &
                         KL+KU, AB, LDAB, WORK, SCALE, WORK( 2*N+1 ), &
                         INFO )
!
!           Multiply by inv(L').
!
            if ( LNOTI ) then
               DO 30 J = N - 1, 1, -1
                  LM = MIN( KL, N-J )
                  WORK( J ) = WORK( J ) - DDOT( LM, AB( KD+1, J ), 1, &
                              WORK( J+1 ), 1 )
                  JP = IPIV( J )
                  if ( JP.NE.J ) then
                     T = WORK( JP )
                     WORK( JP ) = WORK( J )
                     WORK( J ) = T
                  end if
   30          CONTINUE
            end if
         end if
!
!        Divide X by 1/SCALE if doing so will not cause overflow.
!
         NORMIN = 'Y'
         if ( SCALE.NE.ONE ) then
            IX = IDAMAX( N, WORK, 1 )
            if ( SCALE < ABS( WORK( IX ) )*SMLNUM .OR. SCALE == ZERO ) &
               GO TO 40
            CALL DRSCL( N, SCALE, WORK, 1 )
         end if
         GO TO 10
      end if
!
!     Compute the estimate of the reciprocal condition number.
!
      if ( AINVNM.NE.ZERO ) &
         RCOND = ( ONE / AINVNM ) / ANORM
!
   40 CONTINUE
      RETURN
!
!     End of DGBCON
!
      END
