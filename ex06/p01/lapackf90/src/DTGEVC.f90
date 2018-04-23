      SUBROUTINE DTGEVC( SIDE, HOWMNY, SELECT, N, A, LDA, B, LDB, VL, &
                         LDVL, VR, LDVR, MM, M, WORK, INFO )
!
!  -- LAPACK routine (version 3.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     June 30, 1999
!
!     .. Scalar Arguments ..
      CHARACTER          HOWMNY, SIDE
      INTEGER            INFO, LDA, LDB, LDVL, LDVR, M, MM, N
!     ..
!     .. Array Arguments ..
      LOGICAL            SELECT( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), VL( LDVL, * ), &
                         VR( LDVR, * ), WORK( * )
!     ..
!
!
!  Purpose
!  =======
!
!  DTGEVC computes some or all of the right and/or left generalized
!  eigenvectors of a pair of real upper triangular matrices (A,B).
!
!  The right generalized eigenvector x and the left generalized
!  eigenvector y of (A,B) corresponding to a generalized eigenvalue
!  w are defined by:
!
!          (A - wB) * x = 0  and  y**H * (A - wB) = 0
!
!  where y**H denotes the conjugate tranpose of y.
!
!  If an eigenvalue w is determined by zero diagonal elements of both A
!  and B, a unit vector is returned as the corresponding eigenvector.
!
!  If all eigenvectors are requested, the routine may either return
!  the matrices X and/or Y of right or left eigenvectors of (A,B), or
!  the products Z*X and/or Q*Y, where Z and Q are input orthogonal
!  matrices.  If (A,B) was obtained from the generalized real-Schur
!  factorization of an original pair of matrices
!     (A0,B0) = (Q*A*Z**H,Q*B*Z**H),
!  then Z*X and Q*Y are the matrices of right or left eigenvectors of
!  A.
!
!  A must be block upper triangular, with 1-by-1 and 2-by-2 diagonal
!  blocks.  Corresponding to each 2-by-2 diagonal block is a complex
!  conjugate pair of eigenvalues and eigenvectors; only one
!  eigenvector of the pair is computed, namely the one corresponding
!  to the eigenvalue with positive imaginary part.
!
!  Arguments
!  =========
!
!  SIDE    (input) CHARACTER*1
!          = 'R': compute right eigenvectors only;
!          = 'L': compute left eigenvectors only;
!          = 'B': compute both right and left eigenvectors.
!
!  HOWMNY  (input) CHARACTER*1
!          = 'A': compute all right and/or left eigenvectors;
!          = 'B': compute all right and/or left eigenvectors, and
!                 backtransform them using the input matrices supplied
!                 in VR and/or VL;
!          = 'S': compute selected right and/or left eigenvectors,
!                 specified by the logical array SELECT.
!
!  SELECT  (input) LOGICAL array, dimension (N)
!          If HOWMNY='S', SELECT specifies the eigenvectors to be
!          computed.
!          If HOWMNY='A' or 'B', SELECT is not referenced.
!          To select the real eigenvector corresponding to the real
!          eigenvalue w(j), SELECT(j) must be set to .TRUE.  To select
!          the complex eigenvector corresponding to a complex conjugate
!          pair w(j) and w(j+1), either SELECT(j) or SELECT(j+1) must
!          be set to .TRUE..
!
!  N       (input) INTEGER
!          The order of the matrices A and B.  N >= 0.
!
!  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
!          The upper quasi-triangular matrix A.
!
!  LDA     (input) INTEGER
!          The leading dimension of array A.  LDA >= max(1, N).
!
!  B       (input) DOUBLE PRECISION array, dimension (LDB,N)
!          The upper triangular matrix B.  If A has a 2-by-2 diagonal
!          block, then the corresponding 2-by-2 block of B must be
!          diagonal with positive elements.
!
!  LDB     (input) INTEGER
!          The leading dimension of array B.  LDB >= max(1,N).
!
!  VL      (input/output) DOUBLE PRECISION array, dimension (LDVL,MM)
!          On entry, if SIDE = 'L' or 'B' and HOWMNY = 'B', VL must
!          contain an N-by-N matrix Q (usually the orthogonal matrix Q
!          of left Schur vectors returned by DHGEQZ).
!          On exit, if SIDE = 'L' or 'B', VL contains:
!          if HOWMNY = 'A', the matrix Y of left eigenvectors of (A,B);
!          if HOWMNY = 'B', the matrix Q*Y;
!          if HOWMNY = 'S', the left eigenvectors of (A,B) specified by
!                      SELECT, stored consecutively in the columns of
!                      VL, in the same order as their eigenvalues.
!          If SIDE = 'R', VL is not referenced.
!
!          A complex eigenvector corresponding to a complex eigenvalue
!          is stored in two consecutive columns, the first holding the
!          real part, and the second the imaginary part.
!
!  LDVL    (input) INTEGER
!          The leading dimension of array VL.
!          LDVL >= max(1,N) if SIDE = 'L' or 'B'; LDVL >= 1 otherwise.
!
!  VR      (input/output) DOUBLE PRECISION array, dimension (LDVR,MM)
!          On entry, if SIDE = 'R' or 'B' and HOWMNY = 'B', VR must
!          contain an N-by-N matrix Q (usually the orthogonal matrix Z
!          of right Schur vectors returned by DHGEQZ).
!          On exit, if SIDE = 'R' or 'B', VR contains:
!          if HOWMNY = 'A', the matrix X of right eigenvectors of (A,B);
!          if HOWMNY = 'B', the matrix Z*X;
!          if HOWMNY = 'S', the right eigenvectors of (A,B) specified by
!                      SELECT, stored consecutively in the columns of
!                      VR, in the same order as their eigenvalues.
!          If SIDE = 'L', VR is not referenced.
!
!          A complex eigenvector corresponding to a complex eigenvalue
!          is stored in two consecutive columns, the first holding the
!          real part and the second the imaginary part.
!
!  LDVR    (input) INTEGER
!          The leading dimension of the array VR.
!          LDVR >= max(1,N) if SIDE = 'R' or 'B'; LDVR >= 1 otherwise.
!
!  MM      (input) INTEGER
!          The number of columns in the arrays VL and/or VR. MM >= M.
!
!  M       (output) INTEGER
!          The number of columns in the arrays VL and/or VR actually
!          used to store the eigenvectors.  If HOWMNY = 'A' or 'B', M
!          is set to N.  Each selected real eigenvector occupies one
!          column and each selected complex eigenvector occupies two
!          columns.
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension (6*N)
!
!  INFO    (output) INTEGER
!          = 0:  successful exit.
!          < 0:  if INFO = -i, the i-th argument had an illegal value.
!          > 0:  the 2-by-2 block (INFO:INFO+1) does not have a complex
!                eigenvalue.
!
!  Further Details
!  ===============
!
!  Allocation of workspace:
!  ---------- -- ---------
!
!     WORK( j ) = 1-norm of j-th column of A, above the diagonal
!     WORK( N+j ) = 1-norm of j-th column of B, above the diagonal
!     WORK( 2*N+1:3*N ) = real part of eigenvector
!     WORK( 3*N+1:4*N ) = imaginary part of eigenvector
!     WORK( 4*N+1:5*N ) = real part of back-transformed eigenvector
!     WORK( 5*N+1:6*N ) = imaginary part of back-transformed eigenvector
!
!  Rowwise vs. columnwise solution methods:
!  ------- --  ---------- -------- -------
!
!  Finding a generalized eigenvector consists basically of solving the
!  singular triangular system
!
!   (A - w B) x = 0     (for right) or:   (A - w B)**H y = 0  (for left)
!
!  Consider finding the i-th right eigenvector (assume all eigenvalues
!  are real). The equation to be solved is:
!       n                   i
!  0 = sum  C(j,k) v(k)  = sum  C(j,k) v(k)     for j = i,. . .,1
!      k=j                 k=j
!
!  where  C = (A - w B)  (The components v(i+1:n) are 0.)
!
!  The "rowwise" method is:
!
!  (1)  v(i) := 1
!  for j = i-1,. . .,1:
!                          i
!      (2) compute  s = - sum C(j,k) v(k)   and
!                        k=j+1
!
!      (3) v(j) := s / C(j,j)
!
!  Step 2 is sometimes called the "dot product" step, since it is an
!  inner product between the j-th row and the portion of the eigenvector
!  that has been computed so far.
!
!  The "columnwise" method consists basically in doing the sums
!  for all the rows in parallel.  As each v(j) is computed, the
!  contribution of v(j) times the j-th column of C is added to the
!  partial sums.  Since FORTRAN arrays are stored columnwise, this has
!  the advantage that at each step, the elements of C that are accessed
!  are adjacent to one another, whereas with the rowwise method, the
!  elements accessed at a step are spaced LDA (and LDB) words apart.
!
!  When finding left eigenvectors, the matrix in question is the
!  transpose of the one in storage, so the rowwise method then
!  actually accesses columns of A and B at each step, and so is the
!  preferred method.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, SAFETY
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, &
                         SAFETY = 1.0D+2 )
!     ..
!     .. Local Scalars ..
      LOGICAL            COMPL, COMPR, IL2BY2, ILABAD, ILALL, ILBACK, &
                         ILBBAD, ILCOMP, ILCPLX, LSA, LSB
      INTEGER            I, IBEG, IEIG, IEND, IHWMNY, IINFO, IM, ISIDE, &
                         J, JA, JC, JE, JR, JW, NA, NW
      DOUBLE PRECISION   ACOEF, ACOEFA, ANORM, ASCALE, BCOEFA, BCOEFI, &
                         BCOEFR, BIG, BIGNUM, BNORM, BSCALE, CIM2A, &
                         CIM2B, CIMAGA, CIMAGB, CRE2A, CRE2B, CREALA, &
                         CREALB, DMIN, SAFMIN, SALFAR, SBETA, SCALE, &
                         SMALL, TEMP, TEMP2, TEMP2I, TEMP2R, ULP, XMAX, &
                         XSCALE
!     ..
!     .. Local Arrays ..
      DOUBLE PRECISION   BDIAG( 2 ), SUM( 2, 2 ), SUMA( 2, 2 ), &
                         SUMB( 2, 2 )
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, DLAMCH
!     ..
!     .. External Subroutines ..
      EXTERNAL           DGEMV, DLACPY, DLAG2, DLALN2, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
!     ..
!     .. Executable Statements ..
!
!     Decode and Test the input parameters
!
      if ( LSAME( HOWMNY, 'A' ) ) then
         IHWMNY = 1
         ILALL = .TRUE.
         ILBACK = .FALSE.
      else if ( LSAME( HOWMNY, 'S' ) ) then
         IHWMNY = 2
         ILALL = .FALSE.
         ILBACK = .FALSE.
      else if ( LSAME( HOWMNY, 'B' ) .OR. LSAME( HOWMNY, 'T' ) ) then
         IHWMNY = 3
         ILALL = .TRUE.
         ILBACK = .TRUE.
      ELSE
         IHWMNY = -1
         ILALL = .TRUE.
      end if
!
      if ( LSAME( SIDE, 'R' ) ) then
         ISIDE = 1
         COMPL = .FALSE.
         COMPR = .TRUE.
      else if ( LSAME( SIDE, 'L' ) ) then
         ISIDE = 2
         COMPL = .TRUE.
         COMPR = .FALSE.
      else if ( LSAME( SIDE, 'B' ) ) then
         ISIDE = 3
         COMPL = .TRUE.
         COMPR = .TRUE.
      ELSE
         ISIDE = -1
      end if
!
      INFO = 0
      if ( ISIDE < 0 ) then
         INFO = -1
      else if ( IHWMNY < 0 ) then
         INFO = -2
      else if ( N < 0 ) then
         INFO = -4
      else if ( LDA < MAX( 1, N ) ) then
         INFO = -6
      else if ( LDB < MAX( 1, N ) ) then
         INFO = -8
      end if
      if ( INFO.NE.0 ) then
         CALL XERBLA( 'DTGEVC', -INFO )
         RETURN
      end if
!
!     Count the number of eigenvectors to be computed
!
      if ( .NOT.ILALL ) then
         IM = 0
         ILCPLX = .FALSE.
         DO 10 J = 1, N
            if ( ILCPLX ) then
               ILCPLX = .FALSE.
               GO TO 10
            end if
            if ( J < N ) then
               if ( A( J+1, J ).NE.ZERO ) &
                  ILCPLX = .TRUE.
            end if
            if ( ILCPLX ) then
               if ( SELECT( J ) .OR. SELECT( J+1 ) ) &
                  IM = IM + 2
            ELSE
               if ( SELECT( J ) ) &
                  IM = IM + 1
            end if
   10    CONTINUE
      ELSE
         IM = N
      end if
!
!     Check 2-by-2 diagonal blocks of A, B
!
      ILABAD = .FALSE.
      ILBBAD = .FALSE.
      DO 20 J = 1, N - 1
         if ( A( J+1, J ).NE.ZERO ) then
            if ( B( J, J ) == ZERO .OR. B( J+1, J+1 ).EQ.ZERO .OR. &
                B( J, J+1 ).NE.ZERO )ILBBAD = .TRUE.
            if ( J < N-1 ) then
               if ( A( J+2, J+1 ).NE.ZERO ) &
                  ILABAD = .TRUE.
            end if
         end if
   20 CONTINUE
!
      if ( ILABAD ) then
         INFO = -5
      else if ( ILBBAD ) then
         INFO = -7
      else if ( COMPL .AND. LDVL < N .OR. LDVL.LT.1 ) then
         INFO = -10
      else if ( COMPR .AND. LDVR < N .OR. LDVR.LT.1 ) then
         INFO = -12
      else if ( MM < IM ) then
         INFO = -13
      end if
      if ( INFO.NE.0 ) then
         CALL XERBLA( 'DTGEVC', -INFO )
         RETURN
      end if
!
!     Quick return if possible
!
      M = IM
      if ( N == 0 ) &
         RETURN
!
!     Machine Constants
!
      SAFMIN = DLAMCH( 'Safe minimum' )
      BIG = ONE / SAFMIN
      CALL DLABAD( SAFMIN, BIG )
      ULP = DLAMCH( 'Epsilon' )*DLAMCH( 'Base' )
      SMALL = SAFMIN*N / ULP
      BIG = ONE / SMALL
      BIGNUM = ONE / ( SAFMIN*N )
!
!     Compute the 1-norm of each column of the strictly upper triangular
!     part (i.e., excluding all elements belonging to the diagonal
!     blocks) of A and B to check for possible overflow in the
!     triangular solver.
!
      ANORM = ABS( A( 1, 1 ) )
      if ( N.GT.1 ) &
         ANORM = ANORM + ABS( A( 2, 1 ) )
      BNORM = ABS( B( 1, 1 ) )
      WORK( 1 ) = ZERO
      WORK( N+1 ) = ZERO
!
      DO 50 J = 2, N
         TEMP = ZERO
         TEMP2 = ZERO
         if ( A( J, J-1 ) == ZERO ) then
            IEND = J - 1
         ELSE
            IEND = J - 2
         end if
         DO 30 I = 1, IEND
            TEMP = TEMP + ABS( A( I, J ) )
            TEMP2 = TEMP2 + ABS( B( I, J ) )
   30    CONTINUE
         WORK( J ) = TEMP
         WORK( N+J ) = TEMP2
         DO 40 I = IEND + 1, MIN( J+1, N )
            TEMP = TEMP + ABS( A( I, J ) )
            TEMP2 = TEMP2 + ABS( B( I, J ) )
   40    CONTINUE
         ANORM = MAX( ANORM, TEMP )
         BNORM = MAX( BNORM, TEMP2 )
   50 CONTINUE
!
      ASCALE = ONE / MAX( ANORM, SAFMIN )
      BSCALE = ONE / MAX( BNORM, SAFMIN )
!
!     Left eigenvectors
!
      if ( COMPL ) then
         IEIG = 0
!
!        Main loop over eigenvalues
!
         ILCPLX = .FALSE.
         DO 220 JE = 1, N
!
!           Skip this iteration if (a) HOWMNY='S' and SELECT=.FALSE., or
!           (b) this would be the second of a complex pair.
!           Check for complex eigenvalue, so as to be sure of which
!           entry(-ies) of SELECT to look at.
!
            if ( ILCPLX ) then
               ILCPLX = .FALSE.
               GO TO 220
            end if
            NW = 1
            if ( JE < N ) then
               if ( A( JE+1, JE ).NE.ZERO ) then
                  ILCPLX = .TRUE.
                  NW = 2
               end if
            end if
            if ( ILALL ) then
               ILCOMP = .TRUE.
            else if ( ILCPLX ) then
               ILCOMP = SELECT( JE ) .OR. SELECT( JE+1 )
            ELSE
               ILCOMP = SELECT( JE )
            end if
            if ( .NOT.ILCOMP ) &
               GO TO 220
!
!           Decide if (a) singular pencil, (b) real eigenvalue, or
!           (c) complex eigenvalue.
!
            if ( .NOT.ILCPLX ) then
               if ( ABS( A( JE, JE ) ).LE.SAFMIN .AND. &
                   ABS( B( JE, JE ) ).LE.SAFMIN ) then
!
!                 Singular matrix pencil -- return unit eigenvector
!
                  IEIG = IEIG + 1
                  DO 60 JR = 1, N
                     VL( JR, IEIG ) = ZERO
   60             CONTINUE
                  VL( IEIG, IEIG ) = ONE
                  GO TO 220
               end if
            end if
!
!           Clear vector
!
            DO 70 JR = 1, NW*N
               WORK( 2*N+JR ) = ZERO
   70       CONTINUE
!                                                 T
!           Compute coefficients in  ( a A - b B )  y = 0
!              a  is  ACOEF
!              b  is  BCOEFR + i*BCOEFI
!
            if ( .NOT.ILCPLX ) then
!
!              Real eigenvalue
!
               TEMP = ONE / MAX( ABS( A( JE, JE ) )*ASCALE, &
                      ABS( B( JE, JE ) )*BSCALE, SAFMIN )
               SALFAR = ( TEMP*A( JE, JE ) )*ASCALE
               SBETA = ( TEMP*B( JE, JE ) )*BSCALE
               ACOEF = SBETA*ASCALE
               BCOEFR = SALFAR*BSCALE
               BCOEFI = ZERO
!
!              Scale to avoid underflow
!
               SCALE = ONE
               LSA = ABS( SBETA ).GE.SAFMIN .AND. ABS( ACOEF ) < SMALL
               LSB = ABS( SALFAR ).GE.SAFMIN .AND. ABS( BCOEFR ) <  &
                     SMALL
               if ( LSA ) &
                  SCALE = ( SMALL / ABS( SBETA ) )*MIN( ANORM, BIG )
               if ( LSB ) &
                  SCALE = MAX( SCALE, ( SMALL / ABS( SALFAR ) )* &
                          MIN( BNORM, BIG ) )
               if ( LSA .OR. LSB ) then
                  SCALE = MIN( SCALE, ONE / &
                          ( SAFMIN*MAX( ONE, ABS( ACOEF ), &
                          ABS( BCOEFR ) ) ) )
                  if ( LSA ) then
                     ACOEF = ASCALE*( SCALE*SBETA )
                  ELSE
                     ACOEF = SCALE*ACOEF
                  end if
                  if ( LSB ) then
                     BCOEFR = BSCALE*( SCALE*SALFAR )
                  ELSE
                     BCOEFR = SCALE*BCOEFR
                  end if
               end if
               ACOEFA = ABS( ACOEF )
               BCOEFA = ABS( BCOEFR )
!
!              First component is 1
!
               WORK( 2*N+JE ) = ONE
               XMAX = ONE
            ELSE
!
!              Complex eigenvalue
!
               CALL DLAG2( A( JE, JE ), LDA, B( JE, JE ), LDB, &
                           SAFMIN*SAFETY, ACOEF, TEMP, BCOEFR, TEMP2, &
                           BCOEFI )
               BCOEFI = -BCOEFI
               if ( BCOEFI == ZERO ) then
                  INFO = JE
                  RETURN
               end if
!
!              Scale to avoid over/underflow
!
               ACOEFA = ABS( ACOEF )
               BCOEFA = ABS( BCOEFR ) + ABS( BCOEFI )
               SCALE = ONE
               if ( ACOEFA*ULP < SAFMIN .AND. ACOEFA.GE.SAFMIN ) &
                  SCALE = ( SAFMIN / ULP ) / ACOEFA
               if ( BCOEFA*ULP < SAFMIN .AND. BCOEFA.GE.SAFMIN ) &
                  SCALE = MAX( SCALE, ( SAFMIN / ULP ) / BCOEFA )
               if ( SAFMIN*ACOEFA.GT.ASCALE ) &
                  SCALE = ASCALE / ( SAFMIN*ACOEFA )
               if ( SAFMIN*BCOEFA.GT.BSCALE ) &
                  SCALE = MIN( SCALE, BSCALE / ( SAFMIN*BCOEFA ) )
               if ( SCALE.NE.ONE ) then
                  ACOEF = SCALE*ACOEF
                  ACOEFA = ABS( ACOEF )
                  BCOEFR = SCALE*BCOEFR
                  BCOEFI = SCALE*BCOEFI
                  BCOEFA = ABS( BCOEFR ) + ABS( BCOEFI )
               end if
!
!              Compute first two components of eigenvector
!
               TEMP = ACOEF*A( JE+1, JE )
               TEMP2R = ACOEF*A( JE, JE ) - BCOEFR*B( JE, JE )
               TEMP2I = -BCOEFI*B( JE, JE )
               if ( ABS( TEMP ).GT.ABS( TEMP2R )+ABS( TEMP2I ) ) then
                  WORK( 2*N+JE ) = ONE
                  WORK( 3*N+JE ) = ZERO
                  WORK( 2*N+JE+1 ) = -TEMP2R / TEMP
                  WORK( 3*N+JE+1 ) = -TEMP2I / TEMP
               ELSE
                  WORK( 2*N+JE+1 ) = ONE
                  WORK( 3*N+JE+1 ) = ZERO
                  TEMP = ACOEF*A( JE, JE+1 )
                  WORK( 2*N+JE ) = ( BCOEFR*B( JE+1, JE+1 )-ACOEF* &
                                   A( JE+1, JE+1 ) ) / TEMP
                  WORK( 3*N+JE ) = BCOEFI*B( JE+1, JE+1 ) / TEMP
               end if
               XMAX = MAX( ABS( WORK( 2*N+JE ) )+ABS( WORK( 3*N+JE ) ), &
                      ABS( WORK( 2*N+JE+1 ) )+ABS( WORK( 3*N+JE+1 ) ) )
            end if
!
            DMIN = MAX( ULP*ACOEFA*ANORM, ULP*BCOEFA*BNORM, SAFMIN )
!
!                                           T
!           Triangular solve of  (a A - b B)  y = 0
!
!                                   T
!           (rowwise in  (a A - b B) , or columnwise in (a A - b B) )
!
            IL2BY2 = .FALSE.
!
            DO 160 J = JE + NW, N
               if ( IL2BY2 ) then
                  IL2BY2 = .FALSE.
                  GO TO 160
               end if
!
               NA = 1
               BDIAG( 1 ) = B( J, J )
               if ( J < N ) then
                  if ( A( J+1, J ).NE.ZERO ) then
                     IL2BY2 = .TRUE.
                     BDIAG( 2 ) = B( J+1, J+1 )
                     NA = 2
                  end if
               end if
!
!              Check whether scaling is necessary for dot products
!
               XSCALE = ONE / MAX( ONE, XMAX )
               TEMP = MAX( WORK( J ), WORK( N+J ), &
                      ACOEFA*WORK( J )+BCOEFA*WORK( N+J ) )
               if ( IL2BY2 ) &
                  TEMP = MAX( TEMP, WORK( J+1 ), WORK( N+J+1 ), &
                         ACOEFA*WORK( J+1 )+BCOEFA*WORK( N+J+1 ) )
               if ( TEMP.GT.BIGNUM*XSCALE ) then
                  DO 90 JW = 0, NW - 1
                     DO 80 JR = JE, J - 1
                        WORK( ( JW+2 )*N+JR ) = XSCALE* &
                           WORK( ( JW+2 )*N+JR )
   80                CONTINUE
   90             CONTINUE
                  XMAX = XMAX*XSCALE
               end if
!
!              Compute dot products
!
!                    j-1
!              SUM = sum  conjg( a*A(k,j) - b*B(k,j) )*x(k)
!                    k=je
!
!              To reduce the op count, this is done as
!
!              _        j-1                  _        j-1
!              a*conjg( sum  A(k,j)*x(k) ) - b*conjg( sum  B(k,j)*x(k) )
!                       k=je                          k=je
!
!              which may cause underflow problems if A or B are close
!              to underflow.  (E.g., less than SMALL.)
!
!
!              A series of compiler directives to defeat vectorization
!              for the next loop
!
!$PL$ CMCHAR=' '
!DIR$          NEXTSCALAR
!$DIR          SCALAR
!DIR$          NEXT SCALAR
!VD$L          NOVECTOR
!DEC$          NOVECTOR
!VD$           NOVECTOR
!VDIR          NOVECTOR
!VOCL          LOOP,SCALAR
!IBM           PREFER SCALAR
!$PL$ CMCHAR='*'
!
               DO 120 JW = 1, NW
!
!$PL$ CMCHAR=' '
!DIR$             NEXTSCALAR
!$DIR             SCALAR
!DIR$             NEXT SCALAR
!VD$L             NOVECTOR
!DEC$             NOVECTOR
!VD$              NOVECTOR
!VDIR             NOVECTOR
!VOCL             LOOP,SCALAR
!IBM              PREFER SCALAR
!$PL$ CMCHAR='*'
!
                  DO 110 JA = 1, NA
                     SUMA( JA, JW ) = ZERO
                     SUMB( JA, JW ) = ZERO
!
                     DO 100 JR = JE, J - 1
                        SUMA( JA, JW ) = SUMA( JA, JW ) + &
                                         A( JR, J+JA-1 )* &
                                         WORK( ( JW+1 )*N+JR )
                        SUMB( JA, JW ) = SUMB( JA, JW ) + &
                                         B( JR, J+JA-1 )* &
                                         WORK( ( JW+1 )*N+JR )
  100                CONTINUE
  110             CONTINUE
  120          CONTINUE
!
!$PL$ CMCHAR=' '
!DIR$          NEXTSCALAR
!$DIR          SCALAR
!DIR$          NEXT SCALAR
!VD$L          NOVECTOR
!DEC$          NOVECTOR
!VD$           NOVECTOR
!VDIR          NOVECTOR
!VOCL          LOOP,SCALAR
!IBM           PREFER SCALAR
!$PL$ CMCHAR='*'
!
               DO 130 JA = 1, NA
                  if ( ILCPLX ) then
                     SUM( JA, 1 ) = -ACOEF*SUMA( JA, 1 ) + &
                                    BCOEFR*SUMB( JA, 1 ) - &
                                    BCOEFI*SUMB( JA, 2 )
                     SUM( JA, 2 ) = -ACOEF*SUMA( JA, 2 ) + &
                                    BCOEFR*SUMB( JA, 2 ) + &
                                    BCOEFI*SUMB( JA, 1 )
                  ELSE
                     SUM( JA, 1 ) = -ACOEF*SUMA( JA, 1 ) + &
                                    BCOEFR*SUMB( JA, 1 )
                  end if
  130          CONTINUE
!
!                                  T
!              Solve  ( a A - b B )  y = SUM(,)
!              with scaling and perturbation of the denominator
!
               CALL DLALN2( .TRUE., NA, NW, DMIN, ACOEF, A( J, J ), LDA, &
                            BDIAG( 1 ), BDIAG( 2 ), SUM, 2, BCOEFR, &
                            BCOEFI, WORK( 2*N+J ), N, SCALE, TEMP, &
                            IINFO )
               if ( SCALE < ONE ) then
                  DO 150 JW = 0, NW - 1
                     DO 140 JR = JE, J - 1
                        WORK( ( JW+2 )*N+JR ) = SCALE* &
                           WORK( ( JW+2 )*N+JR )
  140                CONTINUE
  150             CONTINUE
                  XMAX = SCALE*XMAX
               end if
               XMAX = MAX( XMAX, TEMP )
  160       CONTINUE
!
!           Copy eigenvector to VL, back transforming if
!           HOWMNY='B'.
!
            IEIG = IEIG + 1
            if ( ILBACK ) then
               DO 170 JW = 0, NW - 1
                  CALL DGEMV( 'N', N, N+1-JE, ONE, VL( 1, JE ), LDVL, &
                              WORK( ( JW+2 )*N+JE ), 1, ZERO, &
                              WORK( ( JW+4 )*N+1 ), 1 )
  170          CONTINUE
               CALL DLACPY( ' ', N, NW, WORK( 4*N+1 ), N, VL( 1, JE ), &
                            LDVL )
               IBEG = 1
            ELSE
               CALL DLACPY( ' ', N, NW, WORK( 2*N+1 ), N, VL( 1, IEIG ), &
                            LDVL )
               IBEG = JE
            end if
!
!           Scale eigenvector
!
            XMAX = ZERO
            if ( ILCPLX ) then
               DO 180 J = IBEG, N
                  XMAX = MAX( XMAX, ABS( VL( J, IEIG ) )+ &
                         ABS( VL( J, IEIG+1 ) ) )
  180          CONTINUE
            ELSE
               DO 190 J = IBEG, N
                  XMAX = MAX( XMAX, ABS( VL( J, IEIG ) ) )
  190          CONTINUE
            end if
!
            if ( XMAX.GT.SAFMIN ) then
               XSCALE = ONE / XMAX
!
               DO 210 JW = 0, NW - 1
                  DO 200 JR = IBEG, N
                     VL( JR, IEIG+JW ) = XSCALE*VL( JR, IEIG+JW )
  200             CONTINUE
  210          CONTINUE
            end if
            IEIG = IEIG + NW - 1
!
  220    CONTINUE
      end if
!
!     Right eigenvectors
!
      if ( COMPR ) then
         IEIG = IM + 1
!
!        Main loop over eigenvalues
!
         ILCPLX = .FALSE.
         DO 500 JE = N, 1, -1
!
!           Skip this iteration if (a) HOWMNY='S' and SELECT=.FALSE., or
!           (b) this would be the second of a complex pair.
!           Check for complex eigenvalue, so as to be sure of which
!           entry(-ies) of SELECT to look at -- if complex, SELECT(JE)
!           or SELECT(JE-1).
!           If this is a complex pair, the 2-by-2 diagonal block
!           corresponding to the eigenvalue is in rows/columns JE-1:JE
!
            if ( ILCPLX ) then
               ILCPLX = .FALSE.
               GO TO 500
            end if
            NW = 1
            if ( JE.GT.1 ) then
               if ( A( JE, JE-1 ).NE.ZERO ) then
                  ILCPLX = .TRUE.
                  NW = 2
               end if
            end if
            if ( ILALL ) then
               ILCOMP = .TRUE.
            else if ( ILCPLX ) then
               ILCOMP = SELECT( JE ) .OR. SELECT( JE-1 )
            ELSE
               ILCOMP = SELECT( JE )
            end if
            if ( .NOT.ILCOMP ) &
               GO TO 500
!
!           Decide if (a) singular pencil, (b) real eigenvalue, or
!           (c) complex eigenvalue.
!
            if ( .NOT.ILCPLX ) then
               if ( ABS( A( JE, JE ) ).LE.SAFMIN .AND. &
                   ABS( B( JE, JE ) ).LE.SAFMIN ) then
!
!                 Singular matrix pencil -- unit eigenvector
!
                  IEIG = IEIG - 1
                  DO 230 JR = 1, N
                     VR( JR, IEIG ) = ZERO
  230             CONTINUE
                  VR( IEIG, IEIG ) = ONE
                  GO TO 500
               end if
            end if
!
!           Clear vector
!
            DO 250 JW = 0, NW - 1
               DO 240 JR = 1, N
                  WORK( ( JW+2 )*N+JR ) = ZERO
  240          CONTINUE
  250       CONTINUE
!
!           Compute coefficients in  ( a A - b B ) x = 0
!              a  is  ACOEF
!              b  is  BCOEFR + i*BCOEFI
!
            if ( .NOT.ILCPLX ) then
!
!              Real eigenvalue
!
               TEMP = ONE / MAX( ABS( A( JE, JE ) )*ASCALE, &
                      ABS( B( JE, JE ) )*BSCALE, SAFMIN )
               SALFAR = ( TEMP*A( JE, JE ) )*ASCALE
               SBETA = ( TEMP*B( JE, JE ) )*BSCALE
               ACOEF = SBETA*ASCALE
               BCOEFR = SALFAR*BSCALE
               BCOEFI = ZERO
!
!              Scale to avoid underflow
!
               SCALE = ONE
               LSA = ABS( SBETA ).GE.SAFMIN .AND. ABS( ACOEF ) < SMALL
               LSB = ABS( SALFAR ).GE.SAFMIN .AND. ABS( BCOEFR ) <  &
                     SMALL
               if ( LSA ) &
                  SCALE = ( SMALL / ABS( SBETA ) )*MIN( ANORM, BIG )
               if ( LSB ) &
                  SCALE = MAX( SCALE, ( SMALL / ABS( SALFAR ) )* &
                          MIN( BNORM, BIG ) )
               if ( LSA .OR. LSB ) then
                  SCALE = MIN( SCALE, ONE / &
                          ( SAFMIN*MAX( ONE, ABS( ACOEF ), &
                          ABS( BCOEFR ) ) ) )
                  if ( LSA ) then
                     ACOEF = ASCALE*( SCALE*SBETA )
                  ELSE
                     ACOEF = SCALE*ACOEF
                  end if
                  if ( LSB ) then
                     BCOEFR = BSCALE*( SCALE*SALFAR )
                  ELSE
                     BCOEFR = SCALE*BCOEFR
                  end if
               end if
               ACOEFA = ABS( ACOEF )
               BCOEFA = ABS( BCOEFR )
!
!              First component is 1
!
               WORK( 2*N+JE ) = ONE
               XMAX = ONE
!
!              Compute contribution from column JE of A and B to sum
!              (See "Further Details", above.)
!
               DO 260 JR = 1, JE - 1
                  WORK( 2*N+JR ) = BCOEFR*B( JR, JE ) - &
                                   ACOEF*A( JR, JE )
  260          CONTINUE
            ELSE
!
!              Complex eigenvalue
!
               CALL DLAG2( A( JE-1, JE-1 ), LDA, B( JE-1, JE-1 ), LDB, &
                           SAFMIN*SAFETY, ACOEF, TEMP, BCOEFR, TEMP2, &
                           BCOEFI )
               if ( BCOEFI == ZERO ) then
                  INFO = JE - 1
                  RETURN
               end if
!
!              Scale to avoid over/underflow
!
               ACOEFA = ABS( ACOEF )
               BCOEFA = ABS( BCOEFR ) + ABS( BCOEFI )
               SCALE = ONE
               if ( ACOEFA*ULP < SAFMIN .AND. ACOEFA.GE.SAFMIN ) &
                  SCALE = ( SAFMIN / ULP ) / ACOEFA
               if ( BCOEFA*ULP < SAFMIN .AND. BCOEFA.GE.SAFMIN ) &
                  SCALE = MAX( SCALE, ( SAFMIN / ULP ) / BCOEFA )
               if ( SAFMIN*ACOEFA.GT.ASCALE ) &
                  SCALE = ASCALE / ( SAFMIN*ACOEFA )
               if ( SAFMIN*BCOEFA.GT.BSCALE ) &
                  SCALE = MIN( SCALE, BSCALE / ( SAFMIN*BCOEFA ) )
               if ( SCALE.NE.ONE ) then
                  ACOEF = SCALE*ACOEF
                  ACOEFA = ABS( ACOEF )
                  BCOEFR = SCALE*BCOEFR
                  BCOEFI = SCALE*BCOEFI
                  BCOEFA = ABS( BCOEFR ) + ABS( BCOEFI )
               end if
!
!              Compute first two components of eigenvector
!              and contribution to sums
!
               TEMP = ACOEF*A( JE, JE-1 )
               TEMP2R = ACOEF*A( JE, JE ) - BCOEFR*B( JE, JE )
               TEMP2I = -BCOEFI*B( JE, JE )
               if ( ABS( TEMP ).GE.ABS( TEMP2R )+ABS( TEMP2I ) ) then
                  WORK( 2*N+JE ) = ONE
                  WORK( 3*N+JE ) = ZERO
                  WORK( 2*N+JE-1 ) = -TEMP2R / TEMP
                  WORK( 3*N+JE-1 ) = -TEMP2I / TEMP
               ELSE
                  WORK( 2*N+JE-1 ) = ONE
                  WORK( 3*N+JE-1 ) = ZERO
                  TEMP = ACOEF*A( JE-1, JE )
                  WORK( 2*N+JE ) = ( BCOEFR*B( JE-1, JE-1 )-ACOEF* &
                                   A( JE-1, JE-1 ) ) / TEMP
                  WORK( 3*N+JE ) = BCOEFI*B( JE-1, JE-1 ) / TEMP
               end if
!
               XMAX = MAX( ABS( WORK( 2*N+JE ) )+ABS( WORK( 3*N+JE ) ), &
                      ABS( WORK( 2*N+JE-1 ) )+ABS( WORK( 3*N+JE-1 ) ) )
!
!              Compute contribution from columns JE and JE-1
!              of A and B to the sums.
!
               CREALA = ACOEF*WORK( 2*N+JE-1 )
               CIMAGA = ACOEF*WORK( 3*N+JE-1 )
               CREALB = BCOEFR*WORK( 2*N+JE-1 ) - &
                        BCOEFI*WORK( 3*N+JE-1 )
               CIMAGB = BCOEFI*WORK( 2*N+JE-1 ) + &
                        BCOEFR*WORK( 3*N+JE-1 )
               CRE2A = ACOEF*WORK( 2*N+JE )
               CIM2A = ACOEF*WORK( 3*N+JE )
               CRE2B = BCOEFR*WORK( 2*N+JE ) - BCOEFI*WORK( 3*N+JE )
               CIM2B = BCOEFI*WORK( 2*N+JE ) + BCOEFR*WORK( 3*N+JE )
               DO 270 JR = 1, JE - 2
                  WORK( 2*N+JR ) = -CREALA*A( JR, JE-1 ) + &
                                   CREALB*B( JR, JE-1 ) - &
                                   CRE2A*A( JR, JE ) + CRE2B*B( JR, JE )
                  WORK( 3*N+JR ) = -CIMAGA*A( JR, JE-1 ) + &
                                   CIMAGB*B( JR, JE-1 ) - &
                                   CIM2A*A( JR, JE ) + CIM2B*B( JR, JE )
  270          CONTINUE
            end if
!
            DMIN = MAX( ULP*ACOEFA*ANORM, ULP*BCOEFA*BNORM, SAFMIN )
!
!           Columnwise triangular solve of  (a A - b B)  x = 0
!
            IL2BY2 = .FALSE.
            DO 370 J = JE - NW, 1, -1
!
!              If a 2-by-2 block, is in position j-1:j, wait until
!              next iteration to process it (when it will be j:j+1)
!
               if ( .NOT.IL2BY2 .AND. J.GT.1 ) then
                  if ( A( J, J-1 ).NE.ZERO ) then
                     IL2BY2 = .TRUE.
                     GO TO 370
                  end if
               end if
               BDIAG( 1 ) = B( J, J )
               if ( IL2BY2 ) then
                  NA = 2
                  BDIAG( 2 ) = B( J+1, J+1 )
               ELSE
                  NA = 1
               end if
!
!              Compute x(j) (and x(j+1), if 2-by-2 block)
!
               CALL DLALN2( .FALSE., NA, NW, DMIN, ACOEF, A( J, J ), &
                            LDA, BDIAG( 1 ), BDIAG( 2 ), WORK( 2*N+J ), &
                            N, BCOEFR, BCOEFI, SUM, 2, SCALE, TEMP, &
                            IINFO )
               if ( SCALE < ONE ) then
!
                  DO 290 JW = 0, NW - 1
                     DO 280 JR = 1, JE
                        WORK( ( JW+2 )*N+JR ) = SCALE* &
                           WORK( ( JW+2 )*N+JR )
  280                CONTINUE
  290             CONTINUE
               end if
               XMAX = MAX( SCALE*XMAX, TEMP )
!
               DO 310 JW = 1, NW
                  DO 300 JA = 1, NA
                     WORK( ( JW+1 )*N+J+JA-1 ) = SUM( JA, JW )
  300             CONTINUE
  310          CONTINUE
!
!              w = w + x(j)*(a A(*,j) - b B(*,j) ) with scaling
!
               if ( J.GT.1 ) then
!
!                 Check whether scaling is necessary for sum.
!
                  XSCALE = ONE / MAX( ONE, XMAX )
                  TEMP = ACOEFA*WORK( J ) + BCOEFA*WORK( N+J )
                  if ( IL2BY2 ) &
                     TEMP = MAX( TEMP, ACOEFA*WORK( J+1 )+BCOEFA* &
                            WORK( N+J+1 ) )
                  TEMP = MAX( TEMP, ACOEFA, BCOEFA )
                  if ( TEMP.GT.BIGNUM*XSCALE ) then
!
                     DO 330 JW = 0, NW - 1
                        DO 320 JR = 1, JE
                           WORK( ( JW+2 )*N+JR ) = XSCALE* &
                              WORK( ( JW+2 )*N+JR )
  320                   CONTINUE
  330                CONTINUE
                     XMAX = XMAX*XSCALE
                  end if
!
!                 Compute the contributions of the off-diagonals of
!                 column j (and j+1, if 2-by-2 block) of A and B to the
!                 sums.
!
!
                  DO 360 JA = 1, NA
                     if ( ILCPLX ) then
                        CREALA = ACOEF*WORK( 2*N+J+JA-1 )
                        CIMAGA = ACOEF*WORK( 3*N+J+JA-1 )
                        CREALB = BCOEFR*WORK( 2*N+J+JA-1 ) - &
                                 BCOEFI*WORK( 3*N+J+JA-1 )
                        CIMAGB = BCOEFI*WORK( 2*N+J+JA-1 ) + &
                                 BCOEFR*WORK( 3*N+J+JA-1 )
                        DO 340 JR = 1, J - 1
                           WORK( 2*N+JR ) = WORK( 2*N+JR ) - &
                                            CREALA*A( JR, J+JA-1 ) + &
                                            CREALB*B( JR, J+JA-1 )
                           WORK( 3*N+JR ) = WORK( 3*N+JR ) - &
                                            CIMAGA*A( JR, J+JA-1 ) + &
                                            CIMAGB*B( JR, J+JA-1 )
  340                   CONTINUE
                     ELSE
                        CREALA = ACOEF*WORK( 2*N+J+JA-1 )
                        CREALB = BCOEFR*WORK( 2*N+J+JA-1 )
                        DO 350 JR = 1, J - 1
                           WORK( 2*N+JR ) = WORK( 2*N+JR ) - &
                                            CREALA*A( JR, J+JA-1 ) + &
                                            CREALB*B( JR, J+JA-1 )
  350                   CONTINUE
                     end if
  360             CONTINUE
               end if
!
               IL2BY2 = .FALSE.
  370       CONTINUE
!
!           Copy eigenvector to VR, back transforming if
!           HOWMNY='B'.
!
            IEIG = IEIG - NW
            if ( ILBACK ) then
!
               DO 410 JW = 0, NW - 1
                  DO 380 JR = 1, N
                     WORK( ( JW+4 )*N+JR ) = WORK( ( JW+2 )*N+1 )* &
                                             VR( JR, 1 )
  380             CONTINUE
!
!                 A series of compiler directives to defeat
!                 vectorization for the next loop
!
!
                  DO 400 JC = 2, JE
                     DO 390 JR = 1, N
                        WORK( ( JW+4 )*N+JR ) = WORK( ( JW+4 )*N+JR ) + &
                           WORK( ( JW+2 )*N+JC )*VR( JR, JC )
  390                CONTINUE
  400             CONTINUE
  410          CONTINUE
!
               DO 430 JW = 0, NW - 1
                  DO 420 JR = 1, N
                     VR( JR, IEIG+JW ) = WORK( ( JW+4 )*N+JR )
  420             CONTINUE
  430          CONTINUE
!
               IEND = N
            ELSE
               DO 450 JW = 0, NW - 1
                  DO 440 JR = 1, N
                     VR( JR, IEIG+JW ) = WORK( ( JW+2 )*N+JR )
  440             CONTINUE
  450          CONTINUE
!
               IEND = JE
            end if
!
!           Scale eigenvector
!
            XMAX = ZERO
            if ( ILCPLX ) then
               DO 460 J = 1, IEND
                  XMAX = MAX( XMAX, ABS( VR( J, IEIG ) )+ &
                         ABS( VR( J, IEIG+1 ) ) )
  460          CONTINUE
            ELSE
               DO 470 J = 1, IEND
                  XMAX = MAX( XMAX, ABS( VR( J, IEIG ) ) )
  470          CONTINUE
            end if
!
            if ( XMAX.GT.SAFMIN ) then
               XSCALE = ONE / XMAX
               DO 490 JW = 0, NW - 1
                  DO 480 JR = 1, IEND
                     VR( JR, IEIG+JW ) = XSCALE*VR( JR, IEIG+JW )
  480             CONTINUE
  490          CONTINUE
            end if
  500    CONTINUE
      end if
!
      RETURN
!
!     End of DTGEVC
!
      END
