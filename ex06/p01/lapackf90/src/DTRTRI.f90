      SUBROUTINE DTRTRI( UPLO, DIAG, N, A, LDA, INFO )
!
!  -- LAPACK routine (version 3.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     March 31, 1993
!
!     .. Scalar Arguments ..
      CHARACTER          DIAG, UPLO
      INTEGER            INFO, LDA, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  DTRTRI computes the inverse of a real upper or lower triangular
!  matrix A.
!
!  This is the Level 3 BLAS version of the algorithm.
!
!  Arguments
!  =========
!
!  UPLO    (input) CHARACTER*1
!          = 'U':  A is upper triangular;
!          = 'L':  A is lower triangular.
!
!  DIAG    (input) CHARACTER*1
!          = 'N':  A is non-unit triangular;
!          = 'U':  A is unit triangular.
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the triangular matrix A.  If UPLO = 'U', the
!          leading N-by-N upper triangular part of the array A contains
!          the upper triangular matrix, and the strictly lower
!          triangular part of A is not referenced.  If UPLO = 'L', the
!          leading N-by-N lower triangular part of the array A contains
!          the lower triangular matrix, and the strictly upper
!          triangular part of A is not referenced.  If DIAG = 'U', the
!          diagonal elements of A are also not referenced and are
!          assumed to be 1.
!          On exit, the (triangular) inverse of the original matrix, in
!          the same storage format.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).
!
!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -i, the i-th argument had an illegal value
!          > 0: if INFO = i, A(i,i) is exactly zero.  The triangular
!               matrix is singular and its inverse can not be computed.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            NOUNIT, UPPER
      INTEGER            J, JB, NB, NN
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
!     ..
!     .. External Subroutines ..
      EXTERNAL           DTRMM, DTRSM, DTRTI2, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      NOUNIT = LSAME( DIAG, 'N' )
      if ( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) then
         INFO = -1
      else if ( .NOT.NOUNIT .AND. .NOT.LSAME( DIAG, 'U' ) ) then
         INFO = -2
      else if ( N < 0 ) then
         INFO = -3
      else if ( LDA < MAX( 1, N ) ) then
         INFO = -5
      end if
      if ( INFO.NE.0 ) then
         CALL XERBLA( 'DTRTRI', -INFO )
         RETURN
      end if
!
!     Quick return if possible
!
      if ( N == 0 ) &
         RETURN
!
!     Check for singularity if non-unit.
!
      if ( NOUNIT ) then
         DO 10 INFO = 1, N
            if ( A( INFO, INFO ) == ZERO ) &
               RETURN
   10    CONTINUE
         INFO = 0
      end if
!
!     Determine the block size for this environment.
!
      NB = ILAENV( 1, 'DTRTRI', UPLO // DIAG, N, -1, -1, -1 )
      if ( NB.LE.1 .OR. NB.GE.N ) then
!
!        Use unblocked code
!
         CALL DTRTI2( UPLO, DIAG, N, A, LDA, INFO )
      ELSE
!
!        Use blocked code
!
         if ( UPPER ) then
!
!           Compute inverse of upper triangular matrix
!
            DO 20 J = 1, N, NB
               JB = MIN( NB, N-J+1 )
!
!              Compute rows 1:j-1 of current block column
!
               CALL DTRMM( 'Left', 'Upper', 'No transpose', DIAG, J-1, &
                           JB, ONE, A, LDA, A( 1, J ), LDA )
               CALL DTRSM( 'Right', 'Upper', 'No transpose', DIAG, J-1, &
                           JB, -ONE, A( J, J ), LDA, A( 1, J ), LDA )
!
!              Compute inverse of current diagonal block
!
               CALL DTRTI2( 'Upper', DIAG, JB, A( J, J ), LDA, INFO )
   20       CONTINUE
         ELSE
!
!           Compute inverse of lower triangular matrix
!
            NN = ( ( N-1 ) / NB )*NB + 1
            DO 30 J = NN, 1, -NB
               JB = MIN( NB, N-J+1 )
               if ( J+JB.LE.N ) then
!
!                 Compute rows j+jb:n of current block column
!
                  CALL DTRMM( 'Left', 'Lower', 'No transpose', DIAG, &
                              N-J-JB+1, JB, ONE, A( J+JB, J+JB ), LDA, &
                              A( J+JB, J ), LDA )
                  CALL DTRSM( 'Right', 'Lower', 'No transpose', DIAG, &
                              N-J-JB+1, JB, -ONE, A( J, J ), LDA, &
                              A( J+JB, J ), LDA )
               end if
!
!              Compute inverse of current diagonal block
!
               CALL DTRTI2( 'Lower', DIAG, JB, A( J, J ), LDA, INFO )
   30       CONTINUE
         end if
      end if
!
      RETURN
!
!     End of DTRTRI
!
      END
