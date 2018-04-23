      SUBROUTINE DGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT, &
                         WORK, LWORK, INFO )
!
!  -- LAPACK driver routine (version 3.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     October 31, 1999
!
!     .. Scalar Arguments ..
      CHARACTER          JOBU, JOBVT
      INTEGER            INFO, LDA, LDU, LDVT, LWORK, M, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), S( * ), U( LDU, * ), &
                         VT( LDVT, * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  DGESVD computes the singular value decomposition (SVD) of a real
!  M-by-N matrix A, optionally computing the left and/or right singular
!  vectors. The SVD is written
!
!       A = U * SIGMA * transpose(V)
!
!  where SIGMA is an M-by-N matrix which is zero except for its
!  min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
!  V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
!  are the singular values of A; they are real and non-negative, and
!  are returned in descending order.  The first min(m,n) columns of
!  U and V are the left and right singular vectors of A.
!
!  Note that the routine returns V**T, not V.
!
!  Arguments
!  =========
!
!  JOBU    (input) CHARACTER*1
!          Specifies options for computing all or part of the matrix U:
!          = 'A':  all M columns of U are returned in array U:
!          = 'S':  the first min(m,n) columns of U (the left singular
!                  vectors) are returned in the array U;
!          = 'O':  the first min(m,n) columns of U (the left singular
!                  vectors) are overwritten on the array A;
!          = 'N':  no columns of U (no left singular vectors) are
!                  computed.
!
!  JOBVT   (input) CHARACTER*1
!          Specifies options for computing all or part of the matrix
!          V**T:
!          = 'A':  all N rows of V**T are returned in the array VT;
!          = 'S':  the first min(m,n) rows of V**T (the right singular
!                  vectors) are returned in the array VT;
!          = 'O':  the first min(m,n) rows of V**T (the right singular
!                  vectors) are overwritten on the array A;
!          = 'N':  no rows of V**T (no right singular vectors) are
!                  computed.
!
!          JOBVT and JOBU cannot both be 'O'.
!
!  M       (input) INTEGER
!          The number of rows of the input matrix A.  M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the input matrix A.  N >= 0.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the M-by-N matrix A.
!          On exit,
!          if JOBU = 'O',  A is overwritten with the first min(m,n)
!                          columns of U (the left singular vectors,
!                          stored columnwise);
!          if JOBVT = 'O', A is overwritten with the first min(m,n)
!                          rows of V**T (the right singular vectors,
!                          stored rowwise);
!          if JOBU .ne. 'O' and JOBVT .ne. 'O', the contents of A
!                          are destroyed.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,M).
!
!  S       (output) DOUBLE PRECISION array, dimension (min(M,N))
!          The singular values of A, sorted so that S(i) >= S(i+1).
!
!  U       (output) DOUBLE PRECISION array, dimension (LDU,UCOL)
!          (LDU,M) if JOBU = 'A' or (LDU,min(M,N)) if JOBU = 'S'.
!          If JOBU = 'A', U contains the M-by-M orthogonal matrix U;
!          if JOBU = 'S', U contains the first min(m,n) columns of U
!          (the left singular vectors, stored columnwise);
!          if JOBU = 'N' or 'O', U is not referenced.
!
!  LDU     (input) INTEGER
!          The leading dimension of the array U.  LDU >= 1; if
!          JOBU = 'S' or 'A', LDU >= M.
!
!  VT      (output) DOUBLE PRECISION array, dimension (LDVT,N)
!          If JOBVT = 'A', VT contains the N-by-N orthogonal matrix
!          V**T;
!          if JOBVT = 'S', VT contains the first min(m,n) rows of
!          V**T (the right singular vectors, stored rowwise);
!          if JOBVT = 'N' or 'O', VT is not referenced.
!
!  LDVT    (input) INTEGER
!          The leading dimension of the array VT.  LDVT >= 1; if
!          JOBVT = 'A', LDVT >= N; if JOBVT = 'S', LDVT >= min(M,N).
!
!  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
!          On exit, if INFO = 0, WORK(1) returns the optimal LWORK;
!          if INFO > 0, WORK(2:MIN(M,N)) contains the unconverged
!          superdiagonal elements of an upper bidiagonal matrix B
!          whose diagonal is in S (not necessarily sorted). B
!          satisfies A = U * B * VT, so it has the same singular values
!          as A, and singular vectors related by U and VT.
!
!  LWORK   (input) INTEGER
!          The dimension of the array WORK. LWORK >= 1.
!          LWORK >= MAX(3*MIN(M,N)+MAX(M,N),5*MIN(M,N)).
!          For good performance, LWORK should generally be larger.
!
!          If LWORK = -1, then a workspace query is assumed; the routine
!          only calculates the optimal size of the WORK array, returns
!          this value as the first entry of the WORK array, and no error
!          message related to LWORK is issued by XERBLA.
!
!  INFO    (output) INTEGER
!          = 0:  successful exit.
!          < 0:  if INFO = -i, the i-th argument had an illegal value.
!          > 0:  if DBDSQR did not converge, INFO specifies how many
!                superdiagonals of an intermediate bidiagonal form B
!                did not converge to zero. See the description of WORK
!                above for details.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            LQUERY, WNTUA, WNTUAS, WNTUN, WNTUO, WNTUS, &
                         WNTVA, WNTVAS, WNTVN, WNTVO, WNTVS
      INTEGER            BDSPAC, BLK, CHUNK, I, IE, IERR, IR, ISCL, &
                         ITAU, ITAUP, ITAUQ, IU, IWORK, LDWRKR, LDWRKU, &
                         MAXWRK, MINMN, MINWRK, MNTHR, NCU, NCVT, NRU, &
                         NRVT, WRKBL
      DOUBLE PRECISION   ANRM, BIGNUM, EPS, SMLNUM
!     ..
!     .. Local Arrays ..
      DOUBLE PRECISION   DUM( 1 )
!     ..
!     .. External Subroutines ..
      EXTERNAL           DBDSQR, DGEBRD, DGELQF, DGEMM, DGEQRF, DLACPY, &
                         DLASCL, DLASET, DORGBR, DORGLQ, DORGQR, DORMBR, &
                         XERBLA
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           LSAME, ILAENV, DLAMCH, DLANGE
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, SQRT
!     ..
!     .. Executable Statements ..
!
!     Test the input arguments
!
      INFO = 0
      MINMN = MIN( M, N )
      MNTHR = ILAENV( 6, 'DGESVD', JOBU // JOBVT, M, N, 0, 0 )
      WNTUA = LSAME( JOBU, 'A' )
      WNTUS = LSAME( JOBU, 'S' )
      WNTUAS = WNTUA .OR. WNTUS
      WNTUO = LSAME( JOBU, 'O' )
      WNTUN = LSAME( JOBU, 'N' )
      WNTVA = LSAME( JOBVT, 'A' )
      WNTVS = LSAME( JOBVT, 'S' )
      WNTVAS = WNTVA .OR. WNTVS
      WNTVO = LSAME( JOBVT, 'O' )
      WNTVN = LSAME( JOBVT, 'N' )
      MINWRK = 1
      LQUERY = ( LWORK == -1 )
!
      if ( .NOT.( WNTUA .OR. WNTUS .OR. WNTUO .OR. WNTUN ) ) then
         INFO = -1
      else if ( .NOT.( WNTVA .OR. WNTVS .OR. WNTVO .OR. WNTVN ) .OR. &
               ( WNTVO .AND. WNTUO ) ) then
         INFO = -2
      else if ( M < 0 ) then
         INFO = -3
      else if ( N < 0 ) then
         INFO = -4
      else if ( LDA < MAX( 1, M ) ) then
         INFO = -6
      else if ( LDU < 1 .OR. ( WNTUAS .AND. LDU.LT.M ) ) then
         INFO = -9
      else if ( LDVT < 1 .OR. ( WNTVA .AND. LDVT.LT.N ) .OR. &
               ( WNTVS .AND. LDVT < MINMN ) ) then
         INFO = -11
      end if
!
!     Compute workspace
!      (Note: Comments in the code beginning "Workspace:" describe the
!       minimal amount of workspace needed at that point in the code,
!       as well as the preferred amount for good performance.
!       NB refers to the optimal block size for the immediately
!       following subroutine, as returned by ILAENV.)
!
      if ( INFO == 0 .AND. ( LWORK.GE.1 .OR. LQUERY ) .AND. M.GT.0 .AND. &
          N.GT.0 ) then
         if ( M.GE.N ) then
!
!           Compute space needed for DBDSQR
!
            BDSPAC = 5*N
            if ( M.GE.MNTHR ) then
               if ( WNTUN ) then
!
!                 Path 1 (M much larger than N, JOBU='N')
!
                  MAXWRK = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, &
                           -1 )
                  MAXWRK = MAX( MAXWRK, 3*N+2*N* &
                           ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  if ( WNTVO .OR. WNTVAS ) &
                     MAXWRK = MAX( MAXWRK, 3*N+( N-1 )* &
                              ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 ) )
                  MAXWRK = MAX( MAXWRK, BDSPAC )
                  MINWRK = MAX( 4*N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               else if ( WNTUO .AND. WNTVN ) then
!
!                 Path 2 (M much larger than N, JOBU='O', JOBVT='N')
!
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M, &
                          N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N* &
                          ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N* &
                          ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = MAX( N*N+WRKBL, N*N+M*N+N )
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               else if ( WNTUO .AND. WNTVAS ) then
!
!                 Path 3 (M much larger than N, JOBU='O', JOBVT='S' or
!                 'A')
!
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M, &
                          N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N* &
                          ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N* &
                          ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )* &
                          ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = MAX( N*N+WRKBL, N*N+M*N+N )
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               else if ( WNTUS .AND. WNTVN ) then
!
!                 Path 4 (M much larger than N, JOBU='S', JOBVT='N')
!
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M, &
                          N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N* &
                          ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N* &
                          ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               else if ( WNTUS .AND. WNTVO ) then
!
!                 Path 5 (M much larger than N, JOBU='S', JOBVT='O')
!
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M, &
                          N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N* &
                          ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N* &
                          ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )* &
                          ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = 2*N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               else if ( WNTUS .AND. WNTVAS ) then
!
!                 Path 6 (M much larger than N, JOBU='S', JOBVT='S' or
!                 'A')
!
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M, &
                          N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N* &
                          ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N* &
                          ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )* &
                          ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               else if ( WNTUA .AND. WNTVN ) then
!
!                 Path 7 (M much larger than N, JOBU='A', JOBVT='N')
!
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+M*ILAENV( 1, 'DORGQR', ' ', M, &
                          M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N* &
                          ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N* &
                          ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               else if ( WNTUA .AND. WNTVO ) then
!
!                 Path 8 (M much larger than N, JOBU='A', JOBVT='O')
!
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+M*ILAENV( 1, 'DORGQR', ' ', M, &
                          M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N* &
                          ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N* &
                          ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )* &
                          ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = 2*N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               else if ( WNTUA .AND. WNTVAS ) then
!
!                 Path 9 (M much larger than N, JOBU='A', JOBVT='S' or
!                 'A')
!
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+M*ILAENV( 1, 'DORGQR', ' ', M, &
                          M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N* &
                          ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N* &
                          ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )* &
                          ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               end if
            ELSE
!
!              Path 10 (M at least N, but not much larger)
!
               MAXWRK = 3*N + ( M+N )*ILAENV( 1, 'DGEBRD', ' ', M, N, &
                        -1, -1 )
               if ( WNTUS .OR. WNTUO ) &
                  MAXWRK = MAX( MAXWRK, 3*N+N* &
                           ILAENV( 1, 'DORGBR', 'Q', M, N, N, -1 ) )
               if ( WNTUA ) &
                  MAXWRK = MAX( MAXWRK, 3*N+M* &
                           ILAENV( 1, 'DORGBR', 'Q', M, M, N, -1 ) )
               if ( .NOT.WNTVN ) &
                  MAXWRK = MAX( MAXWRK, 3*N+( N-1 )* &
                           ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 ) )
               MAXWRK = MAX( MAXWRK, BDSPAC )
               MINWRK = MAX( 3*N+M, BDSPAC )
               MAXWRK = MAX( MAXWRK, MINWRK )
            end if
         ELSE
!
!           Compute space needed for DBDSQR
!
            BDSPAC = 5*M
            if ( N.GE.MNTHR ) then
               if ( WNTVN ) then
!
!                 Path 1t(N much larger than M, JOBVT='N')
!
                  MAXWRK = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, &
                           -1 )
                  MAXWRK = MAX( MAXWRK, 3*M+2*M* &
                           ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  if ( WNTUO .OR. WNTUAS ) &
                     MAXWRK = MAX( MAXWRK, 3*M+M* &
                              ILAENV( 1, 'DORGBR', 'Q', M, M, M, -1 ) )
                  MAXWRK = MAX( MAXWRK, BDSPAC )
                  MINWRK = MAX( 4*M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               else if ( WNTVO .AND. WNTUN ) then
!
!                 Path 2t(N much larger than M, JOBU='N', JOBVT='O')
!
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M, &
                          N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M* &
                          ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )* &
                          ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = MAX( M*M+WRKBL, M*M+M*N+M )
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               else if ( WNTVO .AND. WNTUAS ) then
!
!                 Path 3t(N much larger than M, JOBU='S' or 'A',
!                 JOBVT='O')
!
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M, &
                          N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M* &
                          ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )* &
                          ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M* &
                          ILAENV( 1, 'DORGBR', 'Q', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = MAX( M*M+WRKBL, M*M+M*N+M )
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               else if ( WNTVS .AND. WNTUN ) then
!
!                 Path 4t(N much larger than M, JOBU='N', JOBVT='S')
!
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M, &
                          N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M* &
                          ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )* &
                          ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               else if ( WNTVS .AND. WNTUO ) then
!
!                 Path 5t(N much larger than M, JOBU='O', JOBVT='S')
!
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M, &
                          N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M* &
                          ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )* &
                          ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M* &
                          ILAENV( 1, 'DORGBR', 'Q', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = 2*M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               else if ( WNTVS .AND. WNTUAS ) then
!
!                 Path 6t(N much larger than M, JOBU='S' or 'A',
!                 JOBVT='S')
!
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M, &
                          N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M* &
                          ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )* &
                          ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M* &
                          ILAENV( 1, 'DORGBR', 'Q', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               else if ( WNTVA .AND. WNTUN ) then
!
!                 Path 7t(N much larger than M, JOBU='N', JOBVT='A')
!
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+N*ILAENV( 1, 'DORGLQ', ' ', N, &
                          N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M* &
                          ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )* &
                          ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               else if ( WNTVA .AND. WNTUO ) then
!
!                 Path 8t(N much larger than M, JOBU='O', JOBVT='A')
!
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+N*ILAENV( 1, 'DORGLQ', ' ', N, &
                          N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M* &
                          ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )* &
                          ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M* &
                          ILAENV( 1, 'DORGBR', 'Q', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = 2*M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               else if ( WNTVA .AND. WNTUAS ) then
!
!                 Path 9t(N much larger than M, JOBU='S' or 'A',
!                 JOBVT='A')
!
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+N*ILAENV( 1, 'DORGLQ', ' ', N, &
                          N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M* &
                          ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )* &
                          ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M* &
                          ILAENV( 1, 'DORGBR', 'Q', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               end if
            ELSE
!
!              Path 10t(N greater than M, but not much larger)
!
               MAXWRK = 3*M + ( M+N )*ILAENV( 1, 'DGEBRD', ' ', M, N, &
                        -1, -1 )
               if ( WNTVS .OR. WNTVO ) &
                  MAXWRK = MAX( MAXWRK, 3*M+M* &
                           ILAENV( 1, 'DORGBR', 'P', M, N, M, -1 ) )
               if ( WNTVA ) &
                  MAXWRK = MAX( MAXWRK, 3*M+N* &
                           ILAENV( 1, 'DORGBR', 'P', N, N, M, -1 ) )
               if ( .NOT.WNTUN ) &
                  MAXWRK = MAX( MAXWRK, 3*M+( M-1 )* &
                           ILAENV( 1, 'DORGBR', 'Q', M, M, M, -1 ) )
               MAXWRK = MAX( MAXWRK, BDSPAC )
               MINWRK = MAX( 3*M+N, BDSPAC )
               MAXWRK = MAX( MAXWRK, MINWRK )
            end if
         end if
         WORK( 1 ) = MAXWRK
      end if
!
      if ( LWORK < MINWRK .AND. .NOT.LQUERY ) then
         write ( *, * ) 'DGESVD - DEBUG'
         write ( *, * ) '  MINWRK = ', minwrk
         write ( *, * ) '  LWORK =  ', lwork
         INFO = -13
      end if

      if ( INFO.NE.0 ) then
         CALL XERBLA( 'DGESVD', -INFO )
         RETURN
      else if ( LQUERY ) then
         RETURN
      end if
!
!     Quick return if possible
!
      if ( M == 0 .OR. N.EQ.0 ) then
         if ( LWORK.GE.1 ) &
            WORK( 1 ) = ONE
         RETURN
      end if
!
!     Get machine constants
!
      EPS = DLAMCH( 'P' )
      SMLNUM = SQRT( DLAMCH( 'S' ) ) / EPS
      BIGNUM = ONE / SMLNUM
!
!     Scale A if max element outside range [SMLNUM,BIGNUM]
!
      ANRM = DLANGE( 'M', M, N, A, LDA, DUM )
      ISCL = 0
      if ( ANRM.GT.ZERO .AND. ANRM < SMLNUM ) then
         ISCL = 1
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, IERR )
      else if ( ANRM.GT.BIGNUM ) then
         ISCL = 1
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, IERR )
      end if
!
      if ( M.GE.N ) then
!
!        A has at least as many rows as columns. If A has sufficiently
!        more rows than columns, first reduce using the QR
!        decomposition (if sufficient workspace available)
!
         if ( M.GE.MNTHR ) then
!
            if ( WNTUN ) then
!
!              Path 1 (M much larger than N, JOBU='N')
!              No left singular vectors to be computed
!
               ITAU = 1
               IWORK = ITAU + N
!
!              Compute A=Q*R
!              (Workspace: need 2*N, prefer N+N*NB)
!
               CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), WORK( IWORK ), &
                            LWORK-IWORK+1, IERR )
!
!              Zero out below R
!
               CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ), LDA )
               IE = 1
               ITAUQ = IE + N
               ITAUP = ITAUQ + N
               IWORK = ITAUP + N
!
!              Bidiagonalize R in A
!              (Workspace: need 4*N, prefer 3*N+2*N*NB)
!
               CALL DGEBRD( N, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ), &
                            WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1, &
                            IERR )
               NCVT = 0
               if ( WNTVO .OR. WNTVAS ) then
!
!                 If right singular vectors desired, generate P'.
!                 (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
!
                  CALL DORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
                  NCVT = N
               end if
               IWORK = IE + N
!
!              Perform bidiagonal QR iteration, computing right
!              singular vectors of A in A if desired
!              (Workspace: need BDSPAC)
!
               CALL DBDSQR( 'U', N, NCVT, 0, 0, S, WORK( IE ), A, LDA, &
                            DUM, 1, DUM, 1, WORK( IWORK ), INFO )
!
!              If right singular vectors desired in VT, copy them there
!
               if ( WNTVAS ) &
                  CALL DLACPY( 'F', N, N, A, LDA, VT, LDVT )
!
            else if ( WNTUO .AND. WNTVN ) then
!
!              Path 2 (M much larger than N, JOBU='O', JOBVT='N')
!              N left singular vectors to be overwritten on A and
!              no right singular vectors to be computed
!
               if ( LWORK.GE.N*N+MAX( 4*N, BDSPAC ) ) then
!
!                 Sufficient workspace for a fast algorithm
!
                  IR = 1
                  if ( LWORK.GE.MAX( WRKBL, LDA*N+N )+LDA*N ) then
!
!                    WORK(IU) is LDA by N, WORK(IR) is LDA by N
!
                     LDWRKU = LDA
                     LDWRKR = LDA
                  else if ( LWORK.GE.MAX( WRKBL, LDA*N+N )+N*N ) then
!
!                    WORK(IU) is LDA by N, WORK(IR) is N by N
!
                     LDWRKU = LDA
                     LDWRKR = N
                  ELSE
!
!                    WORK(IU) is LDWRKU by N, WORK(IR) is N by N
!
                     LDWRKU = ( LWORK-N*N-N ) / N
                     LDWRKR = N
                  end if
                  ITAU = IR + LDWRKR*N
                  IWORK = ITAU + N
!
!                 Compute A=Q*R
!                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
!
                  CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                 Copy R to WORK(IR) and zero out below it
!
                  CALL DLACPY( 'U', N, N, A, LDA, WORK( IR ), LDWRKR )
                  CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, WORK( IR+1 ), &
                               LDWRKR )
!
!                 Generate Q in A
!                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
!
                  CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + N
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
!
!                 Bidiagonalize R in WORK(IR)
!                 (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
!
                  CALL DGEBRD( N, N, WORK( IR ), LDWRKR, S, WORK( IE ), &
                               WORK( ITAUQ ), WORK( ITAUP ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                 Generate left vectors bidiagonalizing R
!                 (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
!
                  CALL DORGBR( 'Q', N, N, N, WORK( IR ), LDWRKR, &
                               WORK( ITAUQ ), WORK( IWORK ), &
                               LWORK-IWORK+1, IERR )
                  IWORK = IE + N
!
!                 Perform bidiagonal QR iteration, computing left
!                 singular vectors of R in WORK(IR)
!                 (Workspace: need N*N+BDSPAC)
!
                  CALL DBDSQR( 'U', N, 0, N, 0, S, WORK( IE ), DUM, 1, &
                               WORK( IR ), LDWRKR, DUM, 1, &
                               WORK( IWORK ), INFO )
                  IU = IE + N
!
!                 Multiply Q in A by left singular vectors of R in
!                 WORK(IR), storing result in WORK(IU) and copying to A
!                 (Workspace: need N*N+2*N, prefer N*N+M*N+N)
!
                  DO I = 1, M, LDWRKU
                     CHUNK = MIN( M-I+1, LDWRKU )
                     CALL DGEMM( 'N', 'N', CHUNK, N, N, ONE, A( I, 1 ), &
                                 LDA, WORK( IR ), LDWRKR, ZERO, &
                                 WORK( IU ), LDWRKU )
                     CALL DLACPY( 'F', CHUNK, N, WORK( IU ), LDWRKU, &
                                  A( I, 1 ), LDA )
                  end do
!
               ELSE
!
!                 Insufficient workspace for a fast algorithm
!
                  IE = 1
                  ITAUQ = IE + N
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
!
!                 Bidiagonalize A
!                 (Workspace: need 3*N+M, prefer 3*N+(M+N)*NB)
!
                  CALL DGEBRD( M, N, A, LDA, S, WORK( IE ), &
                               WORK( ITAUQ ), WORK( ITAUP ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                 Generate left vectors bidiagonalizing A
!                 (Workspace: need 4*N, prefer 3*N+N*NB)
!
                  CALL DORGBR( 'Q', M, N, N, A, LDA, WORK( ITAUQ ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + N
!
!                 Perform bidiagonal QR iteration, computing left
!                 singular vectors of A in A
!                 (Workspace: need BDSPAC)
!
                  CALL DBDSQR( 'U', N, 0, M, 0, S, WORK( IE ), DUM, 1, &
                               A, LDA, DUM, 1, WORK( IWORK ), INFO )
!
               end if
!
            else if ( WNTUO .AND. WNTVAS ) then
!
!              Path 3 (M much larger than N, JOBU='O', JOBVT='S' or 'A')
!              N left singular vectors to be overwritten on A and
!              N right singular vectors to be computed in VT
!
               if ( LWORK.GE.N*N+MAX( 4*N, BDSPAC ) ) then
!
!                 Sufficient workspace for a fast algorithm
!
                  IR = 1
                  if ( LWORK.GE.MAX( WRKBL, LDA*N+N )+LDA*N ) then
!
!                    WORK(IU) is LDA by N and WORK(IR) is LDA by N
!
                     LDWRKU = LDA
                     LDWRKR = LDA
                  else if ( LWORK.GE.MAX( WRKBL, LDA*N+N )+N*N ) then
!
!                    WORK(IU) is LDA by N and WORK(IR) is N by N
!
                     LDWRKU = LDA
                     LDWRKR = N
                  ELSE
!
!                    WORK(IU) is LDWRKU by N and WORK(IR) is N by N
!
                     LDWRKU = ( LWORK-N*N-N ) / N
                     LDWRKR = N
                  end if
                  ITAU = IR + LDWRKR*N
                  IWORK = ITAU + N
!
!                 Compute A=Q*R
!                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
!
                  CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                 Copy R to VT, zeroing out below it
!
                  CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
                  CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, VT( 2, 1 ), &
                               LDVT )
!
!                 Generate Q in A
!                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
!
                  CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + N
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
!
!                 Bidiagonalize R in VT, copying result to WORK(IR)
!                 (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
!
                  CALL DGEBRD( N, N, VT, LDVT, S, WORK( IE ), &
                               WORK( ITAUQ ), WORK( ITAUP ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
                  CALL DLACPY( 'L', N, N, VT, LDVT, WORK( IR ), LDWRKR )
!
!                 Generate left vectors bidiagonalizing R in WORK(IR)
!                 (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
!
                  CALL DORGBR( 'Q', N, N, N, WORK( IR ), LDWRKR, &
                               WORK( ITAUQ ), WORK( IWORK ), &
                               LWORK-IWORK+1, IERR )
!
!                 Generate right vectors bidiagonalizing R in VT
!                 (Workspace: need N*N+4*N-1, prefer N*N+3*N+(N-1)*NB)
!
                  CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + N
!
!                 Perform bidiagonal QR iteration, computing left
!                 singular vectors of R in WORK(IR) and computing right
!                 singular vectors of R in VT
!                 (Workspace: need N*N+BDSPAC)
!
                  CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ), VT, LDVT, &
                               WORK( IR ), LDWRKR, DUM, 1, &
                               WORK( IWORK ), INFO )
                  IU = IE + N
!
!                 Multiply Q in A by left singular vectors of R in
!                 WORK(IR), storing result in WORK(IU) and copying to A
!                 (Workspace: need N*N+2*N, prefer N*N+M*N+N)
!
                  DO I = 1, M, LDWRKU
                     CHUNK = MIN( M-I+1, LDWRKU )
                     CALL DGEMM( 'N', 'N', CHUNK, N, N, ONE, A( I, 1 ), &
                                 LDA, WORK( IR ), LDWRKR, ZERO, &
                                 WORK( IU ), LDWRKU )
                     CALL DLACPY( 'F', CHUNK, N, WORK( IU ), LDWRKU, &
                                  A( I, 1 ), LDA )
                  end do
!
               ELSE
!
!                 Insufficient workspace for a fast algorithm
!
                  ITAU = 1
                  IWORK = ITAU + N
!
!                 Compute A=Q*R
!                 (Workspace: need 2*N, prefer N+N*NB)
!
                  CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                 Copy R to VT, zeroing out below it
!
                  CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
                  CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, VT( 2, 1 ), &
                               LDVT )
!
!                 Generate Q in A
!                 (Workspace: need 2*N, prefer N+N*NB)
!
                  CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + N
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
!
!                 Bidiagonalize R in VT
!                 (Workspace: need 4*N, prefer 3*N+2*N*NB)
!
                  CALL DGEBRD( N, N, VT, LDVT, S, WORK( IE ), &
                               WORK( ITAUQ ), WORK( ITAUP ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                 Multiply Q in A by left vectors bidiagonalizing R
!                 (Workspace: need 3*N+M, prefer 3*N+M*NB)
!
                  CALL DORMBR( 'Q', 'R', 'N', M, N, N, VT, LDVT, &
                               WORK( ITAUQ ), A, LDA, WORK( IWORK ), &
                               LWORK-IWORK+1, IERR )
!
!                 Generate right vectors bidiagonalizing R in VT
!                 (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
!
                  CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + N
!
!                 Perform bidiagonal QR iteration, computing left
!                 singular vectors of A in A and computing right
!                 singular vectors of A in VT
!                 (Workspace: need BDSPAC)
!
                  CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), VT, LDVT, &
                               A, LDA, DUM, 1, WORK( IWORK ), INFO )
!
               end if
!
            else if ( WNTUS ) then
!
               if ( WNTVN ) then
!
!                 Path 4 (M much larger than N, JOBU='S', JOBVT='N')
!                 N left singular vectors to be computed in U and
!                 no right singular vectors to be computed
!
                  if ( LWORK.GE.N*N+MAX( 4*N, BDSPAC ) ) then
!
!                    Sufficient workspace for a fast algorithm
!
                     IR = 1
                     if ( LWORK.GE.WRKBL+LDA*N ) then
!
!                       WORK(IR) is LDA by N
!
                        LDWRKR = LDA
                     ELSE
!
!                       WORK(IR) is N by N
!
                        LDWRKR = N
                     end if
                     ITAU = IR + LDWRKR*N
                     IWORK = ITAU + N
!
!                    Compute A=Q*R
!                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
!
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Copy R to WORK(IR), zeroing out below it
!
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IR ), &
                                  LDWRKR )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, &
                                  WORK( IR+1 ), LDWRKR )
!
!                    Generate Q in A
!                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
!
                     CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
!
!                    Bidiagonalize R in WORK(IR)
!                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
!
                     CALL DGEBRD( N, N, WORK( IR ), LDWRKR, S, &
                                  WORK( IE ), WORK( ITAUQ ), &
                                  WORK( ITAUP ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
!
!                    Generate left vectors bidiagonalizing R in WORK(IR)
!                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
!
                     CALL DORGBR( 'Q', N, N, N, WORK( IR ), LDWRKR, &
                                  WORK( ITAUQ ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
                     IWORK = IE + N
!
!                    Perform bidiagonal QR iteration, computing left
!                    singular vectors of R in WORK(IR)
!                    (Workspace: need N*N+BDSPAC)
!
                     CALL DBDSQR( 'U', N, 0, N, 0, S, WORK( IE ), DUM, &
                                  1, WORK( IR ), LDWRKR, DUM, 1, &
                                  WORK( IWORK ), INFO )
!
!                    Multiply Q in A by left singular vectors of R in
!                    WORK(IR), storing result in U
!                    (Workspace: need N*N)
!
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, A, LDA, &
                                 WORK( IR ), LDWRKR, ZERO, U, LDU )
!
                  ELSE
!
!                    Insufficient workspace for a fast algorithm
!
                     ITAU = 1
                     IWORK = ITAU + N
!
!                    Compute A=Q*R, copying result to U
!                    (Workspace: need 2*N, prefer N+N*NB)
!
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
!
!                    Generate Q in U
!                    (Workspace: need 2*N, prefer N+N*NB)
!
                     CALL DORGQR( M, N, N, U, LDU, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
!
!                    Zero out below R in A
!
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ), &
                                  LDA )
!
!                    Bidiagonalize R in A
!                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
!
                     CALL DGEBRD( N, N, A, LDA, S, WORK( IE ), &
                                  WORK( ITAUQ ), WORK( ITAUP ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Multiply Q in U by left vectors bidiagonalizing R
!                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
!
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, A, LDA, &
                                  WORK( ITAUQ ), U, LDU, WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
                     IWORK = IE + N
!
!                    Perform bidiagonal QR iteration, computing left
!                    singular vectors of A in U
!                    (Workspace: need BDSPAC)
!
                     CALL DBDSQR( 'U', N, 0, M, 0, S, WORK( IE ), DUM, &
                                  1, U, LDU, DUM, 1, WORK( IWORK ), &
                                  INFO )
!
                  end if
!
               else if ( WNTVO ) then
!
!                 Path 5 (M much larger than N, JOBU='S', JOBVT='O')
!                 N left singular vectors to be computed in U and
!                 N right singular vectors to be overwritten on A
!
                  if ( LWORK.GE.2*N*N+MAX( 4*N, BDSPAC ) ) then
!
!                    Sufficient workspace for a fast algorithm
!
                     IU = 1
                     if ( LWORK.GE.WRKBL+2*LDA*N ) then
!
!                       WORK(IU) is LDA by N and WORK(IR) is LDA by N
!
                        LDWRKU = LDA
                        IR = IU + LDWRKU*N
                        LDWRKR = LDA
                     else if ( LWORK.GE.WRKBL+( LDA+N )*N ) then
!
!                       WORK(IU) is LDA by N and WORK(IR) is N by N
!
                        LDWRKU = LDA
                        IR = IU + LDWRKU*N
                        LDWRKR = N
                     ELSE
!
!                       WORK(IU) is N by N and WORK(IR) is N by N
!
                        LDWRKU = N
                        IR = IU + LDWRKU*N
                        LDWRKR = N
                     end if
                     ITAU = IR + LDWRKR*N
                     IWORK = ITAU + N
!
!                    Compute A=Q*R
!                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
!
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Copy R to WORK(IU), zeroing out below it
!
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IU ), &
                                  LDWRKU )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, &
                                  WORK( IU+1 ), LDWRKU )
!
!                    Generate Q in A
!                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
!
                     CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
!
!                    Bidiagonalize R in WORK(IU), copying result to
!                    WORK(IR)
!                    (Workspace: need 2*N*N+4*N,
!                                prefer 2*N*N+3*N+2*N*NB)
!
                     CALL DGEBRD( N, N, WORK( IU ), LDWRKU, S, &
                                  WORK( IE ), WORK( ITAUQ ), &
                                  WORK( ITAUP ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', N, N, WORK( IU ), LDWRKU, &
                                  WORK( IR ), LDWRKR )
!
!                    Generate left bidiagonalizing vectors in WORK(IU)
!                    (Workspace: need 2*N*N+4*N, prefer 2*N*N+3*N+N*NB)
!
                     CALL DORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU, &
                                  WORK( ITAUQ ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
!
!                    Generate right bidiagonalizing vectors in WORK(IR)
!                    (Workspace: need 2*N*N+4*N-1,
!                                prefer 2*N*N+3*N+(N-1)*NB)
!
                     CALL DORGBR( 'P', N, N, N, WORK( IR ), LDWRKR, &
                                  WORK( ITAUP ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
                     IWORK = IE + N
!
!                    Perform bidiagonal QR iteration, computing left
!                    singular vectors of R in WORK(IU) and computing
!                    right singular vectors of R in WORK(IR)
!                    (Workspace: need 2*N*N+BDSPAC)
!
                     CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ), &
                                  WORK( IR ), LDWRKR, WORK( IU ), &
                                  LDWRKU, DUM, 1, WORK( IWORK ), INFO )
!
!                    Multiply Q in A by left singular vectors of R in
!                    WORK(IU), storing result in U
!                    (Workspace: need N*N)
!
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, A, LDA, &
                                 WORK( IU ), LDWRKU, ZERO, U, LDU )
!
!                    Copy right singular vectors of R to A
!                    (Workspace: need N*N)
!
                     CALL DLACPY( 'F', N, N, WORK( IR ), LDWRKR, A, &
                                  LDA )
!
                  ELSE
!
!                    Insufficient workspace for a fast algorithm
!
                     ITAU = 1
                     IWORK = ITAU + N
!
!                    Compute A=Q*R, copying result to U
!                    (Workspace: need 2*N, prefer N+N*NB)
!
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
!
!                    Generate Q in U
!                    (Workspace: need 2*N, prefer N+N*NB)
!
                     CALL DORGQR( M, N, N, U, LDU, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
!
!                    Zero out below R in A
!
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ), &
                                  LDA )
!
!                    Bidiagonalize R in A
!                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
!
                     CALL DGEBRD( N, N, A, LDA, S, WORK( IE ), &
                                  WORK( ITAUQ ), WORK( ITAUP ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Multiply Q in U by left vectors bidiagonalizing R
!                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
!
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, A, LDA, &
                                  WORK( ITAUQ ), U, LDU, WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
!
!                    Generate right vectors bidiagonalizing R in A
!                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
!
                     CALL DORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
!
!                    Perform bidiagonal QR iteration, computing left
!                    singular vectors of A in U and computing right
!                    singular vectors of A in A
!                    (Workspace: need BDSPAC)
!
                     CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), A, &
                                  LDA, U, LDU, DUM, 1, WORK( IWORK ), &
                                  INFO )
!
                  end if
!
               else if ( WNTVAS ) then
!
!                 Path 6 (M much larger than N, JOBU='S', JOBVT='S'
!                         or 'A')
!                 N left singular vectors to be computed in U and
!                 N right singular vectors to be computed in VT
!
                  if ( LWORK.GE.N*N+MAX( 4*N, BDSPAC ) ) then
!
!                    Sufficient workspace for a fast algorithm
!
                     IU = 1
                     if ( LWORK.GE.WRKBL+LDA*N ) then
!
!                       WORK(IU) is LDA by N
!
                        LDWRKU = LDA
                     ELSE
!
!                       WORK(IU) is N by N
!
                        LDWRKU = N
                     end if
                     ITAU = IU + LDWRKU*N
                     IWORK = ITAU + N
!
!                    Compute A=Q*R
!                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
!
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Copy R to WORK(IU), zeroing out below it
!
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IU ), &
                                  LDWRKU )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, &
                                  WORK( IU+1 ), LDWRKU )
!
!                    Generate Q in A
!                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
!
                     CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
!
!                    Bidiagonalize R in WORK(IU), copying result to VT
!                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
!
                     CALL DGEBRD( N, N, WORK( IU ), LDWRKU, S, &
                                  WORK( IE ), WORK( ITAUQ ), &
                                  WORK( ITAUP ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', N, N, WORK( IU ), LDWRKU, VT, &
                                  LDVT )
!
!                    Generate left bidiagonalizing vectors in WORK(IU)
!                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
!
                     CALL DORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU, &
                                  WORK( ITAUQ ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
!
!                    Generate right bidiagonalizing vectors in VT
!                    (Workspace: need N*N+4*N-1,
!                                prefer N*N+3*N+(N-1)*NB)
!
                     CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
!
!                    Perform bidiagonal QR iteration, computing left
!                    singular vectors of R in WORK(IU) and computing
!                    right singular vectors of R in VT
!                    (Workspace: need N*N+BDSPAC)
!
                     CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ), VT, &
                                  LDVT, WORK( IU ), LDWRKU, DUM, 1, &
                                  WORK( IWORK ), INFO )
!
!                    Multiply Q in A by left singular vectors of R in
!                    WORK(IU), storing result in U
!                    (Workspace: need N*N)
!
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, A, LDA, &
                                 WORK( IU ), LDWRKU, ZERO, U, LDU )
!
                  ELSE
!
!                    Insufficient workspace for a fast algorithm
!
                     ITAU = 1
                     IWORK = ITAU + N
!
!                    Compute A=Q*R, copying result to U
!                    (Workspace: need 2*N, prefer N+N*NB)
!
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
!
!                    Generate Q in U
!                    (Workspace: need 2*N, prefer N+N*NB)
!
                     CALL DORGQR( M, N, N, U, LDU, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Copy R to VT, zeroing out below it
!
                     CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, VT( 2, 1 ), &
                                  LDVT )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
!
!                    Bidiagonalize R in VT
!                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
!
                     CALL DGEBRD( N, N, VT, LDVT, S, WORK( IE ), &
                                  WORK( ITAUQ ), WORK( ITAUP ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Multiply Q in U by left bidiagonalizing vectors
!                    in VT
!                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
!
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, VT, LDVT, &
                                  WORK( ITAUQ ), U, LDU, WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
!
!                    Generate right bidiagonalizing vectors in VT
!                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
!
                     CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
!
!                    Perform bidiagonal QR iteration, computing left
!                    singular vectors of A in U and computing right
!                    singular vectors of A in VT
!                    (Workspace: need BDSPAC)
!
                     CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), VT, &
                                  LDVT, U, LDU, DUM, 1, WORK( IWORK ), &
                                  INFO )
!
                  end if
!
               end if
!
            else if ( WNTUA ) then
!
               if ( WNTVN ) then
!
!                 Path 7 (M much larger than N, JOBU='A', JOBVT='N')
!                 M left singular vectors to be computed in U and
!                 no right singular vectors to be computed
!
                  if ( LWORK.GE.N*N+MAX( N+M, 4*N, BDSPAC ) ) then
!
!                    Sufficient workspace for a fast algorithm
!
                     IR = 1
                     if ( LWORK.GE.WRKBL+LDA*N ) then
!
!                       WORK(IR) is LDA by N
!
                        LDWRKR = LDA
                     ELSE
!
!                       WORK(IR) is N by N
!
                        LDWRKR = N
                     end if
                     ITAU = IR + LDWRKR*N
                     IWORK = ITAU + N
!
!                    Compute A=Q*R, copying result to U
!                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
!
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
!
!                    Copy R to WORK(IR), zeroing out below it
!
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IR ), &
                                  LDWRKR )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, &
                                  WORK( IR+1 ), LDWRKR )
!
!                    Generate Q in U
!                    (Workspace: need N*N+N+M, prefer N*N+N+M*NB)
!
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
!
!                    Bidiagonalize R in WORK(IR)
!                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
!
                     CALL DGEBRD( N, N, WORK( IR ), LDWRKR, S, &
                                  WORK( IE ), WORK( ITAUQ ), &
                                  WORK( ITAUP ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
!
!                    Generate left bidiagonalizing vectors in WORK(IR)
!                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
!
                     CALL DORGBR( 'Q', N, N, N, WORK( IR ), LDWRKR, &
                                  WORK( ITAUQ ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
                     IWORK = IE + N
!
!                    Perform bidiagonal QR iteration, computing left
!                    singular vectors of R in WORK(IR)
!                    (Workspace: need N*N+BDSPAC)
!
                     CALL DBDSQR( 'U', N, 0, N, 0, S, WORK( IE ), DUM, &
                                  1, WORK( IR ), LDWRKR, DUM, 1, &
                                  WORK( IWORK ), INFO )
!
!                    Multiply Q in U by left singular vectors of R in
!                    WORK(IR), storing result in A
!                    (Workspace: need N*N)
!
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, U, LDU, &
                                 WORK( IR ), LDWRKR, ZERO, A, LDA )
!
!                    Copy left singular vectors of A from A to U
!
                     CALL DLACPY( 'F', M, N, A, LDA, U, LDU )
!
                  ELSE
!
!                    Insufficient workspace for a fast algorithm
!
                     ITAU = 1
                     IWORK = ITAU + N
!
!                    Compute A=Q*R, copying result to U
!                    (Workspace: need 2*N, prefer N+N*NB)
!
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
!
!                    Generate Q in U
!                    (Workspace: need N+M, prefer N+M*NB)
!
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
!
!                    Zero out below R in A
!
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ), &
                                  LDA )
!
!                    Bidiagonalize R in A
!                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
!
                     CALL DGEBRD( N, N, A, LDA, S, WORK( IE ), &
                                  WORK( ITAUQ ), WORK( ITAUP ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Multiply Q in U by left bidiagonalizing vectors
!                    in A
!                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
!
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, A, LDA, &
                                  WORK( ITAUQ ), U, LDU, WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
                     IWORK = IE + N
!
!                    Perform bidiagonal QR iteration, computing left
!                    singular vectors of A in U
!                    (Workspace: need BDSPAC)
!
                     CALL DBDSQR( 'U', N, 0, M, 0, S, WORK( IE ), DUM, &
                                  1, U, LDU, DUM, 1, WORK( IWORK ), &
                                  INFO )
!
                  end if
!
               else if ( WNTVO ) then
!
!                 Path 8 (M much larger than N, JOBU='A', JOBVT='O')
!                 M left singular vectors to be computed in U and
!                 N right singular vectors to be overwritten on A
!
                  if ( LWORK.GE.2*N*N+MAX( N+M, 4*N, BDSPAC ) ) then
!
!                    Sufficient workspace for a fast algorithm
!
                     IU = 1
                     if ( LWORK.GE.WRKBL+2*LDA*N ) then
!
!                       WORK(IU) is LDA by N and WORK(IR) is LDA by N
!
                        LDWRKU = LDA
                        IR = IU + LDWRKU*N
                        LDWRKR = LDA
                     else if ( LWORK.GE.WRKBL+( LDA+N )*N ) then
!
!                       WORK(IU) is LDA by N and WORK(IR) is N by N
!
                        LDWRKU = LDA
                        IR = IU + LDWRKU*N
                        LDWRKR = N
                     ELSE
!
!                       WORK(IU) is N by N and WORK(IR) is N by N
!
                        LDWRKU = N
                        IR = IU + LDWRKU*N
                        LDWRKR = N
                     end if
                     ITAU = IR + LDWRKR*N
                     IWORK = ITAU + N
!
!                    Compute A=Q*R, copying result to U
!                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
!
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
!
!                    Generate Q in U
!                    (Workspace: need 2*N*N+N+M, prefer 2*N*N+N+M*NB)
!
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Copy R to WORK(IU), zeroing out below it
!
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IU ), &
                                  LDWRKU )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, &
                                  WORK( IU+1 ), LDWRKU )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
!
!                    Bidiagonalize R in WORK(IU), copying result to
!                    WORK(IR)
!                    (Workspace: need 2*N*N+4*N,
!                                prefer 2*N*N+3*N+2*N*NB)
!
                     CALL DGEBRD( N, N, WORK( IU ), LDWRKU, S, &
                                  WORK( IE ), WORK( ITAUQ ), &
                                  WORK( ITAUP ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', N, N, WORK( IU ), LDWRKU, &
                                  WORK( IR ), LDWRKR )
!
!                    Generate left bidiagonalizing vectors in WORK(IU)
!                    (Workspace: need 2*N*N+4*N, prefer 2*N*N+3*N+N*NB)
!
                     CALL DORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU, &
                                  WORK( ITAUQ ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
!
!                    Generate right bidiagonalizing vectors in WORK(IR)
!                    (Workspace: need 2*N*N+4*N-1,
!                                prefer 2*N*N+3*N+(N-1)*NB)
!
                     CALL DORGBR( 'P', N, N, N, WORK( IR ), LDWRKR, &
                                  WORK( ITAUP ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
                     IWORK = IE + N
!
!                    Perform bidiagonal QR iteration, computing left
!                    singular vectors of R in WORK(IU) and computing
!                    right singular vectors of R in WORK(IR)
!                    (Workspace: need 2*N*N+BDSPAC)
!
                     CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ), &
                                  WORK( IR ), LDWRKR, WORK( IU ), &
                                  LDWRKU, DUM, 1, WORK( IWORK ), INFO )
!
!                    Multiply Q in U by left singular vectors of R in
!                    WORK(IU), storing result in A
!                    (Workspace: need N*N)
!
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, U, LDU, &
                                 WORK( IU ), LDWRKU, ZERO, A, LDA )
!
!                    Copy left singular vectors of A from A to U
!
                     CALL DLACPY( 'F', M, N, A, LDA, U, LDU )
!
!                    Copy right singular vectors of R from WORK(IR) to A
!
                     CALL DLACPY( 'F', N, N, WORK( IR ), LDWRKR, A, &
                                  LDA )
!
                  ELSE
!
!                    Insufficient workspace for a fast algorithm
!
                     ITAU = 1
                     IWORK = ITAU + N
!
!                    Compute A=Q*R, copying result to U
!                    (Workspace: need 2*N, prefer N+N*NB)
!
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
!
!                    Generate Q in U
!                    (Workspace: need N+M, prefer N+M*NB)
!
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
!
!                    Zero out below R in A
!
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ), &
                                  LDA )
!
!                    Bidiagonalize R in A
!                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
!
                     CALL DGEBRD( N, N, A, LDA, S, WORK( IE ), &
                                  WORK( ITAUQ ), WORK( ITAUP ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Multiply Q in U by left bidiagonalizing vectors
!                    in A
!                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
!
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, A, LDA, &
                                  WORK( ITAUQ ), U, LDU, WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
!
!                    Generate right bidiagonalizing vectors in A
!                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
!
                     CALL DORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
!
!                    Perform bidiagonal QR iteration, computing left
!                    singular vectors of A in U and computing right
!                    singular vectors of A in A
!                    (Workspace: need BDSPAC)
!
                     CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), A, &
                                  LDA, U, LDU, DUM, 1, WORK( IWORK ), &
                                  INFO )
!
                  end if
!
               else if ( WNTVAS ) then
!
!                 Path 9 (M much larger than N, JOBU='A', JOBVT='S'
!                         or 'A')
!                 M left singular vectors to be computed in U and
!                 N right singular vectors to be computed in VT
!
                  if ( LWORK.GE.N*N+MAX( N+M, 4*N, BDSPAC ) ) then
!
!                    Sufficient workspace for a fast algorithm
!
                     IU = 1
                     if ( LWORK.GE.WRKBL+LDA*N ) then
!
!                       WORK(IU) is LDA by N
!
                        LDWRKU = LDA
                     ELSE
!
!                       WORK(IU) is N by N
!
                        LDWRKU = N
                     end if
                     ITAU = IU + LDWRKU*N
                     IWORK = ITAU + N
!
!                    Compute A=Q*R, copying result to U
!                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
!
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
!
!                    Generate Q in U
!                    (Workspace: need N*N+N+M, prefer N*N+N+M*NB)
!
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Copy R to WORK(IU), zeroing out below it
!
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IU ), &
                                  LDWRKU )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, &
                                  WORK( IU+1 ), LDWRKU )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
!
!                    Bidiagonalize R in WORK(IU), copying result to VT
!                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
!
                     CALL DGEBRD( N, N, WORK( IU ), LDWRKU, S, &
                                  WORK( IE ), WORK( ITAUQ ), &
                                  WORK( ITAUP ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', N, N, WORK( IU ), LDWRKU, VT, &
                                  LDVT )
!
!                    Generate left bidiagonalizing vectors in WORK(IU)
!                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
!
                     CALL DORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU, &
                                  WORK( ITAUQ ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
!
!                    Generate right bidiagonalizing vectors in VT
!                    (Workspace: need N*N+4*N-1,
!                                prefer N*N+3*N+(N-1)*NB)
!
                     CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
!
!                    Perform bidiagonal QR iteration, computing left
!                    singular vectors of R in WORK(IU) and computing
!                    right singular vectors of R in VT
!                    (Workspace: need N*N+BDSPAC)
!
                     CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ), VT, &
                                  LDVT, WORK( IU ), LDWRKU, DUM, 1, &
                                  WORK( IWORK ), INFO )
!
!                    Multiply Q in U by left singular vectors of R in
!                    WORK(IU), storing result in A
!                    (Workspace: need N*N)
!
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, U, LDU, &
                                 WORK( IU ), LDWRKU, ZERO, A, LDA )
!
!                    Copy left singular vectors of A from A to U
!
                     CALL DLACPY( 'F', M, N, A, LDA, U, LDU )
!
                  ELSE
!
!                    Insufficient workspace for a fast algorithm
!
                     ITAU = 1
                     IWORK = ITAU + N
!
!                    Compute A=Q*R, copying result to U
!                    (Workspace: need 2*N, prefer N+N*NB)
!
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
!
!                    Generate Q in U
!                    (Workspace: need N+M, prefer N+M*NB)
!
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Copy R from A to VT, zeroing out below it
!
                     CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, VT( 2, 1 ), &
                                  LDVT )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
!
!                    Bidiagonalize R in VT
!                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
!
                     CALL DGEBRD( N, N, VT, LDVT, S, WORK( IE ), &
                                  WORK( ITAUQ ), WORK( ITAUP ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Multiply Q in U by left bidiagonalizing vectors
!                    in VT
!                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
!
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, VT, LDVT, &
                                  WORK( ITAUQ ), U, LDU, WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
!
!                    Generate right bidiagonalizing vectors in VT
!                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
!
                     CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
!
!                    Perform bidiagonal QR iteration, computing left
!                    singular vectors of A in U and computing right
!                    singular vectors of A in VT
!                    (Workspace: need BDSPAC)
!
                     CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), VT, &
                                  LDVT, U, LDU, DUM, 1, WORK( IWORK ), &
                                  INFO )
!
                  end if
!
               end if
!
            end if
!
         ELSE
!
!           M  <  MNTHR
!
!           Path 10 (M at least N, but not much larger)
!           Reduce to bidiagonal form without QR decomposition
!
            IE = 1
            ITAUQ = IE + N
            ITAUP = ITAUQ + N
            IWORK = ITAUP + N
!
!           Bidiagonalize A
!           (Workspace: need 3*N+M, prefer 3*N+(M+N)*NB)
!
            CALL DGEBRD( M, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ), &
                         WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1, &
                         IERR )
            if ( WNTUAS ) then
!
!              If left singular vectors desired in U, copy result to U
!              and generate left bidiagonalizing vectors in U
!              (Workspace: need 3*N+NCU, prefer 3*N+NCU*NB)
!
               CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
               if ( WNTUS ) &
                  NCU = N
               if ( WNTUA ) &
                  NCU = M
               CALL DORGBR( 'Q', M, NCU, N, U, LDU, WORK( ITAUQ ), &
                            WORK( IWORK ), LWORK-IWORK+1, IERR )
            end if
            if ( WNTVAS ) then
!
!              If right singular vectors desired in VT, copy result to
!              VT and generate right bidiagonalizing vectors in VT
!              (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
!
               CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
               CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ), &
                            WORK( IWORK ), LWORK-IWORK+1, IERR )
            end if
            if ( WNTUO ) then
!
!              If left singular vectors desired in A, generate left
!              bidiagonalizing vectors in A
!              (Workspace: need 4*N, prefer 3*N+N*NB)
!
               CALL DORGBR( 'Q', M, N, N, A, LDA, WORK( ITAUQ ), &
                            WORK( IWORK ), LWORK-IWORK+1, IERR )
            end if
            if ( WNTVO ) then
!
!              If right singular vectors desired in A, generate right
!              bidiagonalizing vectors in A
!              (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
!
               CALL DORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ), &
                            WORK( IWORK ), LWORK-IWORK+1, IERR )
            end if
            IWORK = IE + N
            if ( WNTUAS .OR. WNTUO ) &
               NRU = M
            if ( WNTUN ) &
               NRU = 0
            if ( WNTVAS .OR. WNTVO ) &
               NCVT = N
            if ( WNTVN ) &
               NCVT = 0
            if ( ( .NOT.WNTUO ) .AND. ( .NOT.WNTVO ) ) then
!
!              Perform bidiagonal QR iteration, if desired, computing
!              left singular vectors in U and computing right singular
!              vectors in VT
!              (Workspace: need BDSPAC)
!
               CALL DBDSQR( 'U', N, NCVT, NRU, 0, S, WORK( IE ), VT, &
                            LDVT, U, LDU, DUM, 1, WORK( IWORK ), INFO )
            else if ( ( .NOT.WNTUO ) .AND. WNTVO ) then
!
!              Perform bidiagonal QR iteration, if desired, computing
!              left singular vectors in U and computing right singular
!              vectors in A
!              (Workspace: need BDSPAC)
!
               CALL DBDSQR( 'U', N, NCVT, NRU, 0, S, WORK( IE ), A, LDA, &
                            U, LDU, DUM, 1, WORK( IWORK ), INFO )
            ELSE
!
!              Perform bidiagonal QR iteration, if desired, computing
!              left singular vectors in A and computing right singular
!              vectors in VT
!              (Workspace: need BDSPAC)
!
               CALL DBDSQR( 'U', N, NCVT, NRU, 0, S, WORK( IE ), VT, &
                            LDVT, A, LDA, DUM, 1, WORK( IWORK ), INFO )
            end if
!
         end if
!
      ELSE
!
!        A has more columns than rows. If A has sufficiently more
!        columns than rows, first reduce using the LQ decomposition (if
!        sufficient workspace available)
!
         if ( N.GE.MNTHR ) then
!
            if ( WNTVN ) then
!
!              Path 1t(N much larger than M, JOBVT='N')
!              No right singular vectors to be computed
!
               ITAU = 1
               IWORK = ITAU + M
!
!              Compute A=L*Q
!              (Workspace: need 2*M, prefer M+M*NB)
!
               CALL DGELQF( M, N, A, LDA, WORK( ITAU ), WORK( IWORK ), &
                            LWORK-IWORK+1, IERR )
!
!              Zero out above L
!
               CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ), LDA )
               IE = 1
               ITAUQ = IE + M
               ITAUP = ITAUQ + M
               IWORK = ITAUP + M
!
!              Bidiagonalize L in A
!              (Workspace: need 4*M, prefer 3*M+2*M*NB)
!
               CALL DGEBRD( M, M, A, LDA, S, WORK( IE ), WORK( ITAUQ ), &
                            WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1, &
                            IERR )
               if ( WNTUO .OR. WNTUAS ) then
!
!                 If left singular vectors desired, generate Q
!                 (Workspace: need 4*M, prefer 3*M+M*NB)
!
                  CALL DORGBR( 'Q', M, M, M, A, LDA, WORK( ITAUQ ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
               end if
               IWORK = IE + M
               NRU = 0
               if ( WNTUO .OR. WNTUAS ) &
                  NRU = M
!
!              Perform bidiagonal QR iteration, computing left singular
!              vectors of A in A if desired
!              (Workspace: need BDSPAC)
!
               CALL DBDSQR( 'U', M, 0, NRU, 0, S, WORK( IE ), DUM, 1, A, &
                            LDA, DUM, 1, WORK( IWORK ), INFO )
!
!              If left singular vectors desired in U, copy them there
!
               if ( WNTUAS ) &
                  CALL DLACPY( 'F', M, M, A, LDA, U, LDU )
!
            else if ( WNTVO .AND. WNTUN ) then
!
!              Path 2t(N much larger than M, JOBU='N', JOBVT='O')
!              M right singular vectors to be overwritten on A and
!              no left singular vectors to be computed
!
               if ( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) then
!
!                 Sufficient workspace for a fast algorithm
!
                  IR = 1
                  if ( LWORK.GE.MAX( WRKBL, LDA*N+M )+LDA*M ) then
!
!                    WORK(IU) is LDA by N and WORK(IR) is LDA by M
!
                     LDWRKU = LDA
                     CHUNK = N
                     LDWRKR = LDA
                  else if ( LWORK.GE.MAX( WRKBL, LDA*N+M )+M*M ) then
!
!                    WORK(IU) is LDA by N and WORK(IR) is M by M
!
                     LDWRKU = LDA
                     CHUNK = N
                     LDWRKR = M
                  ELSE
!
!                    WORK(IU) is M by CHUNK and WORK(IR) is M by M
!
                     LDWRKU = M
                     CHUNK = ( LWORK-M*M-M ) / M
                     LDWRKR = M
                  end if
                  ITAU = IR + LDWRKR*M
                  IWORK = ITAU + M
!
!                 Compute A=L*Q
!                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
!
                  CALL DGELQF( M, N, A, LDA, WORK( ITAU ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                 Copy L to WORK(IR) and zero out above it
!
                  CALL DLACPY( 'L', M, M, A, LDA, WORK( IR ), LDWRKR )
                  CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, &
                               WORK( IR+LDWRKR ), LDWRKR )
!
!                 Generate Q in A
!                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
!
                  CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + M
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
!
!                 Bidiagonalize L in WORK(IR)
!                 (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
!
                  CALL DGEBRD( M, M, WORK( IR ), LDWRKR, S, WORK( IE ), &
                               WORK( ITAUQ ), WORK( ITAUP ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                 Generate right vectors bidiagonalizing L
!                 (Workspace: need M*M+4*M-1, prefer M*M+3*M+(M-1)*NB)
!
                  CALL DORGBR( 'P', M, M, M, WORK( IR ), LDWRKR, &
                               WORK( ITAUP ), WORK( IWORK ), &
                               LWORK-IWORK+1, IERR )
                  IWORK = IE + M
!
!                 Perform bidiagonal QR iteration, computing right
!                 singular vectors of L in WORK(IR)
!                 (Workspace: need M*M+BDSPAC)
!
                  CALL DBDSQR( 'U', M, M, 0, 0, S, WORK( IE ), &
                               WORK( IR ), LDWRKR, DUM, 1, DUM, 1, &
                               WORK( IWORK ), INFO )
                  IU = IE + M
!
!                 Multiply right singular vectors of L in WORK(IR) by Q
!                 in A, storing result in WORK(IU) and copying to A
!                 (Workspace: need M*M+2*M, prefer M*M+M*N+M)
!
                  DO 30 I = 1, N, CHUNK
                     BLK = MIN( N-I+1, CHUNK )
                     CALL DGEMM( 'N', 'N', M, BLK, M, ONE, WORK( IR ), &
                                 LDWRKR, A( 1, I ), LDA, ZERO, &
                                 WORK( IU ), LDWRKU )
                     CALL DLACPY( 'F', M, BLK, WORK( IU ), LDWRKU, &
                                  A( 1, I ), LDA )
   30             CONTINUE
!
               ELSE
!
!                 Insufficient workspace for a fast algorithm
!
                  IE = 1
                  ITAUQ = IE + M
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
!
!                 Bidiagonalize A
!                 (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
!
                  CALL DGEBRD( M, N, A, LDA, S, WORK( IE ), &
                               WORK( ITAUQ ), WORK( ITAUP ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                 Generate right vectors bidiagonalizing A
!                 (Workspace: need 4*M, prefer 3*M+M*NB)
!
                  CALL DORGBR( 'P', M, N, M, A, LDA, WORK( ITAUP ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + M
!
!                 Perform bidiagonal QR iteration, computing right
!                 singular vectors of A in A
!                 (Workspace: need BDSPAC)
!
                  CALL DBDSQR( 'L', M, N, 0, 0, S, WORK( IE ), A, LDA, &
                               DUM, 1, DUM, 1, WORK( IWORK ), INFO )
!
               end if
!
            else if ( WNTVO .AND. WNTUAS ) then
!
!              Path 3t(N much larger than M, JOBU='S' or 'A', JOBVT='O')
!              M right singular vectors to be overwritten on A and
!              M left singular vectors to be computed in U
!
               if ( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) then
!
!                 Sufficient workspace for a fast algorithm
!
                  IR = 1
                  if ( LWORK.GE.MAX( WRKBL, LDA*N+M )+LDA*M ) then
!
!                    WORK(IU) is LDA by N and WORK(IR) is LDA by M
!
                     LDWRKU = LDA
                     CHUNK = N
                     LDWRKR = LDA
                  else if ( LWORK.GE.MAX( WRKBL, LDA*N+M )+M*M ) then
!
!                    WORK(IU) is LDA by N and WORK(IR) is M by M
!
                     LDWRKU = LDA
                     CHUNK = N
                     LDWRKR = M
                  ELSE
!
!                    WORK(IU) is M by CHUNK and WORK(IR) is M by M
!
                     LDWRKU = M
                     CHUNK = ( LWORK-M*M-M ) / M
                     LDWRKR = M
                  end if
                  ITAU = IR + LDWRKR*M
                  IWORK = ITAU + M
!
!                 Compute A=L*Q
!                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
!
                  CALL DGELQF( M, N, A, LDA, WORK( ITAU ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                 Copy L to U, zeroing about above it
!
                  CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
                  CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ), &
                               LDU )
!
!                 Generate Q in A
!                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
!
                  CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + M
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
!
!                 Bidiagonalize L in U, copying result to WORK(IR)
!                 (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
!
                  CALL DGEBRD( M, M, U, LDU, S, WORK( IE ), &
                               WORK( ITAUQ ), WORK( ITAUP ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
                  CALL DLACPY( 'U', M, M, U, LDU, WORK( IR ), LDWRKR )
!
!                 Generate right vectors bidiagonalizing L in WORK(IR)
!                 (Workspace: need M*M+4*M-1, prefer M*M+3*M+(M-1)*NB)
!
                  CALL DORGBR( 'P', M, M, M, WORK( IR ), LDWRKR, &
                               WORK( ITAUP ), WORK( IWORK ), &
                               LWORK-IWORK+1, IERR )
!
!                 Generate left vectors bidiagonalizing L in U
!                 (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
!
                  CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + M
!
!                 Perform bidiagonal QR iteration, computing left
!                 singular vectors of L in U, and computing right
!                 singular vectors of L in WORK(IR)
!                 (Workspace: need M*M+BDSPAC)
!
                  CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ), &
                               WORK( IR ), LDWRKR, U, LDU, DUM, 1, &
                               WORK( IWORK ), INFO )
                  IU = IE + M
!
!                 Multiply right singular vectors of L in WORK(IR) by Q
!                 in A, storing result in WORK(IU) and copying to A
!                 (Workspace: need M*M+2*M, prefer M*M+M*N+M))
!
                  DO I = 1, N, CHUNK
                     BLK = MIN( N-I+1, CHUNK )
                     CALL DGEMM( 'N', 'N', M, BLK, M, ONE, WORK( IR ), &
                                 LDWRKR, A( 1, I ), LDA, ZERO, &
                                 WORK( IU ), LDWRKU )
                     CALL DLACPY( 'F', M, BLK, WORK( IU ), LDWRKU, &
                                  A( 1, I ), LDA )
                  end do

               ELSE
!
!                 Insufficient workspace for a fast algorithm
!
                  ITAU = 1
                  IWORK = ITAU + M
!
!                 Compute A=L*Q
!                 (Workspace: need 2*M, prefer M+M*NB)
!
                  CALL DGELQF( M, N, A, LDA, WORK( ITAU ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                 Copy L to U, zeroing out above it
!
                  CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
                  CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ), &
                               LDU )
!
!                 Generate Q in A
!                 (Workspace: need 2*M, prefer M+M*NB)
!
                  CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + M
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
!
!                 Bidiagonalize L in U
!                 (Workspace: need 4*M, prefer 3*M+2*M*NB)
!
                  CALL DGEBRD( M, M, U, LDU, S, WORK( IE ), &
                               WORK( ITAUQ ), WORK( ITAUP ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                 Multiply right vectors bidiagonalizing L by Q in A
!                 (Workspace: need 3*M+N, prefer 3*M+N*NB)
!
                  CALL DORMBR( 'P', 'L', 'T', M, N, M, U, LDU, &
                               WORK( ITAUP ), A, LDA, WORK( IWORK ), &
                               LWORK-IWORK+1, IERR )
!
!                 Generate left vectors bidiagonalizing L in U
!                 (Workspace: need 4*M, prefer 3*M+M*NB)
!
                  CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ), &
                               WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + M
!
!                 Perform bidiagonal QR iteration, computing left
!                 singular vectors of A in U and computing right
!                 singular vectors of A in A
!                 (Workspace: need BDSPAC)
!
                  CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), A, LDA, &
                               U, LDU, DUM, 1, WORK( IWORK ), INFO )
!
               end if
!
            else if ( WNTVS ) then
!
               if ( WNTUN ) then
!
!                 Path 4t(N much larger than M, JOBU='N', JOBVT='S')
!                 M right singular vectors to be computed in VT and
!                 no left singular vectors to be computed
!
                  if ( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) then
!
!                    Sufficient workspace for a fast algorithm
!
                     IR = 1
                     if ( LWORK.GE.WRKBL+LDA*M ) then
!
!                       WORK(IR) is LDA by M
!
                        LDWRKR = LDA
                     ELSE
!
!                       WORK(IR) is M by M
!
                        LDWRKR = M
                     end if
                     ITAU = IR + LDWRKR*M
                     IWORK = ITAU + M
!
!                    Compute A=L*Q
!                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
!
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Copy L to WORK(IR), zeroing out above it
!
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IR ), &
                                  LDWRKR )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, &
                                  WORK( IR+LDWRKR ), LDWRKR )
!
!                    Generate Q in A
!                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
!
                     CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
!
!                    Bidiagonalize L in WORK(IR)
!                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
!
                     CALL DGEBRD( M, M, WORK( IR ), LDWRKR, S, &
                                  WORK( IE ), WORK( ITAUQ ), &
                                  WORK( ITAUP ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
!
!                    Generate right vectors bidiagonalizing L in
!                    WORK(IR)
!                    (Workspace: need M*M+4*M, prefer M*M+3*M+(M-1)*NB)
!
                     CALL DORGBR( 'P', M, M, M, WORK( IR ), LDWRKR, &
                                  WORK( ITAUP ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
                     IWORK = IE + M
!
!                    Perform bidiagonal QR iteration, computing right
!                    singular vectors of L in WORK(IR)
!                    (Workspace: need M*M+BDSPAC)
!
                     CALL DBDSQR( 'U', M, M, 0, 0, S, WORK( IE ), &
                                  WORK( IR ), LDWRKR, DUM, 1, DUM, 1, &
                                  WORK( IWORK ), INFO )
!
!                    Multiply right singular vectors of L in WORK(IR) by
!                    Q in A, storing result in VT
!                    (Workspace: need M*M)
!
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IR ), &
                                 LDWRKR, A, LDA, ZERO, VT, LDVT )
!
                  ELSE
!
!                    Insufficient workspace for a fast algorithm
!
                     ITAU = 1
                     IWORK = ITAU + M
!
!                    Compute A=L*Q
!                    (Workspace: need 2*M, prefer M+M*NB)
!
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Copy result to VT
!
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
!
!                    Generate Q in VT
!                    (Workspace: need 2*M, prefer M+M*NB)
!
                     CALL DORGLQ( M, N, M, VT, LDVT, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
!
!                    Zero out above L in A
!
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ), &
                                  LDA )
!
!                    Bidiagonalize L in A
!                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
!
                     CALL DGEBRD( M, M, A, LDA, S, WORK( IE ), &
                                  WORK( ITAUQ ), WORK( ITAUP ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Multiply right vectors bidiagonalizing L by Q in VT
!                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
!
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, A, LDA, &
                                  WORK( ITAUP ), VT, LDVT, &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
!
!                    Perform bidiagonal QR iteration, computing right
!                    singular vectors of A in VT
!                    (Workspace: need BDSPAC)
!
                     CALL DBDSQR( 'U', M, N, 0, 0, S, WORK( IE ), VT, &
                                  LDVT, DUM, 1, DUM, 1, WORK( IWORK ), &
                                  INFO )
!
                  end if
!
               else if ( WNTUO ) then
!
!                 Path 5t(N much larger than M, JOBU='O', JOBVT='S')
!                 M right singular vectors to be computed in VT and
!                 M left singular vectors to be overwritten on A
!
                  if ( LWORK.GE.2*M*M+MAX( 4*M, BDSPAC ) ) then
!
!                    Sufficient workspace for a fast algorithm
!
                     IU = 1
                     if ( LWORK.GE.WRKBL+2*LDA*M ) then
!
!                       WORK(IU) is LDA by M and WORK(IR) is LDA by M
!
                        LDWRKU = LDA
                        IR = IU + LDWRKU*M
                        LDWRKR = LDA
                     else if ( LWORK.GE.WRKBL+( LDA+M )*M ) then
!
!                       WORK(IU) is LDA by M and WORK(IR) is M by M
!
                        LDWRKU = LDA
                        IR = IU + LDWRKU*M
                        LDWRKR = M
                     ELSE
!
!                       WORK(IU) is M by M and WORK(IR) is M by M
!
                        LDWRKU = M
                        IR = IU + LDWRKU*M
                        LDWRKR = M
                     end if
                     ITAU = IR + LDWRKR*M
                     IWORK = ITAU + M
!
!                    Compute A=L*Q
!                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
!
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Copy L to WORK(IU), zeroing out below it
!
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IU ), &
                                  LDWRKU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, &
                                  WORK( IU+LDWRKU ), LDWRKU )
!
!                    Generate Q in A
!                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
!
                     CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
!
!                    Bidiagonalize L in WORK(IU), copying result to
!                    WORK(IR)
!                    (Workspace: need 2*M*M+4*M,
!                                prefer 2*M*M+3*M+2*M*NB)
!
                     CALL DGEBRD( M, M, WORK( IU ), LDWRKU, S, &
                                  WORK( IE ), WORK( ITAUQ ), &
                                  WORK( ITAUP ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, M, WORK( IU ), LDWRKU, &
                                  WORK( IR ), LDWRKR )
!
!                    Generate right bidiagonalizing vectors in WORK(IU)
!                    (Workspace: need 2*M*M+4*M-1,
!                                prefer 2*M*M+3*M+(M-1)*NB)
!
                     CALL DORGBR( 'P', M, M, M, WORK( IU ), LDWRKU, &
                                  WORK( ITAUP ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
!
!                    Generate left bidiagonalizing vectors in WORK(IR)
!                    (Workspace: need 2*M*M+4*M, prefer 2*M*M+3*M+M*NB)
!
                     CALL DORGBR( 'Q', M, M, M, WORK( IR ), LDWRKR, &
                                  WORK( ITAUQ ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
                     IWORK = IE + M
!
!                    Perform bidiagonal QR iteration, computing left
!                    singular vectors of L in WORK(IR) and computing
!                    right singular vectors of L in WORK(IU)
!                    (Workspace: need 2*M*M+BDSPAC)
!
                     CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ), &
                                  WORK( IU ), LDWRKU, WORK( IR ), &
                                  LDWRKR, DUM, 1, WORK( IWORK ), INFO )
!
!                    Multiply right singular vectors of L in WORK(IU) by
!                    Q in A, storing result in VT
!                    (Workspace: need M*M)
!
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ), &
                                 LDWRKU, A, LDA, ZERO, VT, LDVT )
!
!                    Copy left singular vectors of L to A
!                    (Workspace: need M*M)
!
                     CALL DLACPY( 'F', M, M, WORK( IR ), LDWRKR, A, &
                                  LDA )
!
                  ELSE
!
!                    Insufficient workspace for a fast algorithm
!
                     ITAU = 1
                     IWORK = ITAU + M
!
!                    Compute A=L*Q, copying result to VT
!                    (Workspace: need 2*M, prefer M+M*NB)
!
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
!
!                    Generate Q in VT
!                    (Workspace: need 2*M, prefer M+M*NB)
!
                     CALL DORGLQ( M, N, M, VT, LDVT, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
!
!                    Zero out above L in A
!
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ), &
                                  LDA )
!
!                    Bidiagonalize L in A
!                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
!
                     CALL DGEBRD( M, M, A, LDA, S, WORK( IE ), &
                                  WORK( ITAUQ ), WORK( ITAUP ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Multiply right vectors bidiagonalizing L by Q in VT
!                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
!
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, A, LDA, &
                                  WORK( ITAUP ), VT, LDVT, &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Generate left bidiagonalizing vectors of L in A
!                    (Workspace: need 4*M, prefer 3*M+M*NB)
!
                     CALL DORGBR( 'Q', M, M, M, A, LDA, WORK( ITAUQ ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
!
!                    Perform bidiagonal QR iteration, compute left
!                    singular vectors of A in A and compute right
!                    singular vectors of A in VT
!                    (Workspace: need BDSPAC)
!
                     CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT, &
                                  LDVT, A, LDA, DUM, 1, WORK( IWORK ), &
                                  INFO )
!
                  end if
!
               else if ( WNTUAS ) then
!
!                 Path 6t(N much larger than M, JOBU='S' or 'A',
!                         JOBVT='S')
!                 M right singular vectors to be computed in VT and
!                 M left singular vectors to be computed in U
!
                  if ( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) then
!
!                    Sufficient workspace for a fast algorithm
!
                     IU = 1
                     if ( LWORK.GE.WRKBL+LDA*M ) then
!
!                       WORK(IU) is LDA by N
!
                        LDWRKU = LDA
                     ELSE
!
!                       WORK(IU) is LDA by M
!
                        LDWRKU = M
                     end if
                     ITAU = IU + LDWRKU*M
                     IWORK = ITAU + M
!
!                    Compute A=L*Q
!                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
!
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Copy L to WORK(IU), zeroing out above it
!
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IU ), &
                                  LDWRKU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, &
                                  WORK( IU+LDWRKU ), LDWRKU )
!
!                    Generate Q in A
!                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
!
                     CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
!
!                    Bidiagonalize L in WORK(IU), copying result to U
!                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
!
                     CALL DGEBRD( M, M, WORK( IU ), LDWRKU, S, &
                                  WORK( IE ), WORK( ITAUQ ), &
                                  WORK( ITAUP ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, M, WORK( IU ), LDWRKU, U, &
                                  LDU )
!
!                    Generate right bidiagonalizing vectors in WORK(IU)
!                    (Workspace: need M*M+4*M-1,
!                                prefer M*M+3*M+(M-1)*NB)
!
                     CALL DORGBR( 'P', M, M, M, WORK( IU ), LDWRKU, &
                                  WORK( ITAUP ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
!
!                    Generate left bidiagonalizing vectors in U
!                    (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
!
                     CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
!
!                    Perform bidiagonal QR iteration, computing left
!                    singular vectors of L in U and computing right
!                    singular vectors of L in WORK(IU)
!                    (Workspace: need M*M+BDSPAC)
!
                     CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ), &
                                  WORK( IU ), LDWRKU, U, LDU, DUM, 1, &
                                  WORK( IWORK ), INFO )
!
!                    Multiply right singular vectors of L in WORK(IU) by
!                    Q in A, storing result in VT
!                    (Workspace: need M*M)
!
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ), &
                                 LDWRKU, A, LDA, ZERO, VT, LDVT )
!
                  ELSE
!
!                    Insufficient workspace for a fast algorithm
!
                     ITAU = 1
                     IWORK = ITAU + M
!
!                    Compute A=L*Q, copying result to VT
!                    (Workspace: need 2*M, prefer M+M*NB)
!
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
!
!                    Generate Q in VT
!                    (Workspace: need 2*M, prefer M+M*NB)
!
                     CALL DORGLQ( M, N, M, VT, LDVT, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Copy L to U, zeroing out above it
!
                     CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ), &
                                  LDU )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
!
!                    Bidiagonalize L in U
!                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
!
                     CALL DGEBRD( M, M, U, LDU, S, WORK( IE ), &
                                  WORK( ITAUQ ), WORK( ITAUP ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Multiply right bidiagonalizing vectors in U by Q
!                    in VT
!                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
!
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, U, LDU, &
                                  WORK( ITAUP ), VT, LDVT, &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Generate left bidiagonalizing vectors in U
!                    (Workspace: need 4*M, prefer 3*M+M*NB)
!
                     CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
!
!                    Perform bidiagonal QR iteration, computing left
!                    singular vectors of A in U and computing right
!                    singular vectors of A in VT
!                    (Workspace: need BDSPAC)
!
                     CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT, &
                                  LDVT, U, LDU, DUM, 1, WORK( IWORK ), &
                                  INFO )
!
                  end if
!
               end if
!
            else if ( WNTVA ) then
!
               if ( WNTUN ) then
!
!                 Path 7t(N much larger than M, JOBU='N', JOBVT='A')
!                 N right singular vectors to be computed in VT and
!                 no left singular vectors to be computed
!
                  if ( LWORK.GE.M*M+MAX( N+M, 4*M, BDSPAC ) ) then
!
!                    Sufficient workspace for a fast algorithm
!
                     IR = 1
                     if ( LWORK.GE.WRKBL+LDA*M ) then
!
!                       WORK(IR) is LDA by M
!
                        LDWRKR = LDA
                     ELSE
!
!                       WORK(IR) is M by M
!
                        LDWRKR = M
                     end if
                     ITAU = IR + LDWRKR*M
                     IWORK = ITAU + M
!
!                    Compute A=L*Q, copying result to VT
!                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
!
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
!
!                    Copy L to WORK(IR), zeroing out above it
!
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IR ), &
                                  LDWRKR )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, &
                                  WORK( IR+LDWRKR ), LDWRKR )
!
!                    Generate Q in VT
!                    (Workspace: need M*M+M+N, prefer M*M+M+N*NB)
!
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
!
!                    Bidiagonalize L in WORK(IR)
!                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
!
                     CALL DGEBRD( M, M, WORK( IR ), LDWRKR, S, &
                                  WORK( IE ), WORK( ITAUQ ), &
                                  WORK( ITAUP ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
!
!                    Generate right bidiagonalizing vectors in WORK(IR)
!                    (Workspace: need M*M+4*M-1,
!                                prefer M*M+3*M+(M-1)*NB)
!
                     CALL DORGBR( 'P', M, M, M, WORK( IR ), LDWRKR, &
                                  WORK( ITAUP ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
                     IWORK = IE + M
!
!                    Perform bidiagonal QR iteration, computing right
!                    singular vectors of L in WORK(IR)
!                    (Workspace: need M*M+BDSPAC)
!
                     CALL DBDSQR( 'U', M, M, 0, 0, S, WORK( IE ), &
                                  WORK( IR ), LDWRKR, DUM, 1, DUM, 1, &
                                  WORK( IWORK ), INFO )
!
!                    Multiply right singular vectors of L in WORK(IR) by
!                    Q in VT, storing result in A
!                    (Workspace: need M*M)
!
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IR ), &
                                 LDWRKR, VT, LDVT, ZERO, A, LDA )
!
!                    Copy right singular vectors of A from A to VT
!
                     CALL DLACPY( 'F', M, N, A, LDA, VT, LDVT )
!
                  ELSE
!
!                    Insufficient workspace for a fast algorithm
!
                     ITAU = 1
                     IWORK = ITAU + M
!
!                    Compute A=L*Q, copying result to VT
!                    (Workspace: need 2*M, prefer M+M*NB)
!
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
!
!                    Generate Q in VT
!                    (Workspace: need M+N, prefer M+N*NB)
!
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
!
!                    Zero out above L in A
!
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ), &
                                  LDA )
!
!                    Bidiagonalize L in A
!                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
!
                     CALL DGEBRD( M, M, A, LDA, S, WORK( IE ), &
                                  WORK( ITAUQ ), WORK( ITAUP ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Multiply right bidiagonalizing vectors in A by Q
!                    in VT
!                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
!
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, A, LDA, &
                                  WORK( ITAUP ), VT, LDVT, &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
!
!                    Perform bidiagonal QR iteration, computing right
!                    singular vectors of A in VT
!                    (Workspace: need BDSPAC)
!
                     CALL DBDSQR( 'U', M, N, 0, 0, S, WORK( IE ), VT, &
                                  LDVT, DUM, 1, DUM, 1, WORK( IWORK ), &
                                  INFO )
!
                  end if
!
               else if ( WNTUO ) then
!
!                 Path 8t(N much larger than M, JOBU='O', JOBVT='A')
!                 N right singular vectors to be computed in VT and
!                 M left singular vectors to be overwritten on A
!
                  if ( LWORK.GE.2*M*M+MAX( N+M, 4*M, BDSPAC ) ) then
!
!                    Sufficient workspace for a fast algorithm
!
                     IU = 1
                     if ( LWORK.GE.WRKBL+2*LDA*M ) then
!
!                       WORK(IU) is LDA by M and WORK(IR) is LDA by M
!
                        LDWRKU = LDA
                        IR = IU + LDWRKU*M
                        LDWRKR = LDA
                     else if ( LWORK.GE.WRKBL+( LDA+M )*M ) then
!
!                       WORK(IU) is LDA by M and WORK(IR) is M by M
!
                        LDWRKU = LDA
                        IR = IU + LDWRKU*M
                        LDWRKR = M
                     ELSE
!
!                       WORK(IU) is M by M and WORK(IR) is M by M
!
                        LDWRKU = M
                        IR = IU + LDWRKU*M
                        LDWRKR = M
                     end if
                     ITAU = IR + LDWRKR*M
                     IWORK = ITAU + M
!
!                    Compute A=L*Q, copying result to VT
!                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
!
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
!
!                    Generate Q in VT
!                    (Workspace: need 2*M*M+M+N, prefer 2*M*M+M+N*NB)
!
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Copy L to WORK(IU), zeroing out above it
!
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IU ), &
                                  LDWRKU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, &
                                  WORK( IU+LDWRKU ), LDWRKU )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
!
!                    Bidiagonalize L in WORK(IU), copying result to
!                    WORK(IR)
!                    (Workspace: need 2*M*M+4*M,
!                                prefer 2*M*M+3*M+2*M*NB)
!
                     CALL DGEBRD( M, M, WORK( IU ), LDWRKU, S, &
                                  WORK( IE ), WORK( ITAUQ ), &
                                  WORK( ITAUP ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, M, WORK( IU ), LDWRKU, &
                                  WORK( IR ), LDWRKR )
!
!                    Generate right bidiagonalizing vectors in WORK(IU)
!                    (Workspace: need 2*M*M+4*M-1,
!                                prefer 2*M*M+3*M+(M-1)*NB)
!
                     CALL DORGBR( 'P', M, M, M, WORK( IU ), LDWRKU, &
                                  WORK( ITAUP ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
!
!                    Generate left bidiagonalizing vectors in WORK(IR)
!                    (Workspace: need 2*M*M+4*M, prefer 2*M*M+3*M+M*NB)
!
                     CALL DORGBR( 'Q', M, M, M, WORK( IR ), LDWRKR, &
                                  WORK( ITAUQ ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
                     IWORK = IE + M
!
!                    Perform bidiagonal QR iteration, computing left
!                    singular vectors of L in WORK(IR) and computing
!                    right singular vectors of L in WORK(IU)
!                    (Workspace: need 2*M*M+BDSPAC)
!
                     CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ), &
                                  WORK( IU ), LDWRKU, WORK( IR ), &
                                  LDWRKR, DUM, 1, WORK( IWORK ), INFO )
!
!                    Multiply right singular vectors of L in WORK(IU) by
!                    Q in VT, storing result in A
!                    (Workspace: need M*M)
!
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ), &
                                 LDWRKU, VT, LDVT, ZERO, A, LDA )
!
!                    Copy right singular vectors of A from A to VT
!
                     CALL DLACPY( 'F', M, N, A, LDA, VT, LDVT )
!
!                    Copy left singular vectors of A from WORK(IR) to A
!
                     CALL DLACPY( 'F', M, M, WORK( IR ), LDWRKR, A, &
                                  LDA )
!
                  ELSE
!
!                    Insufficient workspace for a fast algorithm
!
                     ITAU = 1
                     IWORK = ITAU + M
!
!                    Compute A=L*Q, copying result to VT
!                    (Workspace: need 2*M, prefer M+M*NB)
!
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
!
!                    Generate Q in VT
!                    (Workspace: need M+N, prefer M+N*NB)
!
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
!
!                    Zero out above L in A
!
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ), &
                                  LDA )
!
!                    Bidiagonalize L in A
!                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
!
                     CALL DGEBRD( M, M, A, LDA, S, WORK( IE ), &
                                  WORK( ITAUQ ), WORK( ITAUP ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Multiply right bidiagonalizing vectors in A by Q
!                    in VT
!                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
!
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, A, LDA, &
                                  WORK( ITAUP ), VT, LDVT, &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Generate left bidiagonalizing vectors in A
!                    (Workspace: need 4*M, prefer 3*M+M*NB)
!
                     CALL DORGBR( 'Q', M, M, M, A, LDA, WORK( ITAUQ ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
!
!                    Perform bidiagonal QR iteration, computing left
!                    singular vectors of A in A and computing right
!                    singular vectors of A in VT
!                    (Workspace: need BDSPAC)
!
                     CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT, &
                                  LDVT, A, LDA, DUM, 1, WORK( IWORK ), &
                                  INFO )
!
                  end if
!
               else if ( WNTUAS ) then
!
!                 Path 9t(N much larger than M, JOBU='S' or 'A',
!                         JOBVT='A')
!                 N right singular vectors to be computed in VT and
!                 M left singular vectors to be computed in U
!
                  if ( LWORK.GE.M*M+MAX( N+M, 4*M, BDSPAC ) ) then
!
!                    Sufficient workspace for a fast algorithm
!
                     IU = 1
                     if ( LWORK.GE.WRKBL+LDA*M ) then
!
!                       WORK(IU) is LDA by M
!
                        LDWRKU = LDA
                     ELSE
!
!                       WORK(IU) is M by M
!
                        LDWRKU = M
                     end if
                     ITAU = IU + LDWRKU*M
                     IWORK = ITAU + M
!
!                    Compute A=L*Q, copying result to VT
!                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
!
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
!
!                    Generate Q in VT
!                    (Workspace: need M*M+M+N, prefer M*M+M+N*NB)
!
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Copy L to WORK(IU), zeroing out above it
!
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IU ), &
                                  LDWRKU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, &
                                  WORK( IU+LDWRKU ), LDWRKU )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
!
!                    Bidiagonalize L in WORK(IU), copying result to U
!                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
!
                     CALL DGEBRD( M, M, WORK( IU ), LDWRKU, S, &
                                  WORK( IE ), WORK( ITAUQ ), &
                                  WORK( ITAUP ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, M, WORK( IU ), LDWRKU, U, &
                                  LDU )
!
!                    Generate right bidiagonalizing vectors in WORK(IU)
!                    (Workspace: need M*M+4*M, prefer M*M+3*M+(M-1)*NB)
!
                     CALL DORGBR( 'P', M, M, M, WORK( IU ), LDWRKU, &
                                  WORK( ITAUP ), WORK( IWORK ), &
                                  LWORK-IWORK+1, IERR )
!
!                    Generate left bidiagonalizing vectors in U
!                    (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
!
                     CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
!
!                    Perform bidiagonal QR iteration, computing left
!                    singular vectors of L in U and computing right
!                    singular vectors of L in WORK(IU)
!                    (Workspace: need M*M+BDSPAC)
!
                     CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ), &
                                  WORK( IU ), LDWRKU, U, LDU, DUM, 1, &
                                  WORK( IWORK ), INFO )
!
!                    Multiply right singular vectors of L in WORK(IU) by
!                    Q in VT, storing result in A
!                    (Workspace: need M*M)
!
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ), &
                                 LDWRKU, VT, LDVT, ZERO, A, LDA )
!
!                    Copy right singular vectors of A from A to VT
!
                     CALL DLACPY( 'F', M, N, A, LDA, VT, LDVT )
!
                  ELSE
!
!                    Insufficient workspace for a fast algorithm
!
                     ITAU = 1
                     IWORK = ITAU + M
!
!                    Compute A=L*Q, copying result to VT
!                    (Workspace: need 2*M, prefer M+M*NB)
!
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
!
!                    Generate Q in VT
!                    (Workspace: need M+N, prefer M+N*NB)
!
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Copy L to U, zeroing out above it
!
                     CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ), &
                                  LDU )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
!
!                    Bidiagonalize L in U
!                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
!
                     CALL DGEBRD( M, M, U, LDU, S, WORK( IE ), &
                                  WORK( ITAUQ ), WORK( ITAUP ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Multiply right bidiagonalizing vectors in U by Q
!                    in VT
!                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
!
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, U, LDU, &
                                  WORK( ITAUP ), VT, LDVT, &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
!
!                    Generate left bidiagonalizing vectors in U
!                    (Workspace: need 4*M, prefer 3*M+M*NB)
!
                     CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ), &
                                  WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
!
!                    Perform bidiagonal QR iteration, computing left
!                    singular vectors of A in U and computing right
!                    singular vectors of A in VT
!                    (Workspace: need BDSPAC)
!
                     CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT, &
                                  LDVT, U, LDU, DUM, 1, WORK( IWORK ), &
                                  INFO )
!
                  end if
!
               end if
!
            end if
!
         ELSE
!
!           N  <  MNTHR
!
!           Path 10t(N greater than M, but not much larger)
!           Reduce to bidiagonal form without LQ decomposition
!
            IE = 1
            ITAUQ = IE + M
            ITAUP = ITAUQ + M
            IWORK = ITAUP + M
!
!           Bidiagonalize A
!           (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
!
            CALL DGEBRD( M, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ), &
                         WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1, &
                         IERR )
            if ( WNTUAS ) then
!
!              If left singular vectors desired in U, copy result to U
!              and generate left bidiagonalizing vectors in U
!              (Workspace: need 4*M-1, prefer 3*M+(M-1)*NB)
!
               CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
               CALL DORGBR( 'Q', M, M, N, U, LDU, WORK( ITAUQ ), &
                            WORK( IWORK ), LWORK-IWORK+1, IERR )
            end if
            if ( WNTVAS ) then
!
!              If right singular vectors desired in VT, copy result to
!              VT and generate right bidiagonalizing vectors in VT
!              (Workspace: need 3*M+NRVT, prefer 3*M+NRVT*NB)
!
               CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
               if ( WNTVA ) &
                  NRVT = N
               if ( WNTVS ) &
                  NRVT = M
               CALL DORGBR( 'P', NRVT, N, M, VT, LDVT, WORK( ITAUP ), &
                            WORK( IWORK ), LWORK-IWORK+1, IERR )
            end if
            if ( WNTUO ) then
!
!              If left singular vectors desired in A, generate left
!              bidiagonalizing vectors in A
!              (Workspace: need 4*M-1, prefer 3*M+(M-1)*NB)
!
               CALL DORGBR( 'Q', M, M, N, A, LDA, WORK( ITAUQ ), &
                            WORK( IWORK ), LWORK-IWORK+1, IERR )
            end if
            if ( WNTVO ) then
!
!              If right singular vectors desired in A, generate right
!              bidiagonalizing vectors in A
!              (Workspace: need 4*M, prefer 3*M+M*NB)
!
               CALL DORGBR( 'P', M, N, M, A, LDA, WORK( ITAUP ), &
                            WORK( IWORK ), LWORK-IWORK+1, IERR )
            end if
            IWORK = IE + M
            if ( WNTUAS .OR. WNTUO ) &
               NRU = M
            if ( WNTUN ) &
               NRU = 0
            if ( WNTVAS .OR. WNTVO ) &
               NCVT = N
            if ( WNTVN ) &
               NCVT = 0
            if ( ( .NOT.WNTUO ) .AND. ( .NOT.WNTVO ) ) then
!
!              Perform bidiagonal QR iteration, if desired, computing
!              left singular vectors in U and computing right singular
!              vectors in VT
!              (Workspace: need BDSPAC)
!
               CALL DBDSQR( 'L', M, NCVT, NRU, 0, S, WORK( IE ), VT, &
                            LDVT, U, LDU, DUM, 1, WORK( IWORK ), INFO )
            else if ( ( .NOT.WNTUO ) .AND. WNTVO ) then
!
!              Perform bidiagonal QR iteration, if desired, computing
!              left singular vectors in U and computing right singular
!              vectors in A
!              (Workspace: need BDSPAC)
!
               CALL DBDSQR( 'L', M, NCVT, NRU, 0, S, WORK( IE ), A, LDA, &
                            U, LDU, DUM, 1, WORK( IWORK ), INFO )
            ELSE
!
!              Perform bidiagonal QR iteration, if desired, computing
!              left singular vectors in A and computing right singular
!              vectors in VT
!              (Workspace: need BDSPAC)
!
               CALL DBDSQR( 'L', M, NCVT, NRU, 0, S, WORK( IE ), VT, &
                            LDVT, A, LDA, DUM, 1, WORK( IWORK ), INFO )
            end if
!
         end if
!
      end if
!
!     If DBDSQR failed to converge, copy unconverged superdiagonals
!     to WORK( 2:MINMN )
!
      if ( INFO.NE.0 ) then
         if ( 2 < IE ) then
            DO I = 1, MINMN - 1
               WORK( I+1 ) = WORK( I+IE-1 )
            end do
         end if
         if ( IE < 2 ) then
            DO I = MINMN - 1, 1, -1
               WORK( I+1 ) = WORK( I+IE-1 )
            end do
         end if
      end if
!
!     Undo scaling if necessary
!
      if ( ISCL == 1 ) then
         if ( BIGNUM < anrm ) &
            CALL DLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN, 1, S, MINMN, &
                         IERR )
         if ( INFO.NE.0 .AND. ANRM.GT.BIGNUM ) &
            CALL DLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN-1, 1, WORK( 2 ), &
                         MINMN, IERR )
         if ( ANRM < SMLNUM ) &
            CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN, 1, S, MINMN, &
                         IERR )
         if ( INFO.NE.0 .AND. ANRM < SMLNUM ) &
            CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN-1, 1, WORK( 2 ), &
                         MINMN, IERR )
      end if
!
!     Return optimal workspace in WORK(1)
!
      WORK( 1 ) = MAXWRK
!
      RETURN
!
!     End of DGESVD
!
      END
