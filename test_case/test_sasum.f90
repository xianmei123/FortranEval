MODULE test_module
    CONTAINS
    FUNCTION SASUM(N, SX, INCX) RESULT(RESULT)
   INTEGER, INTENT(IN) :: N, INCX
   REAL, INTENT(IN) :: SX(*)
   REAL :: RESULT
   REAL :: STEMP
   INTEGER :: I, M, MP1, NINCX

   RESULT = 0.0
   STEMP = 0.0

   IF (N <= 0 .OR. INCX <= 0) THEN
      RETURN
   END IF

   IF (INCX == 1) THEN
!     code for increment equal to 1

!     clean-up loop
      M = MOD(N, 6)
      IF (M /= 0) THEN
         DO I = 1, M
            STEMP = STEMP + ABS(SX(I))
         END DO
         IF (N < 6) THEN
            RESULT = STEMP
            RETURN
         END IF
      END IF
      MP1 = M + 1
      DO I = MP1, N, 6
         STEMP = STEMP + ABS(SX(I)) + ABS(SX(I+1)) &
               + ABS(SX(I+2)) + ABS(SX(I+3)) &
               + ABS(SX(I+4)) + ABS(SX(I+5))
      END DO
   ELSE
!     code for increment not equal to 1
      NINCX = N * INCX
      DO I = 1, NINCX, INCX
         STEMP = STEMP + ABS(SX(I))
      END DO
   END IF

   RESULT = STEMP
   RETURN

END FUNCTION SASUM
END MODULE test_module

PROGRAM main
    USE test_module
    IMPLICIT NONE

    call test(7, [1.0, -2.0, 3.0, -4.0, 5.0, -6.0, 7.0], 1, 28.0)
    call test(7, [1.0, -2.0, 3.0, -4.0, 5.0, -6.0, 7.0], 0, 0.0)
    call test(7, [1.0, -2.0, 3.0, -4.0, 5.0, -6.0, 7.0], 2, 17.0)
    call test(9, [1.0, -2.0, 3.0, -4.0, 5.0, -6.0, 7.0, 0.0, 3.0], 2, 19.0)
    contains
    subroutine test (N,SX,INCX, st)
        implicit none
            INTEGER, INTENT(IN) :: N, INCX
            REAL, DIMENSION(N), INTENT(IN) :: SX
            REAL :: st
            REAL :: a
            logical :: exists
            inquire(file='result.txt', exist=exists)

            a = SASUM(N,SX,INCX)
!            write(*,*) a, ST,N,INCX,SX(1:7)
            if (a /= st) then
                if (exists) then
                  open(1, file = 'result.txt', status='old')
                else
                  open(1, file = 'result.txt', status='new')
                end if
                write(1,*)'F', a, st
            end if
        end subroutine test


END PROGRAM main

