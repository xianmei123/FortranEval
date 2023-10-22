MODULE test_module
    CONTAINS
FUNCTION SDSDOT(N, SB, SX, INCX, SY, INCY) RESULT(result)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N, INCX, INCY
  REAL, INTENT(IN) :: SB
  REAL, DIMENSION(*), INTENT(IN) :: SX
  REAL, DIMENSION(*), INTENT(IN) :: SY
  REAL :: result
  INTEGER :: I, KX, KY, NS
  REAL(KIND=8) :: DSDOT

  DSDOT = DBLE(SB)
  IF (N.LE.0) THEN
    result = REAL(DSDOT)
    RETURN
  END IF

  IF (INCX.EQ.INCY .AND. INCX.GT.0) THEN
    NS = N * INCX
    DO I = 1, NS, INCX
      DSDOT = DSDOT + DBLE(SX(I)) * DBLE(SY(I))
    END DO
  ELSE
    KX = 1
    KY = 1
    IF (INCX.LT.0) KX = 1 + (1 - N) * INCX
    IF (INCY.LT.0) KY = 1 + (1 - N) * INCY
    DO I = 1, N
      DSDOT = DSDOT + DBLE(SX(KX)) * DBLE(SY(KY))
      KX = KX + INCX
      KY = KY + INCY
    END DO
  END IF

  result = REAL(DSDOT)
END FUNCTION SDSDOT
END MODULE test_module

PROGRAM SDSDOT_Test
  USE test_module
  IMPLICIT NONE
  call test(5, 1.0, [1.0, 2.0, 3.0, 4.0, 5.0], 1, [2.0, 3.0, 4.0, 5.0, 6.0], 1, 71.0000000)
  call test(4, 11.0, [1.1, -2.0, 3.0, -4.0, 5.0, -6.0, 7.0], 2, &
          [1.955, -2.0, 3.0, -4.0], 1, -5.84949970)
  call test(7, 11.0, [1.0, -2.89498, 3.0, -4.0, 5.0, -6.0, 7.0], -1, &
          [0.0, -2.325, 3.0, -4.0, 5.0, -6.0, 7.0], 1, 95.3198776)
  call test(4, 11.0894, [1.984, -2.0, 3.0, -4.0, 5.0, -6.0, 7.0], -2, &
          [1.0, -2.0, 3.0, -4.0], -1, -5.92659950)
  contains
  subroutine test (N, SB, SX, INCX, SY, INCY, ST)
  implicit none
      INTEGER, INTENT(IN) :: N, INCX, INCY
      REAL, DIMENSION(N), INTENT(IN) :: SX, SY
      REAL, INTENT(IN) :: SB, st
      REAL :: a
      logical :: exists
      inquire(file='result.txt', exist=exists)

      a = SDSDOT(N, SB, SX, INCX, SY, INCY)
      write(*,*) a, ST
      if (a /= st) then
          if (exists) then
            open(1, file = 'result.txt', status='old')
          else
            open(1, file = 'result.txt', status='new')
          end if
          write(1,*)'F', a, st
      end if
  end subroutine test


END PROGRAM SDSDOT_Test