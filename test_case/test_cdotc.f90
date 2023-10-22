MODULE test_module
    CONTAINS
    COMPLEX FUNCTION CDOTC(N,CX,INCX,CY,INCY)
    INTEGER, INTENT(IN) :: N, INCX, INCY
    COMPLEX, INTENT(IN) :: CX(*), CY(*)
    COMPLEX :: CTEMP
    INTEGER :: I, IX, IY

    CTEMP = (0.0,0.0)
    CDOTC = (0.0,0.0)

    IF (N.LE.0) RETURN

    IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
        ! Code for both increments equal to 1
        DO I = 1, N
            CTEMP = CTEMP + CONJG(CX(I))*CY(I)
        END DO
    ELSE
        ! Code for unequal increments or increments not equal to 1
        IX = 1
        IY = 1
        IF (INCX.LT.0) IX = (-N+1)*INCX + 1
        IF (INCY.LT.0) IY = (-N+1)*INCY + 1
        DO I = 1, N
            CTEMP = CTEMP + CONJG(CX(IX))*CY(IY)
            IX = IX + INCX
            IY = IY + INCY
        END DO
    END IF


    CDOTC = CTEMP
END FUNCTION CDOTC
END MODULE test_module

PROGRAM main
    USE test_module
    IMPLICIT NONE
    call test(5, [(1.0, 2.0), (2.0, 3.0), (3.0, 4.0), (4.0, 5.0), (5.0, 6.0)], 1, &
            [(2.0, 1.0), (3.0, 2.0), (4.0, 3.0), (5.0, 4.0), (6.0, 5.0)], 1, (140.000000    , -35.0000000    ))

    call test(3, [(1.0, 2.0), (2.0, 3.0), (3.0, 4.0), (4.0, 5.0), (5.0, 6.0)], 2, &
            [(2.0, 1.0), (3.0, 2.0), (4.0, 3.0)], 1, (  59.0000000    , -18.0000000    ))
    call test(5, [(1.0, 2.0), (2.0, 3.0), (3.0, 4.0), (4.0, 5.0), (5.0, 6.0)], -1, &
            [(2.0, 1.0), (3.0, 2.0), (4.0, 3.0), (5.0, 4.0), (6.0, 5.0)], -1, (  140.000000    , -35.0000000    ))


    call test(3, [(1.0, 2.0), (2.0, 3.0), (3.0, 4.0), (4.0, 5.0), (5.0, 6.0)], -2, &
            [(2.0, 1.0), (3.0, 2.0), (4.0, 3.0), (5.0, 4.0), (6.0, 5.0)], 1,  (  43.0000000    , -18.0000000    ) )


    contains
    subroutine test (N,CX,INCX,CY,INCY,st)
        implicit none
            INTEGER, INTENT(IN) :: N, INCX, INCY
            COMPLEX, INTENT(IN) :: CX(*), CY(*)
            COMPLEX, INTENT(IN) :: st
            COMPLEX :: a
            logical :: exists
            inquire(file='result.txt', exist=exists)

            a = CDOTC(N,CX,INCX,CY,INCY)
            write(*,*) a, ST,N,INCX,INCY,CX(1:5),CY(1:5)
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
