MODULE test_module
    CONTAINS
        FUNCTION has_close_elements(numbers, threshold) RESULT(result)
        IMPLICIT NONE
        REAL, DIMENSION(:), INTENT(IN) :: numbers
        REAL,INTENT(IN) :: threshold 
        INTEGER :: size0, N, M
        LOGICAL :: result
        result = .FALSE.
        size0 = SIZE(numbers)

        DO N = 1, SIZE(numbers),1
            DO M = N+1,size0,1
                IF(ABS(numbers(M)-numbers(N))<threshold) THEN
                    result = .TRUE.
                    EXIT
                    END IF
                END DO
            END DO
        END FUNCTION has_close_elements
END MODULE test_module


PROGRAM main
    USE test_module
    IMPLICIT NONE
    call test([1.0, 2.0, 3.0], 0.5, .false.)
    call test([1.0, 2.8, 3.0, 4.0, 5.0, 2.0], 0.3, .true.)
    call test([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.3, .true.)
    call test([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.05, .false.)
    call test([1.0, 2.0, 5.9, 4.0, 5.0], 0.95, .True.)
    call test([1.0, 2.0, 5.9, 4.0, 5.0], 0.8, .false.)
    call test([1.0, 2.0, 3.0, 4.0, 5.0, 2.0], 0.1, .true.)
    call test([1.1, 2.2, 3.1, 4.1, 5.1], 1.0, .True.)
    call test([1.1, 2.2, 3.1, 4.1, 5.1], 0.5, .false.)
    contains
    subroutine test (p1, p2, st)
        implicit none
            REAL, DIMENSION(:), INTENT(IN) :: p1
            REAL, INTENT(IN) :: p2
            logical, INTENT(IN) :: st
            logical :: a
            logical :: exists
            inquire(file='result.txt', exist=exists)
            a = has_close_elements(p1, p2)
            if (a .eqv. st) then
                if (exists) then
                  open(1, file = 'result.txt', status='old')
                else
                  open(1, file = 'result.txt', status='new')
                end if
                write(1,*)'F', a, st
            end if
        end subroutine test

END PROGRAM main
