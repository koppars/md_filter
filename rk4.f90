    SUBROUTINE RK4
    !Need to pass in both x and y values, as in the RHS we need to calculate the absolute distance between the particles
    !Fourth order Runge-Kutta algorithm from Hoover's SPAM book
    !YYN hold intermediate positions, force, and energy
    !YPN hold derivatives at intermediate times
        USE headerFile
        IMPLICIT NONE

        REAL(kind = double), DIMENSION(:,:), ALLOCATABLE :: XX1,XX2,XX3,XX4,XXP
        REAL(kind = double), DIMENSION(:,:), ALLOCATABLE :: XP1,XP2,XP3,XP4
        REAL(kind = double), DIMENSION(:,:), ALLOCATABLE :: YY1,YY2,YY3,YY4,YYP
        REAL(kind = double), DIMENSION(:,:), ALLOCATABLE :: YP1,YP2,YP3,YP4

        ALLOCATE(XXP(2,nop))
        ALLOCATE(XX1(2,nop))
        ALLOCATE(XX2(2,nop))
        ALLOCATE(XX3(2,nop))
        ALLOCATE(XX4(2,nop))
        ALLOCATE(XP1(2,nop))
        ALLOCATE(XP2(2,nop))
        ALLOCATE(XP3(2,nop))
        ALLOCATE(XP4(2,nop))

        ALLOCATE(YYP(2,nop))
        ALLOCATE(YY1(2,nop))
        ALLOCATE(YY2(2,nop))
        ALLOCATE(YY3(2,nop))
        ALLOCATE(YY4(2,nop))
        ALLOCATE(YP1(2,nop))
        ALLOCATE(YP2(2,nop))
        ALLOCATE(YP3(2,nop))
        ALLOCATE(YP4(2,nop))

        !X1
        DO I = 1,2
            DO J = 1, nop
                XX1(I,J) = x(I,J)
                YY1(I,J) = y(I,J)
            END DO
        END DO  

        !RHS1
        CALL RHS(XX1,XP1,YY1,YP1)

        !X2
        DO I = 1,2
            DO J = 1, nop
                XX2(I,J) = x(I,J) + (0.5D00*DT*XP1(I,J))
                YY2(I,J) = y(I,J) + (0.5D00*DT*YP1(I,J))
            END DO
        END DO

        !RHS2
        CALL RHS(XX2,XP2,YY2,YP2)

        !X3
        DO I = 1,2
            DO J = 1, nop
                XX3(I,J) = x(I,J) + (0.5D00*DT*XP2(I,J))
                YY3(I,J) = y(I,J) + (0.5D00*DT*YP2(I,J))
            END DO
        END DO

        !RHS3
        CALL RHS(XX3,XP3,YY3,YP3)

        !X4
        DO I = 1,2
            DO J = 1, nop
                XX4(I,J) = x(I,J) + (DT*XP3(I,J))
                YY4(I,J) = y(I,J) + (DT*YP3(I,J))
            END DO
        END DO

        !RHS4
        CALL RHS(XX4,XP4,YY4,YP4)

        !Finish x
        DO I = 1,2
            DO J = 1, nop
                XXP(I,J) = (XP1(I,J) + 2.0D00*(XP2(I,J)+XP3(I,J))+XP4(I,J))/6.0D00
                YYP(I,J) = (YP1(I,J) + 2.0D00*(YP2(I,J)+YP3(I,J))+YP4(I,J))/6.0D00
            END DO
        END DO
        DO I = 1,2
            DO J = 1, nop
                x(I,J) = x(I,J) + (DT*XXP(I,J))
                y(I,J) = y(I,J) + (DT*YYP(I,J))
            END DO
        END DO

    END SUBROUTINE