SUBROUTINE boundaryConditions
!Boundary conditions: periodic, elastic, stone wall
    USE headerFile
    IMPLICIT NONE
    REAL(kind = double) :: particleToCheck

    DO I=1, nop

        !Side boundaries based on input file            
        !Periodic x
        IF (nonEquilibrium /= 1 .AND. sideBoundary == 1) THEN
            x(1,I) = x(1,I) - lWidth*NINT(x(1,I)/lWidth)
        !Elastic x
        ELSE IF (nonEquilibrium /= 1 .AND. sideBoundary == 2) THEN

            particleToCheck = x(1,i)

            !Left boundary
            IF (particleToCheck < -lWidth/2.0) THEN
                !Reverse velocity
                IF (x(2,i) < 0) THEN
                    x(2,i) = -x(2,I)
                END IF
            END IF

            !Right boundary
            IF (particleToCheck > lWidth/2.0) THEN
                !Reverse velocity
                IF (x(2,i) > 0) THEN
                    x(2,i) = -x(2,i)
                END IF
            END IF

        !Stone wall
        ELSE IF (nonEquilibrium /= 1 .AND. sideBoundary == 3) THEN

            particleToCheck = x(1,i)

            !Left boundary
            IF (particleToCheck < -lWidth/2.0) THEN
                x(2,i) = 0
            END IF

            !Right boundary
            IF (particleToCheck > lWidth/2.0) THEN
                !Remove x velocity
                x(2,i) = 0
            END IF

        END IF

        !Bottom and top boundary based on input file
        !Periodic Y
        IF (topBoundary == 1) THEN
            y(1,I) = y(1,I) - lHeight*NINT(y(1,I)/lHeight)      

        !Elastic Y
        ELSE IF (topBoundary == 2) THEN
            particleToCheck = y(1,I)
            !Top boundary
            IF (particleToCheck > (lHeight/2.0)) THEN
                !Reverse velocity
                IF (y(2,I) > 0) THEN
                    y(2,I) = -y(2,I)
                END IF
            END IF           
            !if we are filtering don't use the bottom boundary if we are past the filter limit
            IF (filter == 1 .and. timeStep < equilibriumSteps) THEN 
                !Bottom boundary
                IF (particleToCheck < -(lHeight/2.0)) THEN
                    !Reverse velocity
                    IF (y(2,I) < 0) THEN
                        y(2,I) = -y(2,I)
                    END IF
                END IF
            END IF
            !if we aren't filtering use the bottom boundary
            IF (filter /= 1) THEN 
                !Bottom boundary
                IF (particleToCheck < -(lHeight/2.0)) THEN

                    !Reverse velocity
                    IF (y(2,I) < 0) THEN
                        y(2,I) = -y(2,I)
                    END IF
                END IF
            END IF
        END IF

    END DO

END SUBROUTINE