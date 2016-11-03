SUBROUTINE Density(xValues, yValues)

    USE headerFile
    IMPLICIT NONE
    REAL(kind = double), DIMENSION(2, nop) :: xValues,yValues

    densities = 0

    DO i=1, nop
        DO j =1, nop
            IF (i /= j) THEN                    
                !Calculate distance between i and j, using nearest image
                ix = xValues(1,i) 
                iy = yValues(1,i)

                jx = xValues(1,j)
                jy = yValues(1,j)

                xDiff = ix - jx
                yDiff = iy - jy 

                IF (topBoundary == 1) THEN
                    yDiff = yDiff - lHeight*NINT(yDiff/lHeight)
                END IF

                IF (nonEquilibrium /= 1 .AND. sideBoundary == 1) THEN
                    xDiff = xDiff - lWidth*NINT(xDiff/lWidth)
                END IF

                r = DSQRT(xDiff**2 + yDiff**2) 

                !Calculate the density
                IF (r < h) THEN
                    CALL Lucy
                    densities(i) = densities(i) + w 
                END IF

            !If it's the same particles, then set r to be 0
            ELSE
                r=0
                w=0
                CALL Lucy
                densities(i) = densities(i) + w 
            END IF
        END DO
    END DO

END SUBROUTINE