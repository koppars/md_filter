SUBROUTINE RHS(XX,XP,YY,YP)
!RHS of RK4 scheme        

    USE headerFile
    IMPLICIT NONE
    REAL(kind = double), DIMENSION(2, nop) ::  XX,XP,YY,YP
    REAL(kind = double), DIMENSION(nop) :: xEmbeddedAtomForces,yEmbeddedAtomForces !FOR SPAM
    REAL(kind = double), DIMENSION(:), ALLOCATABLE :: grhox, grhoy


    ALLOCATE(grhox(nop))
    ALLOCATE(grhoy(nop))

    ix = 0.0
    iy = 0.0
    jx = 0.0
    jy = 0.0

    !If it's embedded atom then calculate densities
    IF (fluidPotential == 2 .or. fluidPotential == 3) THEN
        CALL Density(XX,YY)
    END IF

    DO i=1, nop
        XP(1,i) = XX(2,i)
        XP(2,i) = 0.0
        YP(1,i) = YY(2,i)
        YP(2,i) = 0.0
        xEmbeddedAtomForces(I) = 0.0
        yEmbeddedAtomForces(I) = 0.0
    END DO

    !MD STUFF (if r < 1 they repel)
    DO i=1, nop            
        DO j=1, nop
            IF (i /= j) THEN

                !WRITE(*,*) 'i = ', i
                !WRITE(*,*) 'j = ', j

                ix = XX(1,i)
                iy = YY(1,i)

                jx = XX(1,j)
                jy = YY(1,j)

                xDiff = ix - jx
                yDiff = iy - jy 

                IF (topBoundary == 1) THEN                        
                    yDiff = yDiff - lHeight*NINT(yDiff/lHeight)
                END IF

                IF (nonEquilibrium /= 1 .AND. sideBoundary == 1) THEN
                    xDiff = xDiff - lWidth*NINT(xDiff/lWidth)
                END IF

                r = DSQRT(xDiff**2 + yDiff**2)       

                !MD                    
                IF (r < 1) THEN

                    !Core repulsive
                    IF (fluidPotential == 1 .or. fluidPotential == 3) THEN
                        XP(2,i) = XP(2,i) + (8.0*r*((1.0-r**2)**3)*(xDiff/r)) !Calculate forces
                        YP(2,i) = YP(2,i) + (8.0*r*((1.0-r**2)**3)*(yDiff/r)) !Calculate forces 
                    END IF

                    !Embedded atom
                    IF (fluidPotential == 2 .or. fluidPotential == 3) THEN
                        xEmbeddedAtomForces(i) = xEmbeddedAtomForces(i) + &
                        &((((2.0-(densities(i)/p0)-(densities(j)/p0))*wp)*(xDiff/r)))

                        yEmbeddedAtomForces(i) = yEmbeddedAtomForces(i) + &
                        &((((2.0-(densities(i)/p0)-(densities(j)/p0))*wp)*(yDiff/r)))
                    END IF       

                END IF

            END IF                        
        END DO    
    END DO

    !ADD gravity to Y Velocity
    DO i=1, nop
        YP(2,i) = YP(2,i) + g
    END DO

    !Add forces for interactions with scatterers
    !MD STUFF (if r < 1 they repel)
    !Scatterers don't move
    !!!!DISPLACE THE FORCE!!!!
    IF (filter == 1 .and. timeStep > equilibriumSteps) THEN
        DO i=1, nop
            DO j=1, nos
                ix = XX(1,i)
                iy = YY(1,i)

                jx = scatterersX(j)
                jy = scatterersY(j)

                xDiff = ix -jx
                yDiff = iy - jy

                IF (topBoundary == 1) THEN
                    yDiff = yDiff - lHeight*NINT(yDiff/lHeight)
                END IF

                IF (nonEquilibrium /= 1 .AND. sideBoundary == 1) THEN
                    xDiff = xDiff - lWidth*NINT(xDiff/lWidth)
                END IF

                r = DSQRT(xDiff**2 + yDiff**2)

                !MD FORCES
                IF (r < 1) THEN

                    !Core repulsive
                    IF (filterPotential == 1) THEN
                        XP(2,i) = XP(2,i) + (8.0*r*((1.0-r**2)**3)*(xDiff/r)) !Calculate forces
                        YP(2,i) = YP(2,i) + (8.0*r*((1.0-r**2)**3)*(yDiff/r)) !Calculate forces

                    !****SHOULD ONLY USE CORE REPULSIVE****
                    !Embedded atom
                   !ELSE IF (filterPotential == 2) THEN
                   !    xEmbeddedAtomForces(i) = xEmbeddedAtomForces(i) + &
                   !    &((((2.0-(densities(i)/p0)-(densities(j)/p0))*wp)*(xDiff/r)))

                   !    yEmbeddedAtomForces(i) = yEmbeddedAtomForces(i) + &
                   !    &((((2.0-(densities(i)/p0)-(densities(j)/p0))*wp)*(yDiff/r)))
                   END IF       

                END IF
            END DO
        END DO
    END IF

    !Add Embedded atom forces
    IF (fluidPotential == 2 .or. fluidPotential == 3) THEN
        DO i=1, nop
            xEmbeddedAtomForces(i) = (b0/(p0**2))*xEmbeddedAtomForces(i)
            XP(2,i) = XP(2,i) + xEmbeddedAtomForces(i)

            yEmbeddedAtomForces(i) = (b0/(p0**2))*yEmbeddedAtomForces(i)
            YP(2,i) = YP(2,i) + yEmbeddedAtomForces(i)
        END DO
    END IF

END SUBROUTINE