SUBROUTINE initialiseVelocities
!Initialise the velocities - random and so the total = 32

    USE headerFile
    IMPLICIT NONE
    REAL(kind = double), DIMENSION(nop) :: xVelocities, yVelocities
    INTEGER, DIMENSION (12) :: seed = (1000)
    REAL(kind = double) :: totalYVelocity, totalXVelocity, totalYVelocitySquared, totalXVelocitySquared

    IF (initaliseVels == 1) THEN
        intx = 0.0
        inty = 0.0
        totalXVelocity = 0.0
        totalYVelocity = 0.0
        totalXVelocitySquared = 0.0
        totalYVelocitySquared = 0.0

        ! Give them random velocities between 0 and 1
        DO i=1, nop
            CALL randomNumber
            xVelocities(i) = randNum
            CALL randomNumber
            yVelocities(i) = randNum
        END DO

        ! Take off 0.5 to make some negative
        DO i=1, nop
            xVelocities(i) = xVelocities(i) - 0.5
            yVelocities(i) = yVelocities(i) - 0.5
        END DO

        DO i=1, nop
            totalXVelocity = xVelocities(i) + totalXVelocity
            totalYVelocity = yVelocities(i) + totalYVelocity
        END DO

        !Vanishing center of mass velocity
        !vi = vi - (1/16*total vel)
        DO i=1, nop
            xVelocities(i) = xVelocities(i)-((1.0/16.0)*totalXVelocity)
            yVelocities(i) = yVelocities(i)-((1.0/16.0)*totalYVelocity)
        END DO

        !Initial kinetic energy        
        !vi = DSQRT(32/total vel^2)
        DO i=1, nop
            totalXVelocitySquared = xVelocities(i)**2 + totalXVelocitySquared
            totalYVelocitySquared = yVelocities(i)**2 + totalYVelocitySquared
        END DO

        DO i=1, nop
            xVelocities(i) = xVelocities(i)*(DSQRT(1.0/(totalXVelocitySquared)))
            yVelocities(i) = yVelocities(i)*(DSQRT(1.0/(totalYVelocitySquared)))
        END DO

        DO i=1, nop
            x(2,i) = xVelocities(i)
            y(2,i) = yVelocities(i)
        END DO

    ELSE
        DO i=1, nop
            x(2,i) = 0
            y(2,i) = 0
        END DO
    END IF

END SUBROUTINE
