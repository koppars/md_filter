SUBROUTINE initialiseUnitCell
!Initalise the shape of the unit cell, and write it out to a file for VMD

    USE headerFile
    IMPLICIT NONE
    REAl(kind = double) :: xPosition, yPosition

    !Calculate lattice height and width from number of particles and denisty (and shape!)
    !Square
    IF (shape == 0) THEN
        lWidth = SQRT(nop/p0)
        !WRITE(*,*)lWidth
        lHeight = lWidth
        !WRITE(*,*)lHeight
        nopPerRow = nop**0.5
        !WRITE(*,*)nopPerRow
    !Column
    ELSE
        lWidth = SQRT(nop/p0)/2.0
        lHeight = SQRT(nop/p0)*2.0
        nopPerRow = (nop**0.5)/2.0
        !WRITE(*,*)'lheight', lHeight
        !WRITE(*,*)'lwidth: ', lWidth
        !WRITE(*,*)'Nopperrow: ', nopPerRow
    END IF

    unitCellXPositions(1) = 0.0 + (lWidth/2.0)
    unitCellYPositions(1) = 0.0 + (lHeight/2.0)

    unitCellXPositions(2) = 0.0 - (lWidth/2.0)
    unitCellYPositions(2) = 0.0 + (lHeight/2.0)

    unitCellXPositions(3) = 0.0 - (lWidth/2.0)
    unitCellYPositions(3) = 0.0 - (lHeight/2.0)

    unitCellXPositions(4) = 0.0 + (lWidth/2.0)
    unitCellYPositions(4) = 0.0 - (lHeight/2.0)

    !Record the coordinates to a file to draw out the unit cell
    OPEN(11, FILE = 'resultsAnimationUnitCell.xyz')

    !Set up animation file
    WRITE(11,*) '400'
    WRITE(11,*) 'Test comment'

    DO i=1, 100

        xPosition = unitCellXPositions(1) - (((unitCellXPositions(1)-unitCellXPositions(2))/100.0)*i)
        yPosition = unitCellYPositions(1)

        WRITE(11,*) 'N ', xPosition, ' ', yPosition, ' 0.00'
        !WRITE(*,*) i

    END DO

    DO i=1, 100

        xPosition = unitCellXPositions(2)
        yPosition = unitCellYPositions(2) - (((unitCellYPositions(2)-unitCellYPositions(3))/100.0)*i)

        WRITE(11,*) 'N ', xPosition, ' ', yPosition, ' 0.00'
        !WRITE(*,*) i

    END DO

    DO i=1, 100

        xPosition = unitCellXPositions(3) - (((unitCellXPositions(3)-unitCellXPositions(4))/100.0)*i)
        yPosition = unitCellYPositions(3)

        WRITE(11,*) 'N ', xPosition, ' ', yPosition, ' 0.00'
        !WRITE(*,*) i

    END DO

    DO i=1, 100

        xPosition = unitCellXPositions(4)
        yPosition = unitCellYPositions(4) - (((unitCellYPositions(4)-unitCellYPositions(1))/100.0)*i)

        WRITE(11,*) 'N ', xPosition, ' ', yPosition, ' 0.00'
        !WRITE(*,*) i

    END DO


END SUBROUTINE