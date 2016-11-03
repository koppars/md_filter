SUBROUTINE initialisePositions
    !Initialise the positions to create a square lattice. Origin at 0

    USE headerFile
    IMPLICIT NONE
    INTEGER :: nopCreated,numberOfRows,nopCreatedOnThisRow,numberOfRowsCreated,nopPerColumn

    numberOfRowsCreated = 0
    nopCreated = 0
    nopPerColumn = nop/nopPerRow

    DO WHILE (nopCreated < nop)

        nopCreatedOnThisRow = 0

        DO WHILE (nopCreatedOnThisRow < nopPerRow)

            x(1,nopCreated + 1) = 0.0 - (lWidth/2.0) + (lWidth/(nopPerRow*2.0))&
            & + ((lWidth/nopPerRow)*nopCreatedOnThisRow)

            y(1,nopCreated + 1) = 0.0 + (lHeight/2.0) - (lHeight/(nopPerColumn*2.0))&
            & - ((lHeight/nopPerColumn)*numberOfRowsCreated)

            nopCreatedOnThisRow = nopCreatedOnThisRow + 1
            nopCreated = nopCreated + 1

        END DO

        numberOfRowsCreated = numberOfRowsCreated + 1

    END DO

    !Write them to a file
    OPEN(26, FILE = 'startingConfig.xyz')
    WRITE(26,*) nop
    WRITE(26,*) 'COMMENT'
    DO i=1, nop
        WRITE(26,*) 'H', x(1,i), y(1,i), 0.00
    END DO

END SUBROUTINE