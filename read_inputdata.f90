    SUBROUTINE readInputData
    !Read the input file

        USE headerFile
        IMPLICIT NONE
        OPEN(1, FILE = 'inputFile.txt') !Open the file
        READ(1, NML = inputDeck) !Read the data
        WRITE(*, NML = inputDeck) !Write to the screen for user to check
        CLOSE(1) !Close the file

    END SUBROUTINE