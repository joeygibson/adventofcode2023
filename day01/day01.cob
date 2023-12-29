        IDENTIFICATION DIVISION.
        PROGRAM-ID. Day01.
        author. Joey Gibson.
        DATE-WRITTEN. 2023-12-29.

      * This is a solution for Day 1 of the Advent of Code 2023. So
      * far, I've only done the first part.
      *
      * This code works with GNU COBOL, and _may_ work with others, but
      * I haven't tested it.

        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT F-input-file ASSIGN TO WS-FILE-NAME
              ORGANIZATION IS LINE SEQUENTIAL.

        DATA DIVISION.
        FILE SECTION.
        FD  F-input-file.
        01  F-input-record.
            05  F-input-record-data PIC X(80).

        WORKING-STORAGE SECTION.
        01  WS-file-name PIC X(100).
        01  WS-args-count PIC 9(4) COMP.
        01  WS-Total PIC 9(10).
        01  WS-Num-As-String PIC X(10).
        01  WS-eof PIC X VALUE "N".
        01  WS-Index PIC 9(10) VALUE 1.
        01  WS-Digit-Found PIC X VALUE "N".
        01  WS-Formatted-Int PIC Z(9)9.

        PROCEDURE DIVISION.
            accept WS-ARGS-COUNT from argument-number.
                perform WS-ARGS-COUNT times
                    accept WS-FILE-NAME from argument-value
                end-perform

                display 'file name: ' WS-FILE-NAME
            OPEN INPUT F-input-file.
            perform until WS-eof = "Y"
                READ F-INPUT-FILE
                    AT END
                        MOVE "Y" TO WS-eof
                    NOT AT END
                      perform varying WS-Index from 1 by 1
                        until F-input-record(WS-Index:1) = SPACE
      
                        if F-input-record(WS-Index:1) is NUMERIC
                          if WS-Digit-Found = "N"
                            Move F-input-record(WS-Index:1) to
                              WS-Num-As-String(1:1)
                            Move "Y" to WS-Digit-Found
                          else
                            Move F-input-record(WS-Index:1) to
                              WS-Num-As-String(2:1)
                          end-if
                        end-if
                      end-perform

                    if WS-Num-As-String(2:1) = SPACE
                      move WS-Num-As-String(1:1) 
                        to WS-Num-As-String(2:1)
                    end-if

                    add function NUMVAL(WS-Num-As-String) to WS-Total

                    move SPACES to WS-Num-As-String
                    move 'N' to WS-Digit-Found
                END-READ
            END-PERFORM.
            close F-INPUT-FILE.

            move WS-TOTAL to WS-Formatted-Int.
            display "part1 -> " WS-Formatted-Int.
            stop run.
