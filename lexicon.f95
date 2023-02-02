module lexicon
    implicit none

    contains

    subroutine buildlexicon(dictionary, alphabetIndex)
        implicit none
        !
        ! opens the file dict2.txt, reads all the lines,
        ! stores all the words in the dynmaic array fo strings called dictionary
        ! alphabetIndex keep an array of integers with the line numbers for each new letter ('A', 'B', ... 'Z')
        ! alphabetIndex(27) = the last line number (last 'z' word)
        !
        logical :: lexist
        character (len = 20) :: fname = "dict2.txt" 
        character (len = 8) :: line
        character (len = 8), allocatable, dimension(:), intent(inout) :: dictionary
        integer, dimension(27), intent(out) :: alphabetIndex
        integer :: lineNum = 1
        
        ! check if file exists & open file
        inquire(file=fname, exist=lexist)
        
        if (lexist) then
            open(unit=9,file=fname,status='old',action='read')
        else
            write (*,*) 'file does not exit - abort'
            return
        end if
        
        ! loop until end of file
        do while (1 == 1)
            ! read and store the line in the dictionary
            read (9,*,End=200) line
            dictionary(lineNum) = line
            
            ! store the line numbers for each character in the alphabet
            select case (line)
            
                case ('A')
                    alphabetIndex(1) = lineNum
                case ('B')
                    alphabetIndex(2) = lineNum
                case ('C')
                    alphabetIndex(3) = lineNum
                case ('D')
                    alphabetIndex(4) = lineNum
                case ('E')
                    alphabetIndex(5) = lineNum
                case ('F')
                    alphabetIndex(6) = lineNum
                case ('G')
                    alphabetIndex(7) = lineNum
                case ('H')
                    alphabetIndex(8) = lineNum
                case ('I')
                    alphabetIndex(9) = lineNum
                case ('J')
                    alphabetIndex(10) = lineNum
                case ('K')
                    alphabetIndex(11) = lineNum
                case ('L')
                    alphabetIndex(12) = lineNum
                case ('M')
                    alphabetIndex(13) = lineNum
                case ('N')
                    alphabetIndex(14) = lineNum
                case ('O')
                    alphabetIndex(15) = lineNum
                case ('P')
                    alphabetIndex(16) = lineNum
                case ('Q')
                    alphabetIndex(17) = lineNum
                case ('R')
                    alphabetIndex(18) = lineNum
                case ('S')
                    alphabetIndex(19) = lineNum
                case ('T')
                    alphabetIndex(20) = lineNum
                case ('U')
                    alphabetIndex(21) = lineNum
                case ('V')
                    alphabetIndex(22) = lineNum
                case ('W')
                    alphabetIndex(23) = lineNum
                case ('X')
                    alphabetIndex(24) = lineNum
                case ('Y')
                    alphabetIndex(25) = lineNum
                case ('Z')
                    alphabetIndex(26) = lineNum

            end select

            lineNum = lineNum + 1
        end do
        ! end of file, can close the file
        200 continue
        close(9)

        ! store the last word's line number
        alphabetIndex(27) = lineNum

        return
    end subroutine buildlexicon

    integer function letterToIndex(input)
        implicit none
        !
        ! given a character (case in-sensitive), return it's integer value
        ! ex:  'a' || 'A' = 1, 'b' || 'B' = 2
        !
        character :: input
        select case (input)

            case ('A')
                letterToIndex = 1
            case ('a')
                letterToIndex = 1
            case ('B')
                letterToIndex = 2
            case ('b')
                letterToIndex = 2
            case ('C')
                letterToIndex = 3
            case ('c')
                letterToIndex = 3
            case ('D')
                letterToIndex = 4
            case ('d')
                letterToIndex = 4
            case ('E')
                letterToIndex = 5
            case ('e')
                letterToIndex = 5
            case ('F')
                letterToIndex = 6
            case ('f')
                letterToIndex = 6
            case ('G')
                letterToIndex = 7
            case ('g')
                letterToIndex = 7
            case ('H')
                letterToIndex = 8
            case ('h')
                letterToIndex = 8
            case ('I')
                letterToIndex = 9
            case ('i')
                letterToIndex = 9
            case ('J')
                letterToIndex = 10
            case ('j')
                letterToIndex = 10
            case ('K')
                letterToIndex = 11
            case ('k')
                letterToIndex = 11
            case ('L')
                letterToIndex = 12
            case ('l')
                letterToIndex = 12
            case ('M')
                letterToIndex = 13
            case ('m')
                letterToIndex = 13
            case ('N')
                letterToIndex = 14
            case ('n')
                letterToIndex = 14
            case ('O')
                letterToIndex = 15
            case ('o')
                letterToIndex = 15
            case ('P')
                letterToIndex = 16
            case ('p')
                letterToIndex = 16
            case ('Q')
                letterToIndex = 17
            case ('q')
                letterToIndex = 17
            case ('R')
                letterToIndex = 18
            case ('r')
                letterToIndex = 18
            case ('S')
                letterToIndex = 19
            case ('s')
                letterToIndex = 19
            case ('T')
                letterToIndex = 20
            case ('t')
                letterToIndex = 20
            case ('U')
                letterToIndex = 21
            case ('u')
                letterToIndex = 21
            case ('V')
                letterToIndex = 22
            case ('v')
                letterToIndex = 22
            case ('W')
                letterToIndex = 23
            case ('w')
                letterToIndex = 23
            case ('X')
                letterToIndex = 24
            case ('x')
                letterToIndex = 24
            case ('Y')
                letterToIndex = 25
            case ('y')
                letterToIndex = 25
            case ('Z')
                letterToIndex = 26
            case ('z')
                letterToIndex = 26

        end select

    end function letterToIndex

    subroutine findlex(word, alphabetIndex, dictionary, result)
        implicit none
        !
        ! search the dictionary and return true/false if the word was found
        !
        character (len = 8), intent(in) :: word
        character (len = 8), allocatable, dimension(:), intent(in) :: dictionary
        integer, dimension(27), intent(in) :: alphabetIndex
        logical, intent(out) :: result
        integer :: lineNum

        ! loop the dictionary 
        ! only go through the strings that start with the same first character is the input word 
        do lineNum = alphabetIndex(letterToIndex(word(1:1))), alphabetIndex(letterToIndex(word(1:1))+1)
            ! if the word is found, return true
            if (word == dictionary(lineNum)) then
                result = .true.
                exit
            ! otherwise return false
            else
                result = .false.
            end if
        end do
        
        return
    end subroutine findlex

end module lexicon