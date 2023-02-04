
! gfortran -Wall lexicon.f95 solvejumble.f95 && ./a.out

program solvejumble
    use lexicon
    implicit none
    !
    ! User inputs jumbled words, program generates their anagrams and finds a match in a dictonary
    ! Then user specifies which words are circled, which creates a final jumbled word and finds a final match 
    !
    integer :: numWords, wordLen, numAnagrams
    integer :: numCircled = 100, circledIndex = 100, leftIndex = 1, savedAnagrams = 1, inputError = 1, index, i
    character (len=8), allocatable, dimension(:) :: jumbledWords, anagrams, dictionary
    integer, dimension(27) :: alphabetIndex
    character (len=8) :: foundAnagram = '', finalWord = ''

    ! Allocate memory for a dynmaic array of strings that stores the dictionary
    allocate(dictionary(88670))
    ! Build the dictionary and an alphabet index
    call buildlexicon(dictionary, alphabetIndex)

    write(*,*) 'Welcome to the jumble solver!'

    ! Get user to input all the jumbled words
    call inputJumble(numWords, jumbledWords)

    ! For all the jumbled words:
    do index = 1, numWords
        ! Reset all necessary variables each loop
        inputError = 1
        wordLen = len_trim(jumbledWords(index))
        numAnagrams = 1
        numCircled = 100

        ! calculate the number of anagrams
        ! formula = factorial(wordLen)
        do i = 1, wordLen
            numAnagrams = numAnagrams * i
        end do

        ! Allocate memory for a dynmaic array of strings that stores the anagrams
        allocate(anagrams(numAnagrams))

        ! Generate the anagrams (recursivley) of the given word
        call generateAnagram(jumbledWords(index), anagrams, leftIndex, wordLen, savedAnagrams)

        ! Search for a match of an anagram using the dictionary 
        ! Store the found anagaram in foundAnagram
        ! If no anagram is found, foundAnagram = ''
        call findAnagram(anagrams, numAnagrams, alphabetIndex, dictionary, foundAnagram)

        ! No anagram is found
        if (foundAnagram == '') then
            write (*,100) index, trim(jumbledWords(index))
            100 format('Jumbled word ' I1, ': ', A, ' = NO MATCH WAS FOUND')
            ! final word does not need to be concatinated
            finalWord = trim(finalWord)
        ! Anagram is found
        else
            write (*,101) index, trim(jumbledWords(index)), trim(foundAnagram)
            101 format('Jumbled word ' I1, ': ', A, ' = ', A)

            ! Inquire how many circled letters in found anagram
            ! user input validation
            do while (inputError /= 0)
                do while (numCircled > len_trim(foundAnagram))
                    
                    write (*,102) trim(foundAnagram)
                    102 format('How many circled letters in ' A, ':')
                    read (*,*,iostat=inputError) numCircled
                    
                    ! validate input is a string
                    if (inputError /= 0) then
                        write(*,*) 'Need to input an integer, try again'
                        cycle
                    end if

                    ! validate the number or circles is >= the number of characters in the word
                    if (numCircled > len_trim(foundAnagram)) then
                        write(*,103) len_trim(foundAnagram) + 1
                        103 format ('Number of circles has to be less than ', I1, ' try again')

                    end if
                end do
            end do

            ! For number of circles times, inquire the index of each circled letter
            do i = 1, numCircled
                ! Reset all necessary variables each loop
                inputError = 1
                circledIndex = 100

                ! user input validation
                do while (inputError /= 0)
                    do while (circledIndex > len_trim(foundAnagram) .or. circledIndex == 0)
                        
                        write(*,104) i, trim(foundAnagram)
                        104 format ('Circled letter ', I1, ' index (starting at 1) in ', A, ':')
                        read(*,*,iostat=inputError) circledIndex
                        
                        ! validate input is a string
                        if (inputError /= 0) then
                            write(*,*) 'Need to input an integer, try again'
                            cycle
                        end if
        
                        ! validate the circle index is within the number of characters in the word
                        if (circledIndex > len_trim(foundAnagram) .or. circledIndex == 0) then
                            write(*,105) len_trim(foundAnagram)
                            105 format('Index has to be between 1 and ', I1, ' try again')
                        end if
                    end do
                end do

                ! final word is the concatination of the chosen circled characters
                finalWord = trim(finalWord) // trim(foundAnagram(circledIndex:circledIndex))
            end do
        end if
        ! End of current jumbled word

        ! Reset all necessary variables each loop
        deallocate(anagrams)
        savedAnagrams = 1
        leftIndex = 1
        foundAnagram = ''
    end do
    ! End of all the jumbled words

    ! Find solution to final jumble (concatination of all circled characters)
    wordLen = len_trim(finalWord)

    ! if there is a final word
    if (wordLen > 0) then
        ! Reset all necessary variables each loop
        numAnagrams = 1

        ! calculate the number of anagrams
        ! formula = factorial(wordLen)
        do i = 1, wordLen
            numAnagrams = numAnagrams * i
        end do

        ! Allocate memory for a dynmaic array of strings that stores the anagrams
        allocate(anagrams(numAnagrams))

        ! Generate the anagrams (recursivley) of the given word
        call generateAnagram(finalWord, anagrams, leftIndex, wordLen, savedAnagrams)

        ! Search for a match of an anagram using the dictionary 
        ! Store the found anagaram in foundAnagram
        ! If no anagram is found, foundAnagram = ''
        call findAnagram(anagrams, numAnagrams, alphabetIndex, dictionary, foundAnagram)

        ! There is no match for the final word anagram
        if (foundAnagram == '') then
            write(*,106) trim(finalWord)
            106 format('Final jumbled word ', A, ' = NO MATCH WAS FOUND')
        ! Final word anagram solved
        else
            write(*,107) trim(finalWord), trim(foundAnagram)
            107 format('Final jumbled word ', A, ' = ' A)
        end if
    
    ! There is no final word (no jumbled words are found)
    else
        write(*,*) 'NO FINAL JUMBLE WORD'
    end if

    contains

    subroutine inputJumble(numWords, jumbledWords)
        implicit none
        !
        ! this function takes in numWords number of input strings from the user 
        ! stores the strings in a dynamically allocated array of strings
        !
        integer, intent(out) :: numWords
        character (len=8), allocatable, dimension(:), intent(out) :: jumbledWords
        integer :: index
        integer :: inputError = 1

        ! validate input
        do while (inputError /= 0)
            write(*,*) 'How many words are jumbled?'
            read(*,*,iostat=inputError) numWords
            if (inputError /= 0) then
                write(*,*) 'Need to input an integer, try again'
            end if
        end do

        ! allocate memory to the string array
        allocate(jumbledWords(numWords))

        ! get user string input
        do index = 1, numWords
            write(*,200) index
            200 format ('Input jumbled word ', I1, ':')
            read(*,*) jumbledWords(index)
        end do
    
        return
    
    end subroutine inputJumble

    subroutine swap(word, leftIndex, i)
        implicit none
        !
        ! swap the word(leftIndex:leftIndex) with word(i:i)
        !
        character (len=8), intent(inout) :: word
        character (len=8) :: temp
        integer, intent(in) :: leftIndex, i


        temp = word(leftIndex:leftIndex)
        word(leftIndex:leftIndex) = word(i:i)
        word(i:i) = temp

        return
    end subroutine swap

    recursive subroutine generateAnagram(word, anagrams, leftIndex, rightIndex, numSavedAnagrams)
        implicit none
        ! 
        ! generates all the anagrams of a given word
        ! uses a recursive algorithm
        ! algorithm inspired from: https://www.geeksforgeeks.org/write-a-c-program-to-print-all-permutations-of-a-given-string/
        !
        character (len=8), intent(inout) :: word
        character (len=8), allocatable, dimension(:), intent(inout) :: anagrams
        integer, intent(inout) :: rightIndex, numSavedAnagrams
        integer, intent(in) ::leftIndex
        integer :: i
    
        ! anagram found
        if (leftIndex == rightIndex) then
            ! save it it in the anagrams array
            anagrams(numSavedAnagrams) = word
            numSavedAnagrams = numSavedAnagrams + 1
        else
            ! loop through all the characters between the indices
            do i = leftIndex, rightIndex
                ! swap the substring
                call swap(word, leftIndex, i)
                ! recursive call
                call generateAnagram(word,anagrams,leftIndex+1,rightIndex,numSavedanagrams)
                ! backtracing
                call swap(word, leftIndex, i)
            end do
        end if
        
        return
    
    end subroutine generateAnagram

    subroutine findAnagram(anagrams, numAnagrams, alphabetIndex, dictionary, foundAnagram)
        implicit none
        !
        ! return the found anagram, given a list of anagrams and a dictionary
        !
        character (len=8), allocatable, dimension(:), intent(in):: anagrams, dictionary
        integer, intent(in) :: numAnagrams
        integer, dimension(27), intent(in) :: alphabetIndex
        character (len = 8), intent(out) :: foundAnagram
        integer :: i
        logical :: result = .false.

        ! for all anagrams, search if they exist in the dictionary
        do i = 1, numAnagrams
            call findlex(anagrams(i), alphabetIndex, dictionary, result)
            ! if it exsits, return it
            if (result) then
                foundAnagram = anagrams(i)
                exit
            end if
        end do


        return
    end subroutine findAnagram

end program solvejumble