
! gfortran -Wall lexicon.f95 solvejumble.f95 && ./a.out

program solvejumble
    use lexicon
    implicit none
    !
    !
    !
    integer :: numWords, wordLen, numAnagrams
    integer :: numCircled, circledIndex
    integer :: index, i
    integer :: leftIndex = 1
    integer :: savedAnagrams = 1
    
    character (len=8), allocatable, dimension(:) :: jumbledWords, anagrams, dictionary
    integer, dimension(27) :: alphabetIndex
    character (len=8) :: foundAnagram = ''
    character (len = 8) :: finalWord = ''

    ! allocate memory for dynmaic array of strings
    allocate(dictionary(88670))
    call buildlexicon(dictionary, alphabetIndex)

    call inputJumble(numWords, jumbledWords)

    do index = 1, numWords
        wordLen = len_trim(jumbledWords(index))
        numAnagrams = 1

        do i = 1, wordLen
            numAnagrams = numAnagrams * i
        end do

        allocate(anagrams(numAnagrams))

        call generateAnagram(jumbledWords(index), anagrams,leftIndex,wordLen,savedAnagrams)

        ! cleanAnagrams()

        call findAnagram(anagrams, numAnagrams, alphabetIndex, dictionary, foundAnagram)

        if (foundAnagram == '') then
            write(*,*) "no match was found"
        else
            write (*,*) foundAnagram
        end if

        write (*,*) 'How many circled letters?'
        read (*,*) numCircled

        do i = 1, numCircled
            write(*,100) i
            100 format ('letter', 1I1, 'index')
            read(*,*) circledIndex
            finalWord = trim(finalWord) // trim(foundAnagram(circledIndex:circledIndex))
        end do

        print*,finalWord
    deallocate(anagrams)
    savedAnagrams = 1
    leftIndex = 1
    foundAnagram = ''
    end do

    wordLen = len_trim(finalWord)
    numAnagrams = 1

    do i = 1, wordLen
        numAnagrams = numAnagrams * i
    end do

    allocate(anagrams(numAnagrams))

    call generateAnagram(finalWord, anagrams,leftIndex,wordLen,savedAnagrams)
    call findAnagram(anagrams, numAnagrams, alphabetIndex, dictionary, foundAnagram)

    write(*,*) 'Fianl solution'
    if (foundAnagram == '') then
        write(*,*) "no match was found"
    else
        write (*,*) foundAnagram
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
            write(*,*) 'How many jumbled words? '
            read(*,*,iostat=inputError) numWords
            if (inputError /= 0) then
                write(*,*) 'wrong input, try again'
            end if
        end do

        ! allocate memory to the string array
        allocate(jumbledWords(numWords))

        ! get user string input
        do index = 1, numWords
            write(*,100) index
            100 format ('jumbled word ', I1, ':')
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