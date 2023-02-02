
! gfortran -Wall lexicon.f95 solvejumble.f95 && ./a.out

program solvejumble
    ! use lexicon
    implicit none

    !
    !
    !

    integer :: numWords, wordLen, numAnagrams
    ! integer :: numCircled, circledIndex
    integer :: index, i
    integer :: leftIndex = 1
    integer :: savedAnagrams = 1
    
    character (len=8), allocatable, dimension(:) :: jumbledWords, anagrams
    ! character (len=8), allocatable, dimension(:) :: anagrams
    ! character (len=8) :: anagramFound, finalword, solution

    call inputJumble(numWords, jumbledWords)

    do index = 1, numWords
        ! write(*,*) jumbledWords(index)
        wordLen = len_trim(jumbledWords(index))
        numAnagrams = 1

        do i = 1, wordLen
            numAnagrams = numAnagrams * i
        end do

        allocate(anagrams(numAnagrams))

        call generateAnagram(jumbledWords(index), anagrams,leftIndex,wordLen,savedAnagrams)
        
        do i=1, numAnagrams
            print*,anagrams(i)
        end do

    !   cleanAnagrams

    !     do i = 1, numAnagrams
    !         if (findAnagram(anagrams(i))) then
    !             anagramFound = anagrams(i)
    !             exit
    !         end if
    !     end do
        
    !     write (*,*) anagramFound
    !     write (*,*) 'How many circled letters?'
    !     read (*,*) numCircled

    !     do i = 1, numCircled
    !         write(*,*) 'letter' + i + 'index'
    !         read(*,*) circledIndex
    !         finalword = finalword//anagramFound(circledIndex)
    !     end do

    end do
    ! numAnagrams = 1
    ! wordLen = len_trim(finalword)

    ! do i = 1, wordLen
    !     numAnagrams = numAnagrams * i
    ! end do

    ! call generateAnagram(finalWord, anagrams)
    ! do i = 1, numAnagrams
    !     if (findAnagram(anagrams(i))) then
    !         solution = anagrams(i)
    !         exit
    !     end if
    ! end do

    contains

    subroutine inputJumble(numWords, jumbledWords)
        implicit none
        ! this function takes in numWords number of input strings from the user 
        ! stores the strings in a dynamically allocated array of strings
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
        character (len=8), intent(inout) :: word
        character (len=8) :: temp
        integer, intent(in) ::leftIndex, i


        temp = word(leftIndex:leftIndex)
        word(leftIndex:leftIndex) = word(i:i)
        word(i:i) = temp

        return
    end subroutine swap

    recursive subroutine generateAnagram(word, anagrams, leftIndex, rightIndex, numSavedAnagrams)
        implicit none
        ! 
        character (len=8), intent(inout) :: word
        character (len=8), allocatable, dimension(:), intent(inout) :: anagrams
        integer, intent(inout) :: rightIndex, numSavedAnagrams
        integer, intent(in) ::leftIndex
        integer :: i
    
        if (leftIndex == rightIndex) then
            anagrams(numSavedAnagrams) = word
            numSavedAnagrams = numSavedAnagrams + 1
        else
            do i = leftIndex, rightIndex
                call swap(word, leftIndex, i)
                call generateAnagram(word,anagrams,leftIndex+1,rightIndex,numSavedanagrams)
                call swap(word, leftIndex, i)
            end do
        end if
        
        return
    
    end subroutine generateAnagram

end program solvejumble