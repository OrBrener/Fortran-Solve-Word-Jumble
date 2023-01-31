
! gfortran -Wall lexicon.f95 solvejumble.f95 && ./a.out

program solvejumble
    ! use lexicon
    implicit none

    !
    !
    !

    integer :: index
    integer :: numWords
    
    character (len=8), allocatable, dimension(:) :: jumbledWords

    call inputJumble(numWords, jumbledWords)

    do index = 1, numWords
        write(*,*) jumbledWords(index)
    end do

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
        write(*,*) 'Enter the jumbled word: '
        do index = 1, numWords
            read(*,*) jumbledWords(index)
        end do
    
        return
    
    end subroutine inputJumble

end program solvejumble