
! gfortran -Wall lexicon.f95 solvejumble.f95 && ./a.out

subroutine inputJumble(jumbleWord)
    implicit none
    !
    character (len=8), intent(inout) :: jumbleWord
    write(*,*) 'Enter the jumbled word: '
    read(*,*) jumbleWord

    return

end subroutine inputJumble

program solvejumble
    !
    !
    !

    integer :: numWords
    character (len=8) :: jumbleWord

    write(*,*) 'How many jumbled words? '
    read(*,*) numWords

    do index = 1, numWords
        call inputJumble(jumbleWord)
        write(*,*) jumbleWord
    end do

end program solvejumble