! buildLexicon
! words: array of strings -- dynamically allocated with all the strings in the file
! alphabetIndex keep a array of integers with the line numbers for each new letter ('A', 'B', ... 'Z')

! anagramLetterToIndex
! 'a' || 'A' = 1
! 'b' || 'B' = 2

! findlex
! do i = alphabetIndex(anagramLetterToIndex(anagram(1))), alphabetIndex(anagramLetterToIndex(anagram(1))+1)
!     if (anagram == words(i)) then
!         lex = true
!     end if
! end do