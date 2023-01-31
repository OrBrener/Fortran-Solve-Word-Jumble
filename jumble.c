// Name: Or Brener
// Student #: 1140102

#include "jumble.h"

void swap(char* x, char* y) {
    char temp;
    temp = *x;
    *x = *y;
    *y = temp;
}

/* Function to print permutations of string
This function takes three parameters:
1. String
2. Starting index of the string
3. Ending index of the string. */
void permute(char* input, int leftIndex, int rightIndex) {
    int i;
    if (leftIndex == rightIndex)
        printf("%s\n", input);
    else {
        for (i = leftIndex; i <= rightIndex; i++) {
            swap((input + leftIndex), (input + i));
            permute(input, leftIndex + 1, rightIndex);
            swap((input + leftIndex), (input + i)); // backtrack
        }
    }
}

