// Name: Or Brener
// Student #: 1140102

// make + execute command:
// make clean && make && echo $'\n\nProgram Output:\n' && ./a1

#include "jumble.h"
 
/* Driver code */
int main() {
    
    char str[] = "ABC";
    int n = strlen(str);
    permute(str, 0, n - 1);
    return 0;
}
