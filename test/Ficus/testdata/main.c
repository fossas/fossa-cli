#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "helper.h"

/* 
 * Simple C program for ficus snippet analysis testing
 * This file contains various C constructs that might be detected
 * by ficus fingerprinting algorithms.
 */

int main(int argc, char *argv[]) {
    printf("Hello, World!\n");
    
    // String manipulation that might be fingerprinted
    char buffer[256];
    strncpy(buffer, "Test string for analysis", sizeof(buffer) - 1);
    buffer[sizeof(buffer) - 1] = '\0';
    
    // Function call to helper
    int result = helper_function(42);
    
    // Memory allocation pattern
    int *data = malloc(10 * sizeof(int));
    if (data != NULL) {
        for (int i = 0; i < 10; i++) {
            data[i] = i * i;
        }
        free(data);
    }
    
    printf("Result: %d\n", result);
    return EXIT_SUCCESS;
}