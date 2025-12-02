#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "helper.h"

/**
 * Implementation file for helper functions
 * Contains common C patterns for snippet analysis
 */

int helper_function(int value) {
    // Simple mathematical operation
    if (value < 0) {
        return -1;
    }
    
    // Common algorithm pattern
    int result = 1;
    for (int i = 1; i <= value; i++) {
        result += i * 2;
    }
    
    return result % 1000;  // Keep result manageable
}

void process_data(const char* input, char* output, size_t max_len) {
    if (input == NULL || output == NULL || max_len == 0) {
        return;
    }
    
    // String processing pattern
    size_t input_len = strlen(input);
    size_t copy_len = MIN(input_len, max_len - 1);
    
    strncpy(output, input, copy_len);
    output[copy_len] = '\0';
    
    // Convert to uppercase
    for (size_t i = 0; i < copy_len; i++) {
        if (output[i] >= 'a' && output[i] <= 'z') {
            output[i] = output[i] - 'a' + 'A';
        }
    }
}

double calculate_average(int* numbers, int count) {
    if (numbers == NULL || count <= 0) {
        return 0.0;
    }
    
    long long sum = 0;
    for (int i = 0; i < count; i++) {
        sum += numbers[i];
    }
    
    return (double)sum / count;
}