#ifndef HELPER_H
#define HELPER_H

/**
 * Helper header file for ficus integration testing
 * Contains function declarations and common patterns
 * that might be detected by snippet analysis.
 */

// Function declarations
int helper_function(int value);
void process_data(const char* input, char* output, size_t max_len);
double calculate_average(int* numbers, int count);

// Common macros that might be fingerprinted
#define MAX_BUFFER_SIZE 1024
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))

// Struct definition
typedef struct {
    int id;
    char name[64];
    double value;
} data_record_t;

// Function-like macro
#define SAFE_FREE(ptr) do { \
    if ((ptr) != NULL) { \
        free(ptr); \
        (ptr) = NULL; \
    } \
} while(0)

#endif // HELPER_H