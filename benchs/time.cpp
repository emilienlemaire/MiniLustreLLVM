#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(int argc, char** argv) {

    if (argc < 2) {
        printf("Usage:: ./time input_program_path");
        exit(EXIT_FAILURE);
    }

    struct timespec t0, t1;

    if(timespec_get(&t0, TIME_UTC) != TIME_UTC) {
        printf("Error in calling timespec_get\n");
        exit(EXIT_FAILURE);
    }

    system(argv[1]);

    if(timespec_get(&t1, TIME_UTC) != TIME_UTC) {
        printf("Error in calling timespec_get\n");
        exit(EXIT_FAILURE);
    }

    // Calculate the elapsed time
    double diff = (double)(t1.tv_sec - t0.tv_sec) + ((double)(t1.tv_nsec - t0.tv_nsec)/1000000000L);
    printf("Elapsed time: %lf seconds\n", diff);
    return 0;
}
