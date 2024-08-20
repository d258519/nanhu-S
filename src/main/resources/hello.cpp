#include "svdpi.h"
#include "stdio.h"
extern "C" void hello() {
    printf("Hello DPI-C\n");
    auto fp = fopen("./hello.txt","a+");
    fprintf(fp,"Hello DPI-C\n");
    fclose(fp);
}