int main() {
    unsigned char w;
    short x = 42;
    int y = 15;
    long z = 100;

    w = 250;

    printf("Hello, world!\n");
    printf("Numbers: %d, %d, %d, %ld\n", w, x, y, z);
    printf("y++: %d\n", y++);
    printf("++y: %d\n", ++y);
    

    w = w + 30;
    printf("Wraparound: %d\n", w);

    if(x) {
        printf("x is true\n");
    }

    if(y) {
        printf("y is true (should not happen)\n");
    } else {
        printf("y is false\n");
    }

    x = 4;
    while(x) {
        y = 6;
        while(y) {
            printf("x=%d, y=%d\n", x, y);
            y = y - 1;
        }
        x = x - 1;
    }
}