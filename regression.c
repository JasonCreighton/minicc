extern int printf(const char *fmt, ...);

void begin_test(const char *test_name) {
    printf("=== %s ===\n", test_name);
}

int func3() {
    printf("func3\n");

    return 0;
}

int func2() {
    printf("func2\n");
    return func3();
}

int func1() {
    printf("func1\n");
    return func2();
}

int sub(int x, int y) {
    return x - y;
}

void test_nested_calls() {
    begin_test("test_nested_calls");

    printf("func1() = %d\n", func1());
}

int main() {
    unsigned char w;
    short x = 42;
    int y = 15;
    long z = 100;

    begin_test("main");

    w = 250;

    printf("Hello, world!\n");
    printf("Numbers: %d, %d, %d, %ld\n", w, x, y, z);
    printf("x == 42 = %d\n", x == 42);
    printf("y != 15 = %d\n", y != 15);
    printf("y++: %d\n", y++);
    printf("++y: %d\n", ++y);
    printf("x << 3 = %d\n", x << 3);
    printf("y >> 1 = %d\n", y >> 1);
    printf("x > y = %d\n", x > y);
    printf("x < y = %d\n", x < y);
    printf("sub(x, y) = %d\n", sub(x, y));
    printf(" Call printf with temporaries = %d\n", printf("X") + printf("XX") + printf("XXX") + printf("XXXX"));

    printf("testing\n"), printf("comma\n"), printf("operator\n");    

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

    test_nested_calls();

    return 0;
}