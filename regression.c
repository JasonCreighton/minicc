// Regression test for minicc compiler

extern int printf(const char *fmt, ...);

void begin_test(const char *test_name) {
    printf("=== %s ===\n", test_name);
}

void test_comments() {
    // Single line comments should be supported

    begin_test("test_comments"); // Comments should work on the same line as code

    /*
     * Multiline comments are also supported
     */

    printf("// Comments should not be interpreted inside strings.\n");
    printf("/* Comments should not be interpreted inside strings. */\n");
    printf("Nor across /* ... ");
    printf("*/ strings.\n");
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

void test_signed_integer_operations() {
    int x;
    int y;

    begin_test("test_signed_integer_operations");

    for(x = -10; x <= 10; ++x) {
        for(y = -10; y <= 10; ++y) {
            printf("%d + %d = %d\n", x, y, x + y);
            printf("%d - %d = %d\n", x, y, x - y);
            printf("%d * %d = %d\n", x, y, x * y);

            if(y != 0) printf("%d / %d = %d\n", x, y, x / y);
            if(y != 0) printf("%d %% %d = %d\n", x, y, x % y);

            if(y >= 0) printf("%d << %d = %d\n", x, y, x << y);
            if(y >= 0) printf("%d >> %d = %d\n", x, y, x >> y);

            printf("%d & %d = %d\n", x, y, x & y);
            printf("%d | %d = %d\n", x, y, x | y);
            printf("%d ^ %d = %d\n", x, y, x ^ y);

            printf("%d > %d = %d\n", x, y, x > y);
            printf("%d < %d = %d\n", x, y, x < y);
            printf("%d >= %d = %d\n", x, y, x >= y);
            printf("%d <= %d = %d\n", x, y, x <= y);
            printf("%d != %d = %d\n", x, y, x != y);
            printf("%d == %d = %d\n", x, y, x == y);
        }
    }
}

void test_arrays() {
    int ary[10];
    int ary_2d[5][10];
    int i;
    int j;

    begin_test("test_arrays");

    for(i = 0; i < 10; ++i) ary[i] = i - 5;
    for(i = 0; i < 10; ++i) printf("ary[%d] = %d\n", i, ary[i]);

    for(i = 0; i < 5; ++i) {
        for(j = 0; j < 10; ++j) {
            ary_2d[i][j] = i * j;
            printf("ary_2d[%d][%d] = %d\n", i, j, ary_2d[i][j]);
        }
    }
}

void test_pointers() {
    int x = 42;
    int y = 100;
    int *ptr;

    begin_test("test_pointers");

    ptr = &x;

    printf("x=%d, y=%d, *ptr=%d\n", x, y, *ptr);
    *ptr = 50;
    printf("x=%d, y=%d, *ptr=%d\n", x, y, *ptr);
    ptr = &y;
    printf("x=%d, y=%d, *ptr=%d\n", x, y, *ptr);
    *ptr = 500;
    printf("x=%d, y=%d, *ptr=%d\n", x, y, *ptr);
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

    test_comments();
    test_nested_calls();
    test_signed_integer_operations();
    test_arrays();
    test_pointers();

    return 0;
}