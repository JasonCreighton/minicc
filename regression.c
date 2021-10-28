// Regression test for minicc compiler
//
// Should be compiled with -fwrapv or similar, because minicc wraps on
// overflow.

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

void test_evaluation_width() {
    // Almost all evaluation in C is done at the width of an int or wider
    begin_test("test_evaluation_width");
    const unsigned char UCHAR_MAX = 255;
    const unsigned short USHORT_MAX = 65535;
    const unsigned int UINT_MAX = 4294967295;
    const unsigned long ULONG_MAX = 18446744073709551615UL;
    const signed char SCHAR_MIN = -128;
    const signed char SCHAR_MAX = 127;
    const signed short SHRT_MIN = -32768;
    const signed short SHRT_MAX = 32767;
    const signed int INT_MIN = -2147483648;
    const signed int INT_MAX = 2147483647;
    const signed long LONG_MIN = -9223372036854775807 - 1;
    const signed long LONG_MAX = 9223372036854775807;

    // Validate that arithmetic gets promoted to int even when literals are
    // not used in the expression.
    const unsigned char uchar_one = 1;
    const unsigned short ushort_one = 1;
    printf("UCHAR_MAX + uchar_one = %d\n", UCHAR_MAX + uchar_one);
    printf("USHORT_MAX + ushort_one = %d\n", USHORT_MAX + ushort_one);

    printf("UINT_MAX + 1 = %d\n", UINT_MAX + 1);
    printf("ULONG_MAX + 1 = %ld\n", ULONG_MAX + 1);

    printf("(SCHAR_MIN - 1) / 2 = %d\n", (SCHAR_MIN - 1) / 2);
    printf("(SCHAR_MAX + 1) / 2 = %d\n", (SCHAR_MAX + 1) / 2);
    printf("(SHRT_MIN - 1) / 2 = %d\n", (SHRT_MIN - 1) / 2);
    printf("(SHRT_MAX + 1) / 2 = %d\n", (SHRT_MAX + 1) / 2);
    printf("(INT_MIN - 1) / 2 = %d\n", (INT_MIN - 1) / 2);
    printf("(INT_MAX + 1) / 2 = %d\n", (INT_MAX + 1) / 2);
    printf("(LONG_MIN - 1) / 2 = %ld\n", (LONG_MIN - 1) / 2);
    printf("(LONG_MAX + 1) / 2 = %ld\n", (LONG_MAX + 1) / 2);
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
    int test_vector[17];

    test_vector[0] = 0;
    test_vector[1] = 1;
    test_vector[2] = -1;
    test_vector[3] = 2;
    test_vector[4] = -2;
    test_vector[5] = 3;
    test_vector[6] = -3;
    test_vector[7] = 10;
    test_vector[8] = -10;
    test_vector[9] = 30;
    test_vector[10] = -30;
    test_vector[11] = 1000000000;
    test_vector[12] = -1000000000;
    test_vector[13] = 2000000000;
    test_vector[14] = -2000000000;
    test_vector[15] = 2147483647;
    test_vector[16] = -2147483648;

    int i;
    int j;
    int x;
    int y;

    begin_test("test_signed_integer_operations");

    for(i = 0; i < 17; ++i) {
        x = test_vector[i];
        printf("!%d = %d\n", x, !x);
        printf("~%d = %d\n", x, ~x);

        for(j = 0; j < 17; ++j) {
            y = test_vector[j];

            printf("%d + %d = %d\n", x, y, x + y);
            printf("%d - %d = %d\n", x, y, x - y);
            printf("%d * %d = %d\n", x, y, x * y);

            // Can't divide by zero, or divide MIN_INT by -1
            if(y != 0 && !(x == -2147483648 && y == -1)) {
                printf("%d / %d = %d\n", x, y, x / y);
                printf("%d %% %d = %d\n", x, y, x % y);
            }

            printf("%d << (%d & 31) = %d\n", x, y, x << (y & 31));
            printf("%d >> (%d & 31) = %d\n", x, y, x >> (y & 31));

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

void test_unsigned_integer_operations() {
    unsigned int test_vector[10];

    test_vector[0] = 0;
    test_vector[1] = 1;
    test_vector[2] = 2;
    test_vector[3] = 3;
    test_vector[4] = 10;
    test_vector[5] = 30;
    test_vector[6] = 1000000000;
    test_vector[7] = 2000000000;
    test_vector[8] = 3000000000;
    test_vector[9] = 4294967295;

    int i;
    int j;
    unsigned int x;
    unsigned int y;

    begin_test("test_unsigned_integer_operations");

    for(i = 0; i < 10; ++i) {
        x = test_vector[i];
        printf("!%u = %d\n", x, !x);
        printf("~%u = %u\n", x, ~x);

        for(j = 0; j < 10; ++j) {
            y = test_vector[j];

            printf("%u + %u = %u\n", x, y, x + y);
            printf("%u - %u = %u\n", x, y, x - y);
            printf("%u * %u = %u\n", x, y, x * y);

            // Can't divide by zero
            if(y != 0) {
                printf("%u / %u = %u\n", x, y, x / y);
                printf("%u %% %u = %u\n", x, y, x % y);
            }

            printf("%u << (%u & 31) = %u\n", x, y, x << (y & 31));
            printf("%u >> (%u & 31) = %u\n", x, y, x >> (y & 31));

            printf("%u & %u = %u\n", x, y, x & y);
            printf("%u | %u = %u\n", x, y, x | y);
            printf("%u ^ %u = %u\n", x, y, x ^ y);

            printf("%u > %u = %d\n", x, y, x > y);
            printf("%u < %u = %d\n", x, y, x < y);
            printf("%u >= %u = %d\n", x, y, x >= y);
            printf("%u <= %u = %d\n", x, y, x <= y);
            printf("%u != %u = %d\n", x, y, x != y);
            printf("%u == %u = %d\n", x, y, x == y);
        }
    }
}

void test_signed_long_operations() {
    long test_vector[17];

    test_vector[0] = 0;
    test_vector[1] = 1;
    test_vector[2] = -1;
    test_vector[3] = 2;
    test_vector[4] = -2;
    test_vector[5] = 3;
    test_vector[6] = -3;
    test_vector[7] = 10;
    test_vector[8] = -10;
    test_vector[9] = 30;
    test_vector[10] = -30;
    test_vector[11] = 1000000000000;
    test_vector[12] = -1000000000000;
    test_vector[13] = 2000000000000;
    test_vector[14] = -2000000000000;
    test_vector[15] = 9223372036854775807;
    test_vector[16] = -9223372036854775807 - 1;

    int i;
    int j;
    long x;
    long y;

    begin_test("test_signed_long_operations");

    for(i = 0; i < 17; ++i) {
        x = test_vector[i];
        printf("!%ld = %d\n", x, !x);
        printf("~%ld = %ld\n", x, ~x);

        for(j = 0; j < 17; ++j) {
            y = test_vector[j];

            printf("%ld + %ld = %ld\n", x, y, x + y);
            printf("%ld - %ld = %ld\n", x, y, x - y);
            printf("%ld * %ld = %ld\n", x, y, x * y);

            // Can't divide by zero, or divide MIN_INT by -1
            if(y != 0 && !(x == (-9223372036854775807 - 1) && y == -1)) {
                printf("%ld / %ld = %ld\n", x, y, x / y);
                printf("%ld %% %ld = %ld\n", x, y, x % y);
            }

            printf("%ld << (%ld & 63) = %ld\n", x, y, x << (y & 63));
            printf("%ld >> (%ld & 63) = %ld\n", x, y, x >> (y & 63));

            printf("%ld & %ld = %ld\n", x, y, x & y);
            printf("%ld | %ld = %ld\n", x, y, x | y);
            printf("%ld ^ %ld = %ld\n", x, y, x ^ y);

            printf("%ld > %ld = %d\n", x, y, x > y);
            printf("%ld < %ld = %d\n", x, y, x < y);
            printf("%ld >= %ld = %d\n", x, y, x >= y);
            printf("%ld <= %ld = %d\n", x, y, x <= y);
            printf("%ld != %ld = %d\n", x, y, x != y);
            printf("%ld == %ld = %d\n", x, y, x == y);
        }
    }
}

void test_unsigned_long_operations() {
    unsigned long test_vector[10];

    test_vector[0] = 0;
    test_vector[1] = 1;
    test_vector[2] = 2;
    test_vector[3] = 3;
    test_vector[4] = 10;
    test_vector[5] = 30;
    test_vector[6] = 1000000000000;
    test_vector[7] = 2000000000000;
    test_vector[8] = 10000000000000000000UL;
    test_vector[9] = 18446744073709551615UL;

    int i;
    int j;
    unsigned long x;
    unsigned long y;

    begin_test("test_unsigned_long_operations");

    for(i = 0; i < 10; ++i) {
        x = test_vector[i];
        printf("!%lu = %d\n", x, !x);
        printf("~%lu = %lu\n", x, ~x);

        for(j = 0; j < 10; ++j) {
            y = test_vector[j];

            printf("%lu + %lu = %lu\n", x, y, x + y);
            printf("%lu - %lu = %lu\n", x, y, x - y);
            printf("%lu * %lu = %lu\n", x, y, x * y);

            // Can't divide by zero
            if(y != 0) {
                printf("%lu / %lu = %lu\n", x, y, x / y);
                printf("%lu %% %lu = %lu\n", x, y, x % y);
            }

            printf("%lu << (%lu & 63) = %lu\n", x, y, x << (y & 63));
            printf("%lu >> (%lu & 63) = %lu\n", x, y, x >> (y & 63));

            printf("%lu & %lu = %lu\n", x, y, x & y);
            printf("%lu | %lu = %lu\n", x, y, x | y);
            printf("%lu ^ %lu = %lu\n", x, y, x ^ y);

            printf("%lu > %lu = %d\n", x, y, x > y);
            printf("%lu < %lu = %d\n", x, y, x < y);
            printf("%lu >= %lu = %d\n", x, y, x >= y);
            printf("%lu <= %lu = %d\n", x, y, x <= y);
            printf("%lu != %lu = %d\n", x, y, x != y);
            printf("%lu == %lu = %d\n", x, y, x == y);
        }
    }
}

void test_variable_scope() {
    begin_test("test_variable_scope");

    int x = 42;

    printf("x = %d\n", x);

    {
        long x = 1234;
        printf("x = %ld\n", x);
    }

    printf("x = %d\n", x);
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
    test_evaluation_width();
    test_nested_calls();
    test_signed_integer_operations();
    test_unsigned_integer_operations();
    test_signed_long_operations();
    test_unsigned_long_operations();
    test_variable_scope();
    test_arrays();
    test_pointers();

    return 0;
}