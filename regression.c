// Regression test for minicc compiler
//
// Should be compiled with -fwrapv or similar, because minicc wraps on
// overflow.

extern int printf(const char *fmt, ...);
extern int putchar(int ch);
extern void *malloc(unsigned long size);
extern void free(void *ptr);
extern double sin(double x);
extern double cos(double x);
extern double pow(double x, double y);

int g_num_tests;

void begin_test(const char *test_name) {
    printf("=== %s ===\n", test_name);
    ++g_num_tests;
}

void print_double(double x) {
    printf("print_double(%f)\n", x);
}

void print_float(float x) {
    printf("print_float(%f)\n", x);
}

double long_to_double(long x) {
    return x;
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

void test_double_operations() {
    double test_vector[11];

    test_vector[0] = 0.0;
    test_vector[1] = 1.0;
    test_vector[2] = -1.0;
    test_vector[3] = 2.718281828459045;
    test_vector[4] = 3.141592653589793;
    test_vector[5] = -5.4321;
    test_vector[6] = 1.0e9;
    test_vector[7] = 1.0e12;
    test_vector[8] = -1.0e15;
    test_vector[9] = 1.0e300;
    test_vector[10] = -1.0e300;

    int i;
    int j;
    double x;
    double y;

    begin_test("test_double_operations");

    for(i = 0; i < 11; ++i) {
        x = test_vector[i];
        printf("!%f = %d\n", x, !x);
        printf("-%f = %f\n", x, -x);

        for(j = 0; j < 11; ++j) {
            y = test_vector[j];

            printf("%f + %f = %f\n", x, y, x + y);
            printf("%f - %f = %f\n", x, y, x - y);
            printf("%f * %f = %f\n", x, y, x * y);
            printf("%f / %f = %f\n", x, y, x / y);

            printf("%f > %f = %d\n", x, y, x > y);
            printf("%f < %f = %d\n", x, y, x < y);
            printf("%f >= %f = %d\n", x, y, x >= y);
            printf("%f <= %f = %d\n", x, y, x <= y);
            printf("%f != %f = %d\n", x, y, x != y);
            printf("%f == %f = %d\n", x, y, x == y);
        }
    }
}

void test_float_operations() {
    float test_vector[11];

    test_vector[0] = 0.0f;
    test_vector[1] = 1.0f;
    test_vector[2] = -1.0f;
    test_vector[3] = 2.718281828459045f;
    test_vector[4] = 3.141592653589793f;
    test_vector[5] = -5.4321f;
    test_vector[6] = 1.0e9f;
    test_vector[7] = 1.0e12f;
    test_vector[8] = -1.0e15f;
    test_vector[9] = 1.0e35f;
    test_vector[10] = -1.0e35f;

    int i;
    int j;
    float x;
    float y;

    begin_test("test_float_operations");

    for(i = 0; i < 11; ++i) {
        x = test_vector[i];
        printf("!%f = %d\n", x, !x);
        printf("-%f = %f\n", x, -x);

        for(j = 0; j < 11; ++j) {
            y = test_vector[j];

            printf("%f + %f = %f\n", x, y, x + y);
            printf("%f - %f = %f\n", x, y, x - y);
            printf("%f * %f = %f\n", x, y, x * y);
            printf("%f / %f = %f\n", x, y, x / y);

            printf("%f > %f = %d\n", x, y, x > y);
            printf("%f < %f = %d\n", x, y, x < y);
            printf("%f >= %f = %d\n", x, y, x >= y);
            printf("%f <= %f = %d\n", x, y, x <= y);
            printf("%f != %f = %d\n", x, y, x != y);
            printf("%f == %f = %d\n", x, y, x == y);
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

void test_implicit_function_argument_conversion() {
    begin_test("test_implicit_function_argument_conversion");
    
    print_double(1);
    print_double(42);
    print_double(9223372036854775807);
    print_float(9223372036854775807);

    // TODO: Currently unsigned longs with the MSB set are interpreted
    // incorrectly as signed when converting to floating point.
    // print_float(18446744073709551615UL);
}

void test_implicit_function_return_conversion() {
    begin_test("test_implicit_function_return_conversion");

    print_double(long_to_double(42));
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

void test_trig_functions() {
    begin_test("test_trig_functions");

    int i;
    for(i = 0; i < 32; ++i) {
        double angle = (i / 32.0) * 2.0 * 3.14159265358979312;
        printf("x = %f, sin(x) = %f, cos(x) = %f\n", angle, sin(angle), cos(angle));
    }
}

unsigned long fib(unsigned long n) {
    if(n < 2) {
        return n;
    } else {
        return fib(n - 2) + fib(n - 1);
    }
}

void test_fibonacci() {
    begin_test("test_fibonacci");

    int i;
    for(i = 0; i < 20; ++i) {
        printf("fib(%d) = %lu\n", i, fib(i));
    }
}

int in_mandelbrot_set(double c_re, double c_im) {
    const int max_iterations = 250;
    double z_re = 0.0;
    double z_im = 0.0;

    int i;
    for(i = 0; i < max_iterations; ++i) {
        double next_z_re = (z_re * z_re) - (z_im * z_im) + c_re;
        double next_z_im = (2 * z_re * z_im) + c_im;

        z_re = next_z_re;
        z_im = next_z_im;

        double magSquared = (z_re * z_re) + (z_im * z_im);

        if(magSquared > 4.0) {
            return 0;
        }
    }

    return 1;
}

void test_mandelbrot() {
    begin_test("test_mandelbrot");

    const int width = 120;
    const int height = 60;
    const double start_re = -2.0;
    const double end_re = 0.5;
    const double start_im = -1.25;
    const double end_im = 1.25;
    const double scale_re = (end_re - start_re) / width;
    const double scale_im = (end_im - start_im) / height;

    int x;
    int y;
    for(y = 0; y < height; ++y) {
        for(x = 0; x < width; ++x) {
            if(in_mandelbrot_set(start_re + (x * scale_re), start_im + (y * scale_im))) {
                putchar('*');
            } else {
                putchar(' ');
            }
        }
        putchar('\n');
    }
}

void test_jumps() {
    begin_test("test_jumps");

    int i;
    int j;

    if(0) some_label: printf("Should not be reached because a label is attached to another statement, not an individual statement of its own.\n");
    if(0) goto some_label; // Avoid unused label warning from gcc

    i = 0;
loop:
    printf("i=%d\n", i++);

    if(i < 10) goto loop;

    for(i = 0; i < 50; ++i) {
        if((i % 2) == 0) continue;
        for(j = 0; j < 50; ++j) {
            if((j % 3) == 0) continue;

            printf("i=%d, j=%d\n", i, j);

            if(j > 25) break;
        }
        if(i > 20) break;
    }

    goto done;

    printf("Dead code...\n");

done:
    return;
}

void print_primes_up_to(unsigned long limit) {
    unsigned long ary_size = limit + 1;
    unsigned long i;
    unsigned long j;

    char *is_prime = malloc(ary_size);

    for(i = 0; i < ary_size; ++i) {
        is_prime[i] = 1;
    }

    is_prime[0] = 0;
    is_prime[1] = 0;

    for(i = 2; (i * i) < ary_size; ++i) {
        if(is_prime[i]) {
            for(j = i * i; j < ary_size; j = j + i) {
                is_prime[j] = 0;
            }
        }
    }

    for(i = 0; i < ary_size; ++i) {
        if(is_prime[i]) printf("%lu ", i);
    }
    printf("\n");

    free(is_prime);
}

void test_print_primes() {
    begin_test("test_print_primes");

    print_primes_up_to(1000);
}

void test_array_of_pointers() {
    begin_test("test_array_of_pointers");
    int arrayOfInt[3];
    int *arrayOfPtrToInt[3];
    int *(arrayOfPtrToInt2[3]);
    int (*ptrToArrayOfInt)[3];

    arrayOfInt[0] = 100;
    arrayOfInt[1] = 200;
    arrayOfInt[2] = 300;

    arrayOfPtrToInt[0] = &arrayOfInt[0];
    arrayOfPtrToInt[1] = &arrayOfInt[1];
    arrayOfPtrToInt[2] = &arrayOfInt[2];

    arrayOfPtrToInt2[0] = &arrayOfInt[2];
    arrayOfPtrToInt2[1] = &arrayOfInt[1];
    arrayOfPtrToInt2[2] = &arrayOfInt[0];

    ptrToArrayOfInt = &arrayOfInt;

    int i;
    for(i = 0; i < 3; ++i) {
        printf("*arrayOfPtrToInt[%d] = %d\n", i, *arrayOfPtrToInt[i]);
        printf("*arrayOfPtrToInt2[%d] = %d\n", i, *arrayOfPtrToInt2[i]);
        printf("(*ptrToArrayOfInt)[%d] = %d\n", i, (*ptrToArrayOfInt)[i]);
    }
}

// See https://adventofcode.com/2015/day/22
int dfs_min_mana_so_far;
int dfs_calls;
void dfs_player_won(int p_mana_spent) {
    if(p_mana_spent < dfs_min_mana_so_far) {
        printf("Found new best solution: %d\n", p_mana_spent);
        dfs_min_mana_so_far = p_mana_spent;
    }
}

void dfs(int players_turn, int p_hp, int p_mana_spent, int p_mana_earned, int b_hp, int t_shield, int t_poison, int t_recharge) {
    ++dfs_calls;
    if(p_mana_spent > p_mana_earned) return; // Should not have been allowed to cast this anyway
    if(p_mana_spent > dfs_min_mana_so_far) return; // Prune
    if(b_hp < 0) {
        dfs_player_won(p_mana_spent);
        return;
    }
    if(p_hp < 0) return; // Player lost

    // Apply effects
    int armor = t_shield > 0 ? 7 : 0;
    if(t_poison > 0) b_hp = b_hp - 3;
    if(t_recharge > 0) p_mana_earned = p_mana_earned + 101;

    // Decrement effect counters
    if(t_shield > 0) --t_shield;
    if(t_poison > 0) --t_poison;
    if(t_recharge > 0) --t_recharge;

    if(b_hp < 0) {
        dfs_player_won(p_mana_spent);
        return;
    }

    if(players_turn) {
        // Magic Missile
        dfs(!players_turn, p_hp, p_mana_spent + 53, p_mana_earned, b_hp - 4, t_shield, t_poison, t_recharge);

        // Drain
        dfs(!players_turn, p_hp + 2, p_mana_spent + 73, p_mana_earned, b_hp - 2, t_shield, t_poison, t_recharge);

        // Shield
        if(t_shield == 0)
            dfs(!players_turn, p_hp, p_mana_spent + 113, p_mana_earned, b_hp, 6, t_poison, t_recharge);

        // Poison
        if(t_poison == 0)
            dfs(!players_turn, p_hp, p_mana_spent + 173, p_mana_earned, b_hp, t_shield, 6, t_recharge);

        // Recharge
        if(t_recharge == 0)
            dfs(!players_turn, p_hp, p_mana_spent + 229, p_mana_earned, b_hp, t_shield, t_poison, 5);
    } else {
        // Boss's turn
        int boss_damage = 8 - armor;
        if(boss_damage < 1) boss_damage = 1;
        dfs(!players_turn, p_hp - boss_damage, p_mana_spent, p_mana_earned, b_hp, t_shield, t_poison, t_recharge);
    }
}

void test_depth_first_search() {
    begin_test("test_depth_first_search");

    dfs_calls = 0;
    dfs_min_mana_so_far = 999999;

    dfs(1, 50, 0, 500, 55, 0, 0, 0);

    printf("Calls to dfs(): %d\n", dfs_calls);
    printf("Minimum mana to win: %d\n", dfs_min_mana_so_far);
}

int main() {
    g_num_tests = 0;

    test_comments();
    test_evaluation_width();
    test_nested_calls();
    test_signed_integer_operations();
    test_unsigned_integer_operations();
    test_signed_long_operations();
    test_unsigned_long_operations();
    test_double_operations();
    test_float_operations();
    test_variable_scope();
    test_implicit_function_argument_conversion();
    test_implicit_function_return_conversion();
    test_arrays();
    test_pointers();
    test_trig_functions();
    test_fibonacci();
    test_mandelbrot();
    test_jumps();
    test_print_primes();
    test_array_of_pointers();
    test_depth_first_search();

    printf("=== Ran %d tests ===\n", g_num_tests);

    return 0;
}