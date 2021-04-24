int main() {
	int x;
	int y;
	int z;

	x = 42;
	y = 0;
	z = 7;

	printf("Hello, world!\n");
	printf("A number: %d\n", x);

	if(x) {
		printf("x is true\n");
	}

	if(y) {
		printf("y is true (should not happen)\n");
	} else {
		printf("y is false\n");
	}
}