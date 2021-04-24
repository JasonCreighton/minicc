int main() {
	int x;

	x = 42;

	printf("Hello, world!\n");
	printf("A number: %d\n", x);

	if(x) {
		printf("x is true\n");
	}

	if(0) {
		printf("0 is true (should not happen)\n");
	} else {
		printf("0 is false\n");
	}
}