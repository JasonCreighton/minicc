int main() {
	int x = 42;
	int y = 0;
	int z;

	z = 7;

	printf("Hello, world!\n");
	printf("Numbers: %d, %d, %d\n", x, y, z);

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