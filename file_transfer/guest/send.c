#include <stdio.h>

#define OPEN	(*(volatile char *)0xe8e011)
#define CLOSE	(*(volatile char *)0xe8e013)
#define DATA	(*(volatile char *)0xe8e015)

int main(int argc, char *argv[]) {
	int c;
	FILE *fi;
	if (argc != 2) {
		fprintf(stderr, "Usage: send <filename>\n");
		return 1;
	}
	if (!(fi = fopen(argv[1], "rb"))) {
		fprintf(stderr, "cannot read: %s\n", argv[1]);
		return 1;
	}
	OPEN = 0;
	if (!OPEN) {
		fclose(fi);
		fprintf(stderr, "cannot connect to the host\n");
		return 1;
	}
	while ((c = getc(fi)) != EOF) DATA = c;
	CLOSE = 0;
	fclose(fi);
	return 0;
}
