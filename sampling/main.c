#include <fcntl.h>
#include <unistd.h>

void skip(char *c, int where)
{
	while (*c == ' ' || *c == '\t') read(where, c, 1);
}


int main(int arvc, char **argv)
{
	int f_src = open(argv[1], O_RDONLY);
	int f_new = open(argv[2], O_CREAT | O_WRONLY);
	char c;
	int flag = 1;

	while (read(f_src, &c, 1) > 0)
	{
		if (c == ' ' || c == '\t')
		{
			skip(&c, f_src);
			if (!flag && c != '\n')
				write(f_new, ",", 1);
		}
		flag = (c == '\n');
		write(f_new, &c, 1);
	}
	close(f_src);
	close(f_new);
	return (0);
}
