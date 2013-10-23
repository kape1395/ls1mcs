#define BUF_SIZE 300

void reverse(char *buf, int sz)
{
    /* TODO: Reverse list. */
}

int is_zero(char *buf, int sz)
{
    /* TODO: Check if number is zero.*/
    return 0;
}

void num_div(char *num, int num_sz, char *div, int *div_sz, int *rem)
{
    /* TODO: Divide num by 255. Put result to div and the remainder to rem. */
}

void encode(char *base256, int base256_sz, char *base255, int *base255_sz)
{
    char *num = base256;
    char *div;  /* TODO: Alloc */
    char *tmp;
    int num_sz = base256_sz;
    int div_sz;
    int rem;
    int tmp_sz;
    int sz;

    for (sz = 0; !is_zero(num, num_sz); sz++)
    {
        num_div(num, num_sz, div, &div_sz, &rem);
        base255[sz] = rem;
        /* Swap num<->div */
        tmp    = div;    div    = num;    num    = tmp;
        tmp_sz = div_sz; div_sz = num_sz; num_sz = tmp_sz;
    }
}

int main()
{
    char base255[BUF_SIZE];
    char base256[BUF_SIZE];
    int base255_sz;
    int base256_sz;

    base256[0] = 121;
    base256[1] = 122;
    base256[2] = 142;
    base256[3] = 212;
    base256_sz = 4;

    encode(base256, base256_sz, base255, &base255_sz);

    return 0;
}

