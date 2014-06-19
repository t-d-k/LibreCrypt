
// Copyright in this code is held by Dr B. R. Gladman but free direct or
// derivative use is permitted subject to acknowledgement of its origin.
// Dr B. R. Gladman                               .   25th January 2000.

#ifdef  __cplusplus
    extern  "C"
    {
#endif

IFREF   open_ifile(IFREF inf, const char *name);
OFREF   open_ofile(OFREF outf, const char *name);
void    close_ifile(IFREF inf);
void    close_ofile(OFREF outf);
char    *copy_str(char *s, const char *fstr);
void    put_char(OFREF outf, const char c);
void    put_string(OFREF outf, const char s[]);
void    con_char(const char c);
void    con_string(const char s[]);
bool    get_line(IFREF inf, char s[]);
void    block_out(const u1byte ty, const u1byte b[], OFREF outf, const u4byte len);

int     find_string(const char *s1, const char s2[]);
int     find_line(IFREF inf, char str[]);

u4byte  rand32(void);
u1byte  rand8(void);
int     block_in(u1byte l[], const char *p);
void    block_clear(u1byte l[], const u4byte len);
void    block_reverse(u1byte l[], const u4byte len);
void    block_copy(u1byte l[], const u1byte r[], const u4byte len);
void    block_xor(u1byte l[], const u1byte r[], const u4byte len);
bool    block_cmp(const u1byte l[], const u1byte r[], const u4byte len);
void    block_rndfill(u1byte l[], u4byte len);

void    put_dec(char *s, int val);
int     get_dec(const char *s);
int     cmp_nocase(const char *s1, const char *s2);
bool    test_args(int argc, char *argv[], char des_chr, char tst_chr);

#ifdef  __cplusplus
    }
#endif
