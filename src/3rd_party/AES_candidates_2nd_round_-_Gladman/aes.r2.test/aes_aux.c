
// Copyright in this code is held by Dr B. R. Gladman but free direct or
// derivative use is permitted subject to acknowledgement of its origin.
// Dr B. R. Gladman                               .   25th January 2000.

#ifdef  __cplusplus
#   include <iostream>
#   include <fstream>
#   include <cctype>
    extern  "C"
    {
#else
#   include <stdio.h>
#   include <ctype.h>
#endif

#include "aes_defs.h"
#include "aes_aux.h"

#ifndef __cplusplus
    enum dir_flag mode = both;      // declare dir_flag
#endif

// Strings to locate lines in test vector files

#define no_lhdr 6
char    *fstr[no_lhdr] = { "KEYSIZE=", "I=", "IV=", "KEY=", "PT=", "CT=" };

char    *hxx = "0123456789abcdef";

// Hexadecimal conversion from hex character to a number in range 0 <= no <= 15

STATIC int to_hex(int ch)
{
    return (ch & 15) + (ch >= '0' && ch <= '9' ? 0 : 9);
}

void put_char(OFREF outf, const char c)
{
#ifdef  __cplusplus
    outf << c;
#else
    fprintf(outf, "%c", c);
#endif
}

void put_string(OFREF outf, const char s[])
{
#ifdef  __cplusplus
    outf << s;
#else
    fprintf(outf, "%s", s);
#endif
}

void con_char(const char c)
{
#ifdef  __cplusplus
    std::cout << c;
#else
    printf("%c", c);
#endif
}

void con_string(const char s[])
{
#ifdef  __cplusplus
    std::cout << s;
#else
    printf("%s", s);
#endif
}

bool get_line(IFREF inf, char s[])
{
#ifdef  __cplusplus
    if(inf.eof())

        return false;
    else
    {
        inf.getline(s, 100); return true;
    }
#else
    return (fscanf(inf, "%s", s) == EOF ? false : true);
#endif
}

IFREF open_ifile(IFREF inf, const char *name)
{
#ifdef  __cplusplus
    inf.open(name, std::ios_base::in);
#else
    inf = fopen(name, "r");
#endif
    return inf;
}

OFREF open_ofile(OFREF outf, const char *name)
{
#ifdef  __cplusplus
    outf.open(name, std::ios_base::out);
#else
    outf = fopen(name, "w");
#endif
    return outf;
}

void close_ifile(IFREF inf)
{
#ifdef  __cplusplus
    inf.close();
#else
    fclose(inf);
#endif
}

void close_ofile(OFREF outf)
{
#ifdef  __cplusplus
    outf.close();
#else
    fclose(outf);
#endif
}

char *copy_str(char *s, const char *fstr)
{   const char *p = fstr;

    while(*p)

        *s++ = *p++;

    *s = '\0'; return s;
}

// Read from a string of hexadecimal digits into an array of 8-bit bytes
// Each pair of hexadecimal digits is used to compile one 8-bit byte and
// the first (last) digit forms the most (least) significant four bits
// of each byte. The hexadecimal digits are read from left (low) to right
// (high) and are placed in increasing index positions in the byte array

int block_in(u1byte l[], const char *p)
{   int i = 0;

    while(*p)
    {
        if(isxdigit(*p) && isxdigit(*(p + 1)))
        {
            l[i++] = (to_hex(*p) << 4) + to_hex(*(p + 1)); p += 2;
        }
        else

            break;
    }

    return i;
}

// clears an array of 8-bit bytes

void block_clear(u1byte l[], const u4byte len)
{   u1byte  *lp = l;

    while(lp < l + len)

        *lp++ = 0;
}

// reverses the order of an array of 8-bit bytes

void block_reverse(u1byte l[], const u4byte len)
{
    u1byte  *blo = l, *bhi = l + len - 1, by;

    while(blo < bhi)

        by = *blo, *blo++ = *bhi, *bhi-- = by;
}

// copies an array of 8-bit bytes

void block_copy(u1byte l[], const u1byte r[], const u4byte len)
{   u1byte          *lp = l;
    const u1byte    *rp = r;

    while(lp < l + len)

        *lp++ = *rp++;
}

// xors one array of 8-bit bytes with another

void block_xor(u1byte l[], const u1byte r[], const u4byte len)
{   u1byte          *lp = l;
    const u1byte    *rp = r;

    while(lp < l + len)

        *lp++ ^= *rp++;
}

// compares two arrays of 8-bit bytes and return true if the same or
// false otherwise

bool block_cmp(const u1byte l[], const u1byte r[], const u4byte len)
{   const u1byte    *lp = l, *rp = r;

    while(lp < l + len)

        if(*lp++ != *rp++)

            return false;

    return true;
}

u4byte rand32(void)
{   static u4byte   r4,r_cnt = -1,w = 521288629,z = 362436069;

    z = 36969 * (z & 65535) + (z >> 16);
    w = 18000 * (w & 65535) + (w >> 16);

    r_cnt = 0; r4 = (z << 16) + w; return r4;
}

u1byte rand8(void)
{   static u4byte   r4,r_cnt = 4;

    if(r_cnt == 4)
    {
        r4 = bswap(rand32()); r_cnt = 0;
    }

    return (char)(r4 >> (8 * r_cnt++));
}

// fill a block with random charactrers

void block_rndfill(u1byte l[], u4byte len)
{   u4byte  i;

    for(i = 0; i < len; ++i)

        l[i] = rand8();
}

// Convert decimal number to a character string

void put_dec(char *s, int val)
{   char    *p = s, *q = s, ch;

    do
    {
        *p++ = '0' + (val % 10); val /= 10;
    }
    while
        (val);

    *p-- = '\0';

    while(p > q)
    {
        ch = *p, *p-- = *q, *q++ = ch;
    }
}

// Extract decimal number from a character string

int get_dec(const char *s)
{   const char  *p = s;
    int         nbr = 0;

    while(*p && *p >= '0' && *p <= '9')
    {
        nbr = 10 * nbr + (*p - '0'); ++p;
    }

    return nbr;
}

// Compare two strings ignoring case

int cmp_nocase(const char *s1, const char *s2)
{   const char  *p1 = s1, *p2 = s2;

    while(*p1 && *p2)
    {
        if(toupper(*p1) != toupper(*p2))
        {
            return (toupper(*p1) < toupper(*p2) ? -1 : 1);
        }

        ++p1; ++p2;
    }

    while(*p1) p1++; while(*p2) p1++;

    return (p2 - s2) - (p1 - s1);
}

// Input command line arguments

bool test_args(int argc, char *argv[], char des_chr, char tst_chr)
{   int i, j;

    for(i = 1; i < argc; ++i)
    {
        if(argv[i][0] != '-' && argv[i][0] != '/')

            continue;

        if(argv[i][1] != tolower(des_chr) && argv[i][1] != toupper(des_chr))

            continue;

        else if(!tst_chr)

            return true;

        if(argv[i][2] != ':')

            continue;

        for(j = 3; argv[i][j]; ++j)

            if(argv[i][j] == tolower(tst_chr) || argv[i][j] == toupper(tst_chr))

                return true;
    }

    return false;
}

// Find a given string s2 in s1

int find_string(const char *s1, const char s2[])
{   const char  *p1 = s1, *q1, *q2;

    while(*p1)
    {
        q1 = p1; q2 = s2;

        while(*q1 && *q2 && *q1 == *q2)
        {
            q1++; q2++;
        }

        if(!*q2)

            return p1 - s1;

        p1++;
    }

    return -1;
}

// Find a line with a given header and return line type
// or -1 if end of file

int find_line(IFREF inf, char str[])
{   int ty1;

    while(get_line(inf, str))               // input a line
    {
        for(ty1 = 0; ty1 < no_lhdr; ++ty1)  // compare line header
        {
            if(find_string(str, fstr[ty1]) >= 0)
            {
                return ty1;     // exit if known line header found
            }
        }
    }

    return -1;  // end of file
}

// Output an array of bytes to a string of hexadecimal digits using
// the conventions described for input above.  But if type (ty) = 1
// it outputs a test number in decimal notation

void block_out(const u1byte ty, const u1byte b[], OFREF outf, const u4byte len)
{   char            ostr[16];
    unsigned int    i;

    if(ty <= 1)

        put_char(outf, '\n');

    put_char(outf, '\n'); put_string(outf, fstr[ty]);

    if(ty <= 1)
    {
        put_dec(ostr, len); put_string(outf, ostr);
    }
    else

        for(i = 0; i < len; ++i)
        {
            put_char(outf, hxx[(b[i] >> 4) & 15]);
            put_char(outf, hxx[b[i] & 15]);
        }
}

#ifdef  __cplusplus
    }
#endif
