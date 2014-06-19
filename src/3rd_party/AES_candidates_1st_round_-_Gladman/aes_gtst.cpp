
//  Generate and Compare AES Test Files For KAT and Monte Carlo Tests

//  Directory for AES test vector files and locally generated copies

char    *aes_path = "i:\\testvals\\";
char    *dir_path = "d:\\cpp\\aes\\";

//  Frog, Safer and Serpent hack to get Monte Carlo test agreement
//  because the test vectors for these algorithms assemble the key
//  vectors in the reverse direction in memory

bool    do_fss_hack = true;

#include "../std_defs.h"

#include <iostream>
#include <iomanip>
#include <fstream>
#include <cctype>
#include <string>

using std::ios;
using std::ostream;
using std::ifstream;
using std::ofstream;
using std::cout;
using std::endl;
using std::setw;
using std::ios_base;
using std::string;
using std::getline;

string  fstr[6] = { "KEYSIZE=", "I=", "IV=", "KEY=", "PT=", "CT=" };

char    *hxx = "0123456789abcdef";

void v_out(ostream &outf, u1byte ty, u4byte len, void *v = 0)
{
    if(ty == 1)

        outf << endl << endl << fstr[ty] << len;
    
    else
    {   u1byte *p = reinterpret_cast<u1byte*>(v), buf[80], *bp = buf;
        
        for(int i = 0; i < len; ++i)
        {
            *bp++ = hxx[(*(p + i) >> 4) & 15];
            *bp++ = hxx[*(p + i) & 15];
        }

        *bp = '\0'; outf << endl << fstr[ty] << buf;
    }
};

bool    on_screen = false;

inline void clear_count(void)
{
    if(on_screen)

        cout << "\b\b\b\b    \b\b\b\b";
    
    on_screen = false;
};

void put_count(int x)
{
    clear_count();
    cout << (x /1000) % 10 << (x /100) % 10 << (x /10) % 10 <<  x % 10; 
    on_screen = true;
};

void header(ostream &outf, int type)
{   string  str0("=======================================================");
    string  str1("Author:    Dr B R Gladman                              ");
    string  str2("Test:      ");
    string  str3(string("Algorithm: ") + cipher_name()[0] + " (" + cipher_name()[1] + ")");
    string  str4("Filename:  ");
    
    str2 += (type < 4 ? "ECB " : "CBC ");
    str4 += (type < 4 ? "ecb_" : "cbc_");

    switch(type)
    {
        case  0:    str2 += "Variable Key Known Answer Tests"; 
                    str4 += "vk.txt"; break;
        case  1:    str2 += "Variable Text Known Answer Tests"; 
                    str4 += "vt.txt"; break;
        case  2:
        case  4:    str2 += "Monte Carlo (Encryption) Tests"; 
                    str4 += "me.txt"; break;
        case  3:    
        case  5:    str2 += "Monte Carlo (Decryption) Tests"; 
                    str4 += "md.txt"; break;
    }

    outf << str0 << endl << str1 << endl << str2 << endl << str3
         << endl << str4 << endl << str0;
}

void ecb_vt(ostream &outf)
{   u4byte      i, j, pt[4], ct[4], key[8];

    key[0] = key[1] = key[2] = key[3] = 0;
    key[4] = key[5] = key[6] = key[7] = 0;

    for(i = 0; i < 3; ++i)
    {
        set_key(key, 128 + 64 * i);

        outf << endl << endl << fstr[0] << setw(3) << 128 + 64 * i << endl;
        v_out(outf, 3, 16 + 8 * i, key);

        for(j = 0; j <= 128; ++j)
        {
            v_out(outf, 1, j); 
            
            pt[0] = pt[1] = pt[2] = pt[3] = 0;
            
            if(j)

                *((u1byte*)pt + (j - 1) / 8) = 0x80 >> (j - 1) % 8;

            encrypt(pt , ct);

            v_out(outf, 4, 16, pt); 
            v_out(outf, 5, 16, ct);
        }
    }
};

void ecb_vk(ostream &outf)
{   u4byte      i, j, pt[4], ct[4], key[8];

    pt[0] = pt[1] = pt[2] = pt[3] = 0;

    for(i = 0; i < 3; ++i)
    {
        outf << endl << endl << fstr[0] << setw(3) << 128 + 64 * i << endl;
        v_out(outf, 4, 16, pt); 

        for(j = 0; j <= 128 + 64 * i; ++j)
        {
            v_out(outf, 1, j);
            
            key[0] = key[1] = key[2] = key[3] = 0;
            key[4] = key[5] = key[6] = key[7] = 0;
            
            if(j)

                *((u1byte*)key + (j - 1) / 8) = 0x80 >> (j - 1) % 8;

            set_key(key, 128 + 64 * i);

            v_out(outf, 3, 16 + 8 * i, key);

            encrypt(pt , ct);

            v_out(outf, 5, 16, ct); 
        }
    }
};

void ecb_me(ostream &outf)
{   u4byte      i, j, k, xo, pt[4], key[8], ct[12];

    for(i = 0; i < 3; ++i)
    {
        outf << endl << endl << fstr[0] << setw(3) << 128 + 64 * i;

        pt[0] = pt[1] = pt[2] = pt[3] = 0;

        key[0] = key[1] = key[2] = key[3] = 0;
        key[4] = key[5] = key[6] = key[7] = 0;

        ct[4] = pt[0]; ct[5] = pt[1]; ct[6] = pt[2]; ct[7] = pt[3];

        for(j = 0; j < 400; j++)
        {
            if(j % 40 == 0)

                put_count(j);

            set_key(key, 128 + 64 * i);

            v_out(outf, 1, j);
            v_out(outf, 3, 16 + 8 * i, key);
            v_out(outf, 4, 16, pt); 

            for(k = 0; k < 5000; ++k)
            {
                encrypt(ct + 4, ct); encrypt(ct, ct + 4);
            }

            v_out(outf, 5, 16, ct + 4);
        
            pt[0] = ct[4]; pt[1] = ct[5]; pt[2] = ct[6]; pt[3] = ct[7];

            xo = 4 - 2 * i;

            if(do_fss_hack && (**cipher_name() == 'f' || **cipher_name() == 's'))
            {
                ct[8] = ct[0]; ct[9] = ct[1]; ct[10] = ct[2]; ct[11] = ct[3]; xo = 4;
            }

            for(k = 0; k < 4 + 2 * i; ++k)

                key[k] ^= ct[k + xo];
        }
    }

    clear_count();
};

void ecb_md(ostream &outf)
{   u4byte      i, j, k, xo, pt[4], key[8], ct[12];

    for(i = 0; i < 3; ++i)
    {
        outf << endl << endl << fstr[0] << setw(3) << 128 + 64 * i;

        pt[0] = pt[1] = pt[2] = pt[3] = 0;

        key[0] = key[1] = key[2] = key[3] = 0;
        key[4] = key[5] = key[6] = key[7] = 0;

        ct[4] = pt[0]; ct[5] = pt[1]; ct[6] = pt[2]; ct[7] = pt[3];

        for(j = 0; j < 400; j++)
        {
            if(j % 40 == 0)

                put_count(j);

            set_key(key, 128 + 64 * i);

            v_out(outf, 1, j);
            v_out(outf, 3, 16 + 8 * i, key);
            v_out(outf, 5, 16, pt); 

            for(k = 0; k < 5000; ++k)
            {
                decrypt(ct + 4, ct); decrypt(ct, ct + 4);
            }

            v_out(outf, 4, 16, ct + 4);
        
            pt[0] = ct[4]; pt[1] = ct[5]; pt[2] = ct[6]; pt[3] = ct[7];

            xo = 4 - 2 * i;

            if(do_fss_hack && (**cipher_name() == 'f' || **cipher_name() == 's'))
            {
                ct[8] = ct[0]; ct[9] = ct[1]; ct[10] = ct[2]; ct[11] = ct[3]; xo = 4;
            }

            for(k = 0; k < 4 + 2 * i; ++k)

                key[k] ^= ct[k + xo];
        }
    }

    clear_count();
};

void cbc_me(ostream &outf)
{   u4byte      i, j, k, xo, key[8], ct[12];

    for(i = 0; i < 3; ++i)
    {
        outf << endl << endl << fstr[0] << setw(3) << 128 + 64 * i;

        key[0] = key[1] = key[2] = key[3] = 0;
        key[4] = key[5] = key[6] = key[7] = 0;  // KEY[0]

        ct[4] = ct[5] = ct[6] = ct[7] = 0;      //  IV[0]

        ct[0] = ct[1] = ct[2] = ct[3] = 0;      //  PT[0]

        for(j = 0; j < 400; j++)
        {
            if(j % 40 == 0)

                put_count(j);

            set_key(key, 128 + 64 * i);

            v_out(outf, 1, j);
            v_out(outf, 3, 16 + 8 * i, key);
            v_out(outf, 2, 16, ct + 4); 
            v_out(outf, 4, 16, ct); 

            for(k = 0; k < 5000; ++k)
            {
                ct[0] ^= ct[4]; ct[1] ^= ct[5]; ct[2] ^= ct[6]; ct[3] ^= ct[7]; 

                encrypt(ct, ct);

                ct[4] ^= ct[0]; ct[5] ^= ct[1]; ct[6] ^= ct[2]; ct[7] ^= ct[3]; 

                encrypt(ct + 4, ct + 4);
            }
            
            v_out(outf, 5, 16, ct + 4);
        
            xo = 4 - 2 * i;

            if(do_fss_hack && (**cipher_name() == 'f' || **cipher_name() == 's'))
            {
                ct[8] = ct[0]; ct[9] = ct[1]; ct[10] = ct[2]; ct[11] = ct[3]; xo = 4;
            }

            for(k = 0; k < 4 + 2 * i; ++k)

                key[k] ^= ct[k + xo];
        }
    }

    clear_count();
};

void cbc_md(ostream &outf)
{   u4byte      i, j, k, xo, pt[4], key[8], ct[12];

    for(i = 0; i < 3; ++i)
    {
        outf << endl << endl << fstr[0] << setw(3) << 128 + 64 * i;

        key[0] = key[1] = key[2] = key[3] = 0;
        key[4] = key[5] = key[6] = key[7] = 0;  // KEY[0]

        ct[0] = ct[1] = ct[2] = ct[3] = 0;      //  IV[0]

        ct[4] = ct[5] = ct[6] = ct[7] = 0;      //  CT[0]

        for(j = 0; j < 400; j++)
        {
            if(j % 40 == 0)

                put_count(j);

            set_key(key, 128 + 64 * i);

            v_out(outf, 1, j);
            v_out(outf, 3, 16 + 8 * i, key);
            v_out(outf, 2, 16, ct); 
            v_out(outf, 5, 16, ct + 4); 

            for(k = 0; k < 5000; ++k)
            {
                decrypt(ct + 4, pt);

                ct[0] ^= pt[0]; ct[1] ^= pt[1]; ct[2] ^= pt[2]; ct[3] ^= pt[3];

                decrypt(ct, pt);

                ct[4] ^= pt[0]; ct[5] ^= pt[1]; ct[6] ^= pt[2]; ct[7] ^= pt[3];
            }
            
            v_out(outf, 4, 16, ct + 4);

            xo = 4 - 2 * i;

            if(do_fss_hack && (**cipher_name() == 'f' || **cipher_name() == 's'))
            {
                ct[8] = ct[0]; ct[9] = ct[1]; ct[10] = ct[2]; ct[11] = ct[3]; xo = 4;
            }

            for(k = 0; k < 4 + 2 * i; ++k)

                key[k] ^= ct[k + xo];
        }
    }

    clear_count();
};

int cmp_nocase(const string &s1, const string &s2)
{   string::const_iterator  p1 = s1.begin();
    string::const_iterator  p2 = s2.begin();

    while(p1 != s1.end() && p2 != s2.end())
    {
        if(toupper(*p1) != toupper(*p2))
        {
            return (toupper(*p1) < toupper(*p2) ? -1 : 1);
        }

        ++p1; ++p2;
    }

    return s2.size() - s1.size();
}

int get_no(string s)
{   string::const_iterator  p = s.begin();
    int                     nbr = 0;

    while(p != s.end() && *p >= '0' && *p <= '9')
    {
        nbr = 10 * nbr + (*p - '0'); ++p;
    }

    return nbr;
};

int find_line(ifstream &inf, string &str)
{   string::size_type   pos;
    int                 ty1;

    while(!inf.eof())
    {
        getline(inf, str);

        for(ty1 = 0; ty1 < 5; ++ty1)
        {
            if((pos = str.find(fstr[ty1])) != string::npos && !pos)
            {
                return ty1;
            }
        }
    }

    return -1;
};

int sync(int nbr, ifstream &inf, string &str, bool outp)
{   int     ty, nn;

    for(;;)
    {   
        ty = find_line(inf, str);
        
        if(ty < 0)

            return ty;

        if(ty == 1)
        {
            nn = get_no(str.begin() + 2); 
                
            if(nn >= nbr)
                
                return nn;  
        }
            
        if(outp)
        
            cout << endl << "  " << str; 
    }
};

void comp_vecs(string &fn1, string &fn2)
{   ifstream            if1, if2;
    string              str1, str2;
    int                 ty1, ty2, no1, no2, err_cnt, np_cnt;
    bool                req;

    err_cnt = np_cnt = 0; req = true;

    if1.open(fn1.c_str(), ios::in);

    if(!if1.is_open())
    {
        cout << endl << "*** 1st file (" << fn1 << ") not found ***"; return;
    }

    if2.open(fn2.c_str(), ios::in);

    if(!if2.is_open())
    {
        cout << endl << "*** 2nd file (" << fn2 << ") not found ***"; return;
    }

    while(!(if1.eof() && if2.eof()))
    {
        if(req)
        {
            ty1 = find_line(if1, str1); ty2 = find_line(if2, str2);
        }

        if(ty1 < 0 && ty2 < 0)

            break;

        if(ty1 < 0 || ty2 < 0)
        {
            cout << endl << fn1 << (ty1 < 0 ? " short" : " long") << "er than " << fn2; break;
        }

        if(ty1 == 1)

            no1 = get_no(str1.begin() + 2); 

        if(ty2 == 1)

            no2 = get_no(str2.begin() + 2);

        if(cmp_nocase(str1, str2) == 0)
        {
            req = true; continue;
        }

        if(ty1 == 1 && ty2 == 1)
        {
            np_cnt += abs(no2 - no1); req = false;

            if(no2 < no1)
            {
                cout << endl << "extra test(s) in " << fn2 << ':' << endl << "  " << str2;
                no2 = sync(no1, if2, str2, np_cnt < 10);
            }
        
            if(no1 < no2)
            {
                cout << endl << "extra test(s) in " << fn1 << ':' << endl << "  " << str1;
                no1 = sync(no2, if1, str1, np_cnt < 10);
            }
        }
        else if(ty1 != ty2)
        {
            cout << endl << "*** synchronisation error tests " << no1 << " and " << no2 << " ***"; 
            return;
        }
        else if(ty1 >= 0)
        {
            cout << endl << "*** mismatch error test " << no1 << " *** ";
            
            if(++err_cnt < 6)
            
                cout << endl << "  line 1: " << str1 << endl << "  line 2: " << str2; 
        }
    }

    if(np_cnt && !err_cnt)

        cout << endl << "other tests match" << endl;

    else if(!err_cnt)

        cout << endl << fn1 << " and " << fn2 << " match" << endl;
};

bool test_args(int argc, char *argv[], char ch)
{
    for(int i = 1; i < argc; ++i)
    {
        if(argv[i][0] != '-' && argv[i][0] != '/')

            continue;

        if(argv[i][1] == tolower(ch) || argv[i][1] == toupper(ch))

            return true;
    }

    return false;
};

void main(int argc, char *argv[])
{   ofstream    outf;
    bool        do_cmp;
    string      name, dir1(dir_path), dir2(aes_path);

    if(argc == 1)
    {
        cout << endl << "usage: aes_gkat [/t] [/h] [/k] [/e] [/c] where:";
        cout << endl << "   /t: compare output and reference test file(s)";
        cout << endl << "   /h: author's byte order (frog, safer+ & serpent)";
        cout << endl << "   /k: generate ECB Known Answer Test files";  
        cout << endl << "   /e: generate ECB Monte Carlo Test files";
        cout << endl << "   /c: generate CBC Monte Carlo Test files";
        cout << endl << endl; exit(0);
    }

    cout << endl << "Generate and verify tests for the " << cipher_name()[0] << " algorithm: (" 
                    << cipher_name()[1] << ")" << endl;

    do_fss_hack = test_args(argc, argv, 'h');

    do_cmp = test_args(argc, argv, 't');

    if(test_args(argc, argv, 'k'))
    {
        name = dir1 + string(cipher_name()[0]) + string("\\ecb_vk.txt");

        outf.open(name.c_str(), ios_base::out);

        if(outf)
        {
            header(outf, 0); ecb_vk(outf); outf << endl; outf.close();
        
            if(do_cmp)
        
                comp_vecs(dir2 + cipher_name()[2] + "\\ecb_vk.txt", name);
        }

        name = dir1 + string(cipher_name()[0]) + string("\\ecb_vt.txt");

        outf.open(name.c_str(), ios_base::out);

        if(outf)
        {
            header(outf, 1); ecb_vt(outf); outf << endl; outf.close();
        
            if(do_cmp)
        
                comp_vecs(dir2 + cipher_name()[2] + "\\ecb_vt.txt", name);
        }
    }

    if(test_args(argc, argv, 'e'))
    {
        name = dir1 + string(cipher_name()[0]) + string("\\ecb_me.txt");

        outf.open(name.c_str(), ios_base::out);

        if(outf)
        {
            header(outf, 2); ecb_me(outf); outf << endl; outf.close();
            
            if(do_cmp)
            
                comp_vecs(dir2 + cipher_name()[2] + "\\ecb_e_m.txt", name);
        }

        name = dir1 + string(cipher_name()[0]) + string("\\ecb_md.txt");

        outf.open(name.c_str(), ios_base::out);

        if(outf)
        {
            header(outf, 3); ecb_md(outf); outf << endl; outf.close();
            
            if(do_cmp)
            
                comp_vecs(dir2 + cipher_name()[2] + "\\ecb_d_m.txt", name);
        }
    }

    if(test_args(argc, argv, 'c'))
    {
        name = dir1 + string(cipher_name()[0]) + string("\\cbc_me.txt");

        outf.open(name.c_str(), ios_base::out);

        if(outf)
        {
            header(outf, 4); cbc_me(outf); outf << endl; outf.close();
            
            if(do_cmp)
            
                comp_vecs(dir2 + cipher_name()[2] + "\\cbc_e_m.txt", name);
        }

        name = dir1 + string(cipher_name()[0]) + string("\\cbc_md.txt");

        outf.open(name.c_str(), ios_base::out);

        if(outf)
        {
            header(outf, 5); cbc_md(outf); outf << endl; outf.close();
    
            if(do_cmp)
            
                comp_vecs(dir2 + cipher_name()[2] + "\\cbc_d_m.txt", name);
        }
    }
};
