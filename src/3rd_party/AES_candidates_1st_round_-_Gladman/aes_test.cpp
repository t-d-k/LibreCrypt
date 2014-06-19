
//  Run and Compare AES Tests For Known Answer and Monte Carlo Tests

//  Directory for AES test vector files and locally generated copies

char    *aes_path = "i:\\testvals\\";
char    *dir_path = "d:\\cpp\\aes\\";

#include "../std_defs.h"

#include <iostream>
#include <fstream>
#include <iomanip>
#include <cctype>
#include <string>

using std::string;
using std::getline;
using std::ios;
using std::cout;
using std::ifstream;
using std::endl;

enum test_type  { ecb_vk, ecb_vt, ecb_me, ecb_md, cbc_me, cbc_md };

string  fstr[6] = { "KEYSIZE=", "I=", "IV=", "KEY=", "PT=", "CT=" };

inline int to_hex(char ch)
{
    return (ch & 0x0f) +  (ch > '9' ? 9 : 0);
};

int hval_in(string::iterator p, string::iterator top, char *buf)
{   int i = 0;

    if((top - p) & 1)

        return -1;

    while(p < top)
    {
        if(isxdigit(*p) && isxdigit(*(p + 1)))
        {
            buf[i++] = (to_hex(*p) << 4) + to_hex(*(p + 1)); p += 2;
        }
        else

            break;
    }

    return i;
};

int get_dec(string s)
{   string::const_iterator  p = s.begin();
    int                     nbr = 0;

    while(p != s.end() && *p >= '0' && *p <= '9')
    {
        nbr = 10 * nbr + (*p - '0'); ++p;
    }

    return nbr;
};

int find_line(ifstream &inf, string &str, string fstr[])
{   string::size_type   pos;
    int                 ty1;

    while(!inf.eof())
    {
        getline(inf, str);

        for(ty1 = 0; ty1 < 6; ++ty1)
        {
            if((pos = str.find(fstr[ty1])) != string::npos && !pos)
            {
                return ty1;
            }
        }
    }

    return -1;
};

static bool on_screen = false;

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

void ref_test(const string &in_file, const int it_cnt, test_type t_type)
{   u4byte      i, kl, test_no, cnt, e_cnt, *kp;
    u4byte      key[8], pt[4], iv[4], ect[4], act[8];
    ifstream    inf;
    string      str;
    int         ty;

    cout << endl << "Test file: " << in_file << endl << "Status: ";

    inf.open(in_file.c_str(), ios::in);

    if(!inf.is_open())
    {
        cout << "error in running test" << endl; return;
    }

    cnt = 0; e_cnt = test_no = 0;

    while(!inf.eof())
    {
        ty = find_line(inf, str, fstr);

        if(ty < 0)

            continue;

        switch(ty)
        {
          case 0:   kl = get_dec(str.begin() + 8); continue;
          case 1:   test_no = get_dec(str.begin() + 2); continue;
          case 2:   hval_in(str.begin()+ 3, str.end(), reinterpret_cast<char*>(iv)); continue;
          case 3:   hval_in(str.begin()+ 4, str.end(), reinterpret_cast<char*>(key)); continue;
          case 4:   hval_in(str.begin()+ 3, str.end(), reinterpret_cast<char*>(pt)); 
                    if(t_type != ecb_md && t_type != cbc_md)
                        continue;
                    break;
          case 5:   hval_in(str.begin()+ 3, str.end(), reinterpret_cast<char*>(ect)); 
                    if(t_type == ecb_md || t_type == cbc_md)
                        continue;
                    break;
        }

        kp = set_key(key, kl);

        if(it_cnt > 100 && test_no % 40 == 0)

            put_count(test_no);

        if(t_type == ecb_md || t_type == cbc_md)
        {
            act[0] = ect[0]; act[1] = ect[1]; act[2] = ect[2]; act[3] = ect[3];

            if(t_type == cbc_md)
            {
                act[4] = iv[0]; act[5] = iv[1]; act[6] = iv[2]; act[7] = iv[3];

                for(i = 0; i < it_cnt; i += 2)
                {
                    decrypt(act, ect);

                    act[4] ^= ect[0]; act[5] ^= ect[1]; act[6] ^= ect[2]; act[7] ^= ect[3];

                    decrypt(act + 4, ect);

                    act[0] ^= ect[0]; act[1] ^= ect[1]; act[2] ^= ect[2]; act[3] ^= ect[3];
                }
            }
            else
            
                for(i = 0; i < it_cnt; ++i)
        
                    decrypt(act, act);

            if(pt[0] != act[0] || pt[1] != act[1] || pt[2] != act[2] || pt[3] != act[3])
            {
                cout << endl << "encryption error on test " << test_no; e_cnt++;
            }

            if(t_type != cbc_md)
            {
                for(i = 0; i < it_cnt; ++i)

                    encrypt(act, act); 

                if(ect[0] != act[0] || ect[1] != act[1] || ect[2] != act[2] || ect[3] != act[3])
                {   
                    cout << endl << "decryption error on test " << test_no; e_cnt++;
                }
            }
        }
        else
        {
            if(t_type == cbc_me)
            {
                act[0] = iv[0]; act[1] = iv[1]; act[2] = iv[2]; act[3] = iv[3];
                act[4] = pt[0]; act[5] = pt[1]; act[6] = pt[2]; act[7] = pt[3];

                for(i = 0; i < it_cnt; i += 2)
                {
                    act[4] ^= act[0]; act[5] ^= act[1]; act[6] ^= act[2]; act[7] ^= act[3]; 

                    encrypt(act + 4, act + 4);

                    act[0] ^= act[4]; act[1] ^= act[5]; act[2] ^= act[6]; act[3] ^= act[7]; 

                    encrypt(act, act);
                }
            }
            else
            {
                act[0] = pt[0]; act[1] = pt[1]; act[2] = pt[2]; act[3] = pt[3];

                for(i = 0; i < it_cnt; ++i)
        
                    encrypt(act, act);
            }

            if(ect[0] != act[0] || ect[1] != act[1] || ect[2] != act[2] || ect[3] != act[3])
            {
                cout << endl << "encryption error on test " << test_no; e_cnt++;
            }
        
            if(t_type != cbc_me)
            {
                for(i = 0; i < it_cnt; ++i)

                    decrypt(act, act); 

                if(pt[0] != act[0] || pt[1] != act[1] || pt[2] != act[2] || pt[3] != act[3])
                {   
                    cout << endl << "decryption error on test " << test_no; e_cnt++;
                }
            }
        }
    }

    inf.close(); clear_count();

    if(e_cnt > 0)

        cout << endl << e_cnt << " errors during test" << endl;

    else

        cout << "all tests correct" << endl;
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
{   string  name, path;

    if(argc == 1)
    {
        cout << endl << "usage: aes_test [/k] [/e] [/c] where:";
        cout << endl << "   /k: run ECB Known Answer Tests";    
        cout << endl << "   /e: run ECB Monte Carlo Tests";
        cout << endl << "   /c: run CBC Monte Carlo Tests";
        cout << endl << endl; exit(0);
    }

    cout << endl << "Run tests for the " << cipher_name()[0] << " algorithm: (" 
                    << cipher_name()[1] << ")" << endl;

    path = string(dir_path) + string(cipher_name()[0]);
    
    if(test_args(argc, argv, 'k'))
    {
        name = path + string("\\ecb_vk.txt"); ref_test(name, 1, ecb_vk);

        name = path + string("\\ecb_vt.txt"); ref_test(name, 1, ecb_vt);
    }

    if(test_args(argc, argv, 'e'))
    {
        name = path + string("\\ecb_me.txt");  ref_test(name, 10000, ecb_me);

        name = path + string("\\ecb_md.txt");  ref_test(name, 10000, ecb_md);
    }

    if(test_args(argc, argv, 'c'))
    {
        name = path + string("\\cbc_me.txt");  ref_test(name, 10000, cbc_me);

        name = path + string("\\cbc_md.txt");  ref_test(name, 10000, cbc_md);
    }

    cout << endl;
};
