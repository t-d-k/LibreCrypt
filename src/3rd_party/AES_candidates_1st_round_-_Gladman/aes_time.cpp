
#include <iostream>
#include <iomanip>

#include "../std_defs.h"

u4byte rand32(void);

const unsigned int loops = 100;

double cycles(void)
{   u4byte  hi,lo;

    __asm   
    {
        _emit   0x0f
        _emit   0xa2
        _emit   0x0f
        _emit   0x31
        mov     lo,eax
        mov     hi,edx
        _emit   0x0f
        _emit   0xa2
    }

    return 4294967296.0 * hi + lo;
};

double e_cycles(const u4byte key_len)
{   u4byte      i, pt[4], ct[4], key[8];
    double      cy0, cy1, cy2, c1, c2;

    // set up a random key of 256 bits

    key[0] = rand32(); key[1] = rand32();
    key[2] = rand32(); key[3] = rand32();
    key[4] = rand32(); key[5] = rand32();
    key[6] = rand32(); key[7] = rand32();

    // set up a random plain text

    pt[0] = rand32(); pt[1] = rand32();
    pt[2] = rand32(); pt[3] = rand32();

    // do a set_key in case it is necessary

    set_key(key, key_len); c1 = c2 = 1.0e100;

    // do an encrypt to remove any 'first time through' effects

    encrypt(pt, ct);

    for(i = 0; i < loops; ++i)
    {
        pt[0] = rand32(); pt[1] = rand32();
        pt[2] = rand32(); pt[3] = rand32();

        cy0 = cycles();
        encrypt(pt, ct);
        cy1 = cycles();
        encrypt(pt, ct);
        encrypt(pt, ct);
        cy2 = cycles();

        cy2 -= cy1; cy1 -= cy0;

        c1 = (c1 > cy1 ? cy1 : c1);

        c2 = (c2 > cy2 ? cy2 : c2);
    }

    return c2 - c1;
};

double d_cycles(const u4byte key_len)
{   u4byte      i, pt[4], ct[4], key[8];
    double      cy0, cy1, cy2, c1, c2;

    // set up a random key of 256 bits

    key[0] = rand32(); key[1] = rand32();
    key[2] = rand32(); key[3] = rand32();
    key[4] = rand32(); key[5] = rand32();
    key[6] = rand32(); key[7] = rand32();

    pt[0] = rand32(); pt[1] = rand32();
    pt[2] = rand32(); pt[3] = rand32();

    // do a set_key in case it is necessary

    set_key(key, key_len); c1 = c2 = 1.0e100;

    // do an decrypt to remove any 'first time through' effects

    decrypt(pt, ct);

    for(i = 0; i < loops; ++i)
    {
        pt[0] = rand32(); pt[1] = rand32();
        pt[2] = rand32(); pt[3] = rand32();

        cy0 = cycles();
        decrypt(pt, ct);
        cy1 = cycles();
        decrypt(pt, ct);
        decrypt(pt, ct);
        cy2 = cycles();

        cy2 -= cy1; cy1 -= cy0;

        c1 = (c1 > cy1 ? cy1 : c1);

        c2 = (c2 > cy2 ? cy2 : c2);
    }

    return c2 - c1;
};

// measure cycles for one key setup

double k_cycles(const u4byte key_len)
{   u4byte      i, key[8];
    double      cy0, cy1, cy2, c1, c2;

    // set up a random key of 256 bits

    key[0] = rand32(); key[1] = rand32();
    key[2] = rand32(); key[3] = rand32();
    key[4] = rand32(); key[5] = rand32();
    key[6] = rand32(); key[7] = rand32();

    // do an set_key to remove any 'first time through' effects

    set_key(key, key_len); c1 = c2 = 1.0e100;

    for(i = 0; i < loops; ++i)
    {
        key[0] = rand32(); key[1] = rand32();
        key[2] = rand32(); key[3] = rand32();
        key[4] = rand32(); key[5] = rand32();
        key[6] = rand32(); key[7] = rand32();

        cy0 = cycles();
        set_key(key, key_len);
        cy1 = cycles();
        set_key(key, key_len);
        set_key(key, key_len);
        cy2 = cycles();

        cy2 -= cy1; cy1 -= cy0;

        c1 = (c1 > cy1 ? cy1 : c1);

        c2 = (c2 > cy2 ? cy2 : c2);
    }

    return c2 - c1;
};

using std::ios;
using std::cout;
using std::setprecision;
using std::setw;
using std::endl;

void output(const u4byte klen, const double kt, const double et, const double dt)
{   double  av;

    cout << endl << endl << setw(3) << klen << " bit key:";
    cout << "\nKey Setup:" <<  setprecision(0) << setw(8) << kt << " cycles";
    cout << endl << "Encrypt:  " << setprecision(0) << setw(8) << et << " cycles ="
         << setprecision(1) << setw(8) << 25600 / et << " mbits/sec";
    cout << endl << "Decrypt:  " << setprecision(0) << setw(8) << dt << " cycles ="
         << setprecision(1) << setw(8) << 25600 / dt << " mbits/sec";
    av = 0.5 * (et + dt);
    cout << endl << "Mean:     " << setprecision(0) << setw(8) << av << " cycles ="
         << setprecision(1) << setw(8) << 25600 / av << " mbits/sec";
};

void main(void)
{   double  kt, et, dt;
    u4byte  kl;

    cout << "Algorithm " << cipher_name()[0] << " (" << cipher_name()[1] << ')';

    cout.setf(ios::floatfield, ios::fixed);

    for(kl = 128; kl <= 256; kl += 64)
    {
        kt = k_cycles(kl); 
        dt = d_cycles(kl); 
        et = e_cycles(kl); 
        
        output(kl, kt, et, dt);
    }

    cout << endl << endl;
};
