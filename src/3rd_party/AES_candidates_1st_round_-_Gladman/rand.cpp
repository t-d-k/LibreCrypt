
//  This package makes available Marsaglia's highly portable generator
//  of uniformly distributed pseudo-random numbers.
//
//  The sequence of 24 bit pseudo-random numbers produced has a period
//  of about 2**144, and has passed stringent statistical tests for
//  randomness and independence.
//
//  Supplying a seed or seeds to init_rand(s1, s2) is optional.
//
//  The correspondence between pairs of seeds and generated sequences
//  of pseudo-random numbers is many-to-one.
//
//  References:
//      M G Harmon & T P Baker, ``An Ada Implementation of Marsaglia's
//      "Universal" Random Number Generator'', Ada Letters, late 1987.
//
//      G Marsaglia, ``Toward a universal random number generator'',
//      to appear in the Journal of the American Statistical Association.

//  output values are in the range 0..2^(24)-1

const unsigned long  two_to_24     = 0x01000000;
const unsigned short state_size    =         97;

const unsigned short p             =        179;
const unsigned short pm1           =      p - 1;
const unsigned short q             =     p - 10;

const unsigned long init_c         =     362436;
const unsigned long cd             =    7654321;
const unsigned long cm             =   16777213;

// The state of the generator is described by the following
// variables

unsigned short  index_i;        //  [0..state_size-1]
unsigned short  index_j;        //  [0..state_size-1]
unsigned long   u[state_size];  //  the state array
unsigned long   c;
bool            init_done = false;

// This procedure initialises the state table u for a lagged
// Fibonacci sequence generator, filling it with random bits
// from a small multiplicative congruential sequence.    The
// auxilliaries c, index_i and index_j are also initialized.
// The seeds are transformed into an initial state in such a
// way that identical results are obtained on a wide variety
// of machines.

    // Return a value between 0 and size-1 inclusive.  This value
    // will be anyint itself if possible, otherwise another value
    // in the required interval.

    unsigned short col(short anyint, unsigned short size)
    {   short   i = anyint;

        if (i < 0)
        {   i = - (i / 2);
        }

        while (i >= size)
        {
            i = i / 2;
        }

        return i;
    }

void init_rand(unsigned short seed_a = 1991, unsigned short seed_b = 9880)
{   unsigned long   s,bit;
    unsigned short  ii,jj,kk,mm;    // [0..p-1]
    unsigned short  ll;             // [0..q-1]
    short           sd;

    sd = col(seed_a, pm1 * pm1);
    ii = 1 + sd / pm1; jj = 1 + sd % pm1;

    sd = col(seed_b, pm1 * q);
    kk = 1 + sd / pm1; ll = sd % q;

    if (ii == 1 && jj == 1 && kk == 1)
        ii = 2;

    for(short ind = 0; ind < state_size; ind++)
    {
        s = 0; bit = 1;

        do
        {
            mm = (((ii * jj) % p) * kk) % p;

            ii = jj; jj = kk; kk = mm;

            ll = (53 * ll + 1) % q;

            if ((ll * mm) & 0x0020)
                s += bit;

            bit <<= 1;
        }
        while
            (bit < two_to_24);

        u [ind] = s;
    }

    index_i = state_size - 1; index_j = state_size / 3; c  = init_c;

    init_done = true;
}

// Return uniformly distributed pseudo random numbers in the range
// 0..2^(24)-1 inclusive. There are 2^24 possible return values.

unsigned long rand24(void)
{   unsigned long   temp;

    if(!init_done)

        init_rand();

    c = (c < cd ? c + (cm - cd) : c - cd);

    temp = (u[index_i] -= u[index_j]);

    if (!index_i--)
        index_i = state_size - 1;

    if (!index_j--)
        index_j = state_size - 1;

    return (temp - c) & (two_to_24 - 1);
}

// Return uniformly distributed pseudo random number in the range
// 0..2^(32)-1 inclusive. There are 2^32 possible return values.

static unsigned char    buf[13];    // buffer for conversion from 24 to
                                    // 32 bit output
static unsigned short   bno = 0;    // number of output values in buffer

unsigned long rand32(void)
{
    if(!bno)
    {   for(unsigned short i = 0; i < 12; i += 3)
            *(unsigned long*)(buf + i) = rand24();
        bno = 12;
    }
    bno -= 4; return *(unsigned long*)(buf + 8 - bno);
}

