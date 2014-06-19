// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _wndUserRNG_H
#define _wndUserRNG_H   1

#include <Windows.h>  // Required for RGB(...) macro
#define USERRNG_CONTROL TEXT("UserRNG")


// The distance the cursor has to be moved in either the X or Y direction
// before the new mouse co-ordinates are taken as another "sample"
// DANGER - If this is set too high, then as the mouse moves, samples will
//          be the mouse cursor co-ordinates may end up only incrementing my
//          "MIN_DIFF" each time a sample is taken
// DANGER - If this is set to 1, then with some mice, the mouse pointer moves
//          "on it's own" when left alone, and the mouse pointer may
//          oscillate between two adjacent pixels
// Set this value to 2 to reduce the risk of this happening
#define USERRNG_MIN_DIFF  2

// This specifies how many of the least significant bits from the X & Y
// co-ordinates will be used as random data.
// If this is set too high, then
// If this is set too low, then the user has to move the mouse a greater
// distance between samples, otherwise the higher bits in the sample won't
// change.
// A setting of 1 will use the LSB of the mouse's X, Y co-ordinates
#define USERRNG_BITS_PER_SAMPLE  1

// This specifies the time interval (in ms) between taking samples of the
// mouse's position
#define USERRNG_TIMER_INTERVAL  100


#define USERRNG_DEFAULT_TRAILLINES 5
#define USERRNG_DEFAULT_LINEWIDTH 5
// Navy color
#define USERRNG_DEFAULT_LINECOLOR RGB(0, 0, 128)

#define USERRNG_BYTE unsigned char
#define USERRNG_BIT unsigned char

BOOL WndUserRNGRegister();

LRESULT CALLBACK wndUserRNGR_Proc(
                             HWND hwnd,
                             UINT msg,
                             WPARAM wParam,
                             LPARAM lParam
                            );

typedef struct tagNMUSERRNG_BITGEN
{
    NMHDR hdr;
    USERRNG_BIT Bit;
}   NMUSERRNG_BITGEN;
typedef NMUSERRNG_BITGEN FAR * LPNMUSERRNG_BITGEN;
#define URNG_BITGEN  (WM_USER + 1)

typedef struct tagNMUSERRNG_BYTEGEN
{
    NMHDR hdr;
    USERRNG_BYTE Byte;
}   NMUSERRNG_BYTEGEN;
typedef NMUSERRNG_BYTEGEN FAR * LPNMUSERRNG_BYTEGEN;
#define URNG_BYTEGEN  (WM_USER + 2)

typedef struct tagNMUSERRNG_SAMPLE
{
    NMHDR hdr;
    POINT Pnt;
}   NMUSERRNG_SAMPLE;
typedef NMUSERRNG_SAMPLE FAR * LPNMUSERRNG_SAMPLE;
#define URNG_SAMPLE  (WM_USER + 3)



typedef LRESULT (CALLBACK* USERRNG_BITGENPROC)(HWND, USERRNG_BIT Bit);
typedef LRESULT (CALLBACK* USERRNG_BYTEGENPROC)(HWND, USERRNG_BYTE Byte);
typedef LRESULT (CALLBACK* USERRNG_SAMPLEPROC)(HWND, POINT Pnt);

// =========================================================================
// =========================================================================

#endif

