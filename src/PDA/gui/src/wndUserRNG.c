// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include <windows.h>
#include <Windowsx.h>  // Needed for GET_X_LPARAM, GET_Y_LPARAM macros
#include <commctrl.h>
#include <stdlib.h> // Needed for itow(...)

#include "FreeOTFEDebug.h"
#include "FreeOTFEGUIlib.h"
#include "wndUserRNG.h"
#include "main.h"
#include "FreeOTFElib.h"

#define POINTS_TO_A_LINE  2

struct POINTLIST {
    POINT Point;
    struct POINTLIST* Prev;
    struct POINTLIST* Next;
};
typedef struct POINTLIST *PPOINTLIST;


typedef struct _USERRNG_DATA {
    CREATESTRUCT CreateStruct;

    // This stores the random data as it is generated
    FREEOTFEBYTE RandomByte;
    // This stores the number of random bits in RandomByte
    int RandomByteBits;

    int PointCount;
    PPOINTLIST LinesListHead;
    PPOINTLIST LinesListTail;
    POINT LastPnt;

    int TrailLines;
    int LineWidth;
    COLORREF LineColor;

    DWORD TimeOfLastSample;

    // Callbacks...
    USERRNG_BITGENPROC ProcBitGen;
    USERRNG_BYTEGENPROC ProcByteGen;
    USERRNG_SAMPLEPROC ProcSample;
} USERRNG_DATA, *PUSERRNG_DATA;


// =========================================================================
// Forward declarations...

void wndUserRNG_SetWindowData(HWND hWnd, USERRNG_DATA* WindowData);
USERRNG_DATA* wndUserRNG_GetWindowData(HWND hWnd);
void _wndUserRNG_DrawLine(
    HWND hWnd,
    int Mode,
    POINT A,
    POINT B
);


// =========================================================================
LRESULT CALLBACK wndUserRNG_HandleMsg_WM_CREATE(HWND hWnd, LPARAM lParam)
{
    LRESULT retval = -1;  // Assume failure
    USERRNG_DATA* WindowData;
    
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("wndUserRNG_HandleMsg_WM_CREATE\n")));

    WindowData = malloc(sizeof(*WindowData));
    if (WindowData != NULL)
        {
        memset(WindowData, 0, sizeof(WindowData));

        WindowData->RandomByte = 0;
        WindowData->RandomByteBits = 0;

        WindowData->PointCount = 0;
        WindowData->LinesListHead = NULL;
        WindowData->LinesListTail = NULL;
        WindowData->LastPnt.x = -1;
        WindowData->LastPnt.y = -1;

        WindowData->TrailLines = USERRNG_DEFAULT_TRAILLINES;
        WindowData->LineWidth = USERRNG_DEFAULT_LINEWIDTH;
        WindowData->LineColor = USERRNG_DEFAULT_LINECOLOR;

        WindowData->ProcBitGen = NULL;
        WindowData->ProcByteGen = NULL;
        WindowData->ProcSample = NULL;

        WindowData->CreateStruct = *((CREATESTRUCT*)lParam);

        wndUserRNG_SetWindowData(hWnd, WindowData); 

        WindowData->TimeOfLastSample = 0;

        // Success
        retval = 0;  
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("wndUserRNG_HandleMsg_WM_CREATE\n")));
    return retval;
}


// =========================================================================
// Remove the last point from the tail of the linked list of points
void RemoveLastPoint(USERRNG_DATA* WindowData)
{
    PPOINTLIST tmpPoint;

    if (WindowData->LinesListTail != NULL) 
        {
        tmpPoint = WindowData->LinesListTail;
        WindowData->LinesListTail = WindowData->LinesListTail->Next;
        if (WindowData->LinesListTail != NULL) 
            {
            WindowData->LinesListTail->Prev = NULL;
            }

        // Overwrite position before discarding record
        SecZeroAndFreeMemory(tmpPoint, sizeof(*tmpPoint));
        WindowData->PointCount--;
        }

  if (WindowData->LinesListTail == NULL) 
      {
      WindowData->LinesListHead = NULL;
      }

}


// =========================================================================
// Add a new last point onto the head of the linked list of points
void StoreNewPoint(
    USERRNG_DATA* WindowData,
    POINT pnt
)
{
    PPOINTLIST tmpPoint;

    tmpPoint = malloc(sizeof(*tmpPoint));
    if (tmpPoint != NULL)
        {
        tmpPoint->Point = pnt;
        tmpPoint->Next = NULL;
        tmpPoint->Prev = WindowData->LinesListHead;
        if (WindowData->LinesListHead != NULL) 
            {
            WindowData->LinesListHead->Next = tmpPoint;
            }
        WindowData->LinesListHead = tmpPoint;
        WindowData->PointCount++;

        if (WindowData->LinesListTail == NULL) 
            {
            WindowData->LinesListTail = WindowData->LinesListHead;
            }
        }

}


// =========================================================================
void wndUserRNG_CallbackBit(
  HWND hWnd,
  USERRNG_BIT Bit
)
{
    USERRNG_DATA* WindowData;
    NMUSERRNG_BITGEN nm;

    WindowData = wndUserRNG_GetWindowData(hWnd);

    // Callback proc
    if (WindowData->ProcBitGen != NULL)
        {
        WindowData->ProcBitGen(hWnd, Bit);
        }

    // If possible, notify parent hWnd
    if (WindowData->CreateStruct.hMenu != 0)
        {        
        nm.hdr.hwndFrom = hWnd;
        nm.hdr.idFrom = (UINT)WindowData->CreateStruct.hMenu;
        nm.hdr.code = URNG_BITGEN;
        nm.Bit = Bit;

        SendMessage(
                    WindowData->CreateStruct.hwndParent,
                    WM_NOTIFY,
                    (UINT)WindowData->CreateStruct.hMenu,
                    (LPARAM)(&nm)
                   );
        }

}

// =========================================================================
void wndUserRNG_CallbackByte(
  HWND hWnd,
  USERRNG_BYTE Byte
)
{
    USERRNG_DATA* WindowData;
    NMUSERRNG_BYTEGEN nm;

    WindowData = wndUserRNG_GetWindowData(hWnd);

    // Callback proc
    if (WindowData->ProcByteGen != NULL)
        {
        WindowData->ProcByteGen(hWnd, Byte);
        }

    // If possible, notify parent hWnd
    if (WindowData->CreateStruct.hMenu != 0)
        {        
        nm.hdr.hwndFrom = hWnd;
        nm.hdr.idFrom = (UINT)WindowData->CreateStruct.hMenu;
        nm.hdr.code = URNG_BYTEGEN;
        nm.Byte = Byte;

        SendMessage(
                    WindowData->CreateStruct.hwndParent,
                    WM_NOTIFY,
                    (UINT)WindowData->CreateStruct.hMenu,
                    (LPARAM)(&nm)
                   );
        }

}

// =========================================================================
void wndUserRNG_CallbackSample(
  HWND hWnd,
  POINT Pnt
)
{
    USERRNG_DATA* WindowData;
    NMUSERRNG_SAMPLE nm;

    WindowData = wndUserRNG_GetWindowData(hWnd);

    // Callback proc
    if (WindowData->ProcSample != NULL)
        {
        WindowData->ProcSample(hWnd, Pnt);
        }

    // If possible, notify parent hWnd
    if (WindowData->CreateStruct.hMenu != 0)
        {        
        nm.hdr.hwndFrom = hWnd;
        nm.hdr.idFrom = (UINT)WindowData->CreateStruct.hMenu;
        nm.hdr.code = URNG_SAMPLE;
        nm.Pnt = Pnt;

        SendMessage(
                    WindowData->CreateStruct.hwndParent,
                    WM_NOTIFY,
                    (UINT)WindowData->CreateStruct.hMenu,
                    (LPARAM)(&nm)
                   );
        }

}

// =========================================================================
void ProcessSample(
    HWND hWnd,
    POINT pnt
)
{
    USERRNG_DATA* WindowData;
    int i;

    WindowData = wndUserRNG_GetWindowData(hWnd);

    wndUserRNG_CallbackSample(hWnd, pnt);

    // This stores the random data as it is generated
    for (i = 1; i<= USERRNG_BITS_PER_SAMPLE; i++)
        {
        WindowData->RandomByte = (WindowData->RandomByte << 1);
        WindowData->RandomByte = (WindowData->RandomByte + ((FREEOTFEBYTE)(pnt.x & 1)));
        WindowData->RandomByteBits++;
        wndUserRNG_CallbackBit(hWnd, ((USERRNG_BIT)(pnt.x & 1)));

        WindowData->RandomByte = (WindowData->RandomByte << 1);
        WindowData->RandomByte = (WindowData->RandomByte + (FREEOTFEBYTE)(pnt.y & 1));
        WindowData->RandomByteBits++;
        wndUserRNG_CallbackBit(hWnd, ((USERRNG_BIT)(pnt.y & 1)));

        pnt.x = pnt.x >> 1;
        pnt.y = pnt.y >> 1;
        }

    if (WindowData->RandomByteBits >= 8) 
        {
        wndUserRNG_CallbackByte(hWnd, WindowData->RandomByte);

        WindowData->RandomByteBits = 0;
        WindowData->RandomByte = 0;
        }

}


// =========================================================================
void _wndUserRNG_TakeSampleIfMoved(HWND hWnd)
{
    BOOL changed = FALSE;
    USERRNG_DATA* WindowData;

    if (IsWindowEnabled(hWnd))
        {
        WindowData = wndUserRNG_GetWindowData(hWnd);

        // Handle the situation in which no mouse co-ordinates have yet been taken
        if (
            (WindowData->LastPnt.x > -1) &&
            (WindowData->LastPnt.y > -1)
           )
            {
            // If there are no points, we have a new one
            if (WindowData->PointCount == 0)
                {
                changed = TRUE;
                }
            else
                {
                // If the mouse cursor has moved a significant difference, use the new
                // co-ordinates
                // Both the X *and* Y mouse co-ordinates must have changed, to prevent
                // the user from generating non-random data by simply moving the mouse in
                // just a horizontal (or vertical) motion, in which case the X (or Y)
                // position would change, but the Y (or X) position would remain
                // relativly the same. This would only generate 1/2 as much random data
                // The effects of the following code are trivial to see; simply waggle
                // the mouse back and forth horizontally; instead of seeing a new dark
                // line appearing (indicating that the sample has been taken), the
                // inverse coloured line appears, indicating the mouse pointer
                if (
                    (
                     (WindowData->LastPnt.x > (WindowData->LinesListHead->Point.x+USERRNG_MIN_DIFF)) || 
                     (WindowData->LastPnt.x < (WindowData->LinesListHead->Point.x-USERRNG_MIN_DIFF))
                    ) &&
                    (
                     (WindowData->LastPnt.y > (WindowData->LinesListHead->Point.y+USERRNG_MIN_DIFF)) ||
                     (WindowData->LastPnt.y < (WindowData->LinesListHead->Point.y-USERRNG_MIN_DIFF))
                    )
                   ) 
                    {
                    changed = TRUE;
                    }       
                }
            }

        if (!(changed))
            {
            // User hasn't moved cursor - delete oldest line until we catch up with
            // the cursor
            if ( 
                (WindowData->LinesListTail != WindowData->LinesListHead) &&
                (WindowData->LinesListTail != NULL)
               ) 
                {
                _wndUserRNG_DrawLine(
                                     hWnd,
                                     R2_MERGENOTPEN,
                                     WindowData->LinesListTail->Point,
                                     WindowData->LinesListTail->Next->Point
                                    );
                RemoveLastPoint(WindowData);
                }

            }
        else
            {
            // AT THIS POINT, WE USE LastMouseX AND LastMouseY AS THE CO-ORDS TO USE

            // Store the position
            StoreNewPoint(WindowData, WindowData->LastPnt);

            // User moved cursor - don't delete any more lines unless the max number
            // of lines which may be displayed is exceeded
            if ( 
                ((WindowData->PointCount+1) > WindowData->TrailLines) &&
                (WindowData->PointCount > 1)
               ) 
                {
                _wndUserRNG_DrawLine(
                                     hWnd,
                                     R2_MERGENOTPEN,
                                     WindowData->LinesListTail->Point,
                                     WindowData->LinesListTail->Next->Point
                                    );
                RemoveLastPoint(WindowData);
                }


            // Draw newest line
            if (
                (WindowData->TrailLines > 0) &&
                (WindowData->PointCount > 1)
               )
                {
                _wndUserRNG_DrawLine(
                                     hWnd,
                                     R2_COPYPEN,
                                     WindowData->LinesListHead->Prev->Point,
                                     WindowData->LinesListHead->Point
                                    );
                }


            ProcessSample(hWnd, WindowData->LastPnt);
            }

        }

}


// =========================================================================
LRESULT CALLBACK wndUserRNG_HandleMsg_WM_ENABLE(HWND hWnd, WPARAM wParam)
{
    BOOL enabled = (BOOL)wParam;
    USERRNG_DATA* WindowData;

    WindowData = wndUserRNG_GetWindowData(hWnd);

    if (enabled)
        {
        WindowData->TimeOfLastSample = 0;
        }

    // Redraw to get enabled/disabled background
    RedrawWindow(
                 hWnd, 
                 NULL, 
                 NULL, 
                 (RDW_ERASE | RDW_INVALIDATE)
                );

    return 0;  // Processed WM_ENABLE successfully
}


// =========================================================================
LRESULT CALLBACK wndUserRNG_HandleMsg_WM_ERASEBKGND(HWND hWnd, WPARAM wParam)
{
    LRESULT retval = 0; // Zero indicates failure
    HDC hDC;
    RECT rect;
    HBRUSH hBrush;
    
    hDC = (HDC)wParam;

    if (IsWindowEnabled(hWnd))
        {
        hBrush = GetSysColorBrush(COLOR_WINDOW);
        }
    else
        {
        hBrush = GetSysColorBrush(COLOR_BTNFACE);
        }

    if (GetClientRect(hWnd, &rect))
        {
        FillRect(hDC, &rect, hBrush);
        retval = 1;  // Nonzero means we processed the message
        }

    return retval;
}


// =========================================================================
LRESULT CALLBACK wndUserRNG_HandleMsg_WM_CLOSE(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("wndUserRNG_HandleMsg_WM_CLOSE\n")));

    //xxx - is this called?
    DestroyWindow(hWnd);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("wndUserRNG_HandleMsg_WM_CLOSE\n")));
    return 0;
}


// =========================================================================
LRESULT CALLBACK wndUserRNG_HandleMsg_WM_DESTROY(HWND hWnd)
{
    USERRNG_DATA* WindowData;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("wndUserRNG_HandleMsg_WM_DESTROY\n")));

    WindowData = wndUserRNG_GetWindowData(hWnd);
    SecZeroAndFreeMemory(WindowData, sizeof(*WindowData));
    WindowData = NULL;
    wndUserRNG_SetWindowData(hWnd, WindowData);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("wndUserRNG_HandleMsg_WM_DESTROY\n")));
    return 0;
}


// =========================================================================
void wndUserRNG_SetWindowData(HWND hWnd, USERRNG_DATA* WindowData)
{
    SetWindowLong(hWnd, 0, (LONG)WindowData);
}


// =========================================================================
USERRNG_DATA* wndUserRNG_GetWindowData(HWND hWnd)
{
    return (USERRNG_DATA*)GetWindowLong(hWnd, 0);
}


// =========================================================================
void _wndUserRNG_DrawLine(
    HWND hWnd,
    int Mode,
    POINT A,
    POINT B
)
{
    USERRNG_DATA* WindowData;
    HPEN hPen;
    HDC hDC;
    POINT pts[POINTS_TO_A_LINE];

    WindowData = wndUserRNG_GetWindowData(hWnd);

    hDC = GetDC(hWnd);

    hPen = CreatePen( 
                     PS_SOLID, 
                     WindowData->LineWidth, 
                     WindowData->LineColor
                    );

    SelectObject(hDC, hPen);
    SetROP2(hDC, Mode);

    pts[0] = A;
    pts[1] = B;
    Polyline(hDC, pts, POINTS_TO_A_LINE);

    DeleteObject(hPen);

}


// =========================================================================
void wndUserRNG_HandleMsg_WM_MOUSEMOVE(
                             HWND hWnd,
                             LPARAM lParam
                            )
{
    USERRNG_DATA* WindowData;
    DWORD sampleInterval;
    DWORD currTime;

    if (IsWindowEnabled(hWnd))
        {
        WindowData = wndUserRNG_GetWindowData(hWnd);

        if (
            (WindowData->TrailLines > 0) &&
            (WindowData->PointCount >= 1)
           )
            {
            _wndUserRNG_DrawLine(
                                 hWnd,
                                 R2_XORPEN,
                                 WindowData->LinesListHead->Point,
                                 WindowData->LastPnt
                                );
            }

        WindowData->LastPnt.x = LOWORD(lParam);
        WindowData->LastPnt.y = HIWORD(lParam);

        if (
            (WindowData->TrailLines > 0) &&
            (WindowData->PointCount >= 1)
           )
            {
            _wndUserRNG_DrawLine(
                                 hWnd,
                                 R2_XORPEN,
                                 WindowData->LinesListHead->Point,
                                 WindowData->LastPnt
                                );
            }

        // Prevent potential problems with tick count looping back to 0
        currTime = GetTickCount();
        if (WindowData->TimeOfLastSample > currTime)
            {
            sampleInterval = WindowData->TimeOfLastSample - currTime;
            }
        else
            {
            sampleInterval = currTime - WindowData->TimeOfLastSample;
            }

        if (sampleInterval > USERRNG_TIMER_INTERVAL)
            {
            _wndUserRNG_TakeSampleIfMoved(hWnd);
            WindowData->TimeOfLastSample = GetTickCount();
            }
        }

}


// =========================================================================
LRESULT CALLBACK wndUserRNG_Proc(
                             HWND hWnd,
                             UINT msg,
                             WPARAM wParam,
                             LPARAM lParam
                            )
{
    LRESULT retval = 0;
    // xxx - junk this - static SHACTIVATEINFO s_sai = {0};

    switch(msg)
        {
        case WM_CREATE:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_CREATE\n")));
            // xxx - junk this -  s_sai.cbSize = sizeof(SHACTIVATEINFO);
            retval = wndUserRNG_HandleMsg_WM_CREATE(hWnd, lParam);
            break;
            }

        case WM_ENABLE:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_ENABLE\n")));
            wndUserRNG_HandleMsg_WM_ENABLE(hWnd, wParam);
            break;
            }

        case WM_ERASEBKGND:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_ERASEBKGND\n")));
            retval = wndUserRNG_HandleMsg_WM_ERASEBKGND(hWnd, wParam);
            break;
            }

        case WM_ACTIVATE:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_ACTIVATE\n")));
            // xxx - junk this - SHHandleWMActivate(hWnd, wParam, lParam, &s_sai, 0);
            break;
            }

        case WM_SETTINGCHANGE:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_SETTINGCHANGE\n")));
            // xxx - junk this - SHHandleWMSettingChange(hWnd, wParam, lParam, &s_sai);
            break;
            }

        case WM_CLOSE:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_CLOSE\n")));
            retval = wndUserRNG_HandleMsg_WM_CLOSE(hWnd);
            break;
            }

        case WM_DESTROY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_DESTROY\n")));
            retval = wndUserRNG_HandleMsg_WM_DESTROY(hWnd);
            break;
            }
            
        case WM_MOUSEMOVE:
            {
            wndUserRNG_HandleMsg_WM_MOUSEMOVE(hWnd, lParam);
            break;
            }

        default:
            {
            retval = DefWindowProc(hWnd, msg, wParam, lParam);
            }

        }  // switch(msg)

    return retval;
}


// =========================================================================
BOOL WndUserRNGRegister()
{
    WNDCLASS wc;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("WndMainRegister\n")));

    InitCommonControls();

    wc.style         = (CS_HREDRAW | CS_VREDRAW) ;
    wc.lpfnWndProc   = (WNDPROC)wndUserRNG_Proc;
    wc.cbClsExtra    = 0;
    wc.cbWndExtra    = sizeof(PUSERRNG_DATA);
    wc.hIcon         = NULL;
    wc.hInstance     = G_hInstance;
    wc.hCursor       = NULL; // No cursor for WinCE
    // wc.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH);    
    //wc.hbrBackground = (HBRUSH)GetStockObject(LTGRAY_BRUSH);
    wc.hbrBackground = NULL;  // We use WM_ERASEBKGND
    wc.lpszMenuName  = NULL; // Must be set to NULL; WinCE doesn't support
                             // "MAKEINTRESOURCE(IDR_MAINMENU)" here
    wc.lpszClassName = USERRNG_CONTROL;

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("WndMainRegister\n")));
    return (RegisterClass(&wc) != 0);
}


// =========================================================================
// =========================================================================
