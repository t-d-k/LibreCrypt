// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include <stdlib.h>  // Required for _ultow(...)
#include <storemgr.h>  // Required for STORAGE_DEVICE_TYPE_PCCARD, etc
#include <Commdlg.h>  // Required from OPENFILENAME
#include <winuser.h>  // Required for IDOK, etc
#include <aygshell.h>  // Required for SH... functions/definitions/etc
#pragma comment(lib, "aygshell")  // Link in aygshell.lib

#include "main.h"
#include "dlgNewVolWizard.h"
#include "dlgHashInfo.h"
#include "dlgCypherInfo.h"
#include "FreeOTFEDebug.h"
#include "DriverInterface.h"
#include "DriverInterfaceTwo.h"
#include "DriverInterfaceHash.h"
#include "DriverInterfaceCypher.h"
#include "DriverInterfaceCommon.h"
#include "FreeOTFElib.h"
#include "FreeOTFEGUIlib.h"
#include "FreeOTFE4PDAGUIlib.h"
#include "FreeOTFE4PDAlib.h"
#include "FreeOTFEAPIConstsCommon.h"
#include "resource.h"
#include "SDUGeneral.h"
#include "wndUserRNG.h"
#include "SDUi18n.h"
#include "SDUi18n_GUI.h"


// =========================================================================
// Constants...

#define PAGE_IDX_WELCOME            0
#define PAGE_IDX_FILENAME           1
#define PAGE_IDX_OFFSET             2
#define PAGE_IDX_SIZE               3
#define PAGE_IDX_HASHCYPHERIV       4
#define PAGE_IDX_MASTERKEYLEN       5
#define PAGE_IDX_RNGSELECT          6
#define PAGE_IDX_RNGMOUSEMOVEMENT   7
#define PAGE_IDX_PASSWORD           8
#define PAGE_IDX_READYFORNEWVOL     9
#define PAGE_IDX_KEYITERATIONS     10
#define PAGE_IDX_SALT              11
#define PAGE_IDX_CDBLOCATION       12
#define PAGE_IDX_SUMMARY           13

// The number of steps in the wizard
// Note: Mind - the indexes start from zero!
#define NEWVOLWIZARD_STEPS  (PAGE_IDX_SUMMARY + 1)

const SECTOR_IV_GEN_METHOD ARR_SECTOR_IV_GEN_METHOD[] = {
                                            SCTRIVGEN_NONE,
                                            SCTRIVGEN_32BIT_SECTOR_ID,
                                            SCTRIVGEN_64BIT_SECTOR_ID,
                                            SCTRIVGEN_HASH_32BIT_SECTOR_ID,
                                            SCTRIVGEN_HASH_64BIT_SECTOR_ID,
                                            SCTRIVGEN_ESSIV,
                                            SCTRIVGEN_UNKNOWN
                                            };
#define EDIT_CRLF TEXT("\r\n")
#define BYTES_IN_MEGABYTE (1024 * 1024)

#define DEFAULT_AUTOMOUNT_AFTER_CREATE  TRUE


// Arbitary control ID
#define IDCTRL_USERRNG 125


// =========================================================================
// Structures...

typedef struct _NEW_VOL_PARAMETERS {
    BOOL ParamsOK;
    int ErrorPageIdx;

    WCHAR Filename[FREEOTFE_MAX_FILENAME_LENGTH];

    LARGE_INTEGER Size;  // In bytes

    LARGE_INTEGER Offset;

    WCHAR PrettyNameHash[MAX_PRETTYPRINTED_TITLE];
    WCHAR DriverHash[FREEOTFE_MAX_FILENAME_LENGTH];
    GUID ImplGUIDHash;
    WCHAR PrettyNameCypher[MAX_PRETTYPRINTED_TITLE];
    WCHAR DriverCypher[FREEOTFE_MAX_FILENAME_LENGTH];
    GUID ImplGUIDCypher;
    SECTOR_IV_GEN_METHOD SectorIVGenMethod;
    BOOL XORIV;

    int BlockSizeSelectedCypher;
    int KeySizeSelectedCypher;
    int KeySizeUser;

    RNG_SYSTEM SelectedRNG;  // Combination of the RNGs

    // The following buffers store random data as collected...
    int RandomPoolFilled_UserInput;  // In *bits*
    FREEOTFEBYTE RandomPool_UserInput[(CRITICAL_DATA_LENGTH / 8)];
    int RandomPoolFilled_MSCryptoAPI;  // In *bits*
    FREEOTFEBYTE RandomPool_MSCryptoAPI[(CRITICAL_DATA_LENGTH / 8)];

    unsigned char* UserPassword;
    unsigned char* UserPasswordConfirm;

    BOOL MountAfterCreation;

    unsigned int KeyIterations;

    unsigned int SaltLength;  // In *bits*

    BOOL UseKeyfile;
    WCHAR Keyfile[FREEOTFE_MAX_FILENAME_LENGTH];

} NEW_VOL_PARAMETERS, *PNEW_VOL_PARAMETERS;


// =========================================================================
// Local to this file...

HWND G_dlgNewVolWizard_MenuBar = NULL;
HWND G_dlgNewVolWizard_UserRNG = NULL;
BOOL G_dlgNewVolWizard_PageCompleted[NEWVOLWIZARD_STEPS];
NEW_VOL_PARAMETERS G_dlgNewVolWizard_NewVolParams;
WCHAR* G_dlgNewVolWizard_Mountpoint;

BOOL G_dlgNewVolWizard_GotImplDetails = FALSE;
int G_dlgNewVolWizard_CountImplCypher = 0;
DRIVER_AND_ALG* G_dlgNewVolWizard_ImplCypher = NULL;  // Array of options
int G_dlgNewVolWizard_CountImplHash = 0;
DRIVER_AND_ALG* G_dlgNewVolWizard_ImplHash = NULL;  // Array of options

BOOL G_dlgNewVolWizard_AllowFinish = FALSE;

int G_dlgNewVolWizard_RandomUserInput_Filled;  // In *bits*
FREEOTFEBYTE G_dlgNewVolWizard_RandomUserInput_Data[(CRITICAL_DATA_LENGTH / 8)];

// =========================================================================
// Forward declarations...

void _dlgNewVolWizard_EnableDisableControls_CDBLocation(HWND hDlg);
void _dlgNewVolWizard_EnableDisableControls_HashCypherIV(HWND hDlg);

BOOL CALLBACK dlgNewVolWizard_CreateNew(HWND hDlg);
BOOL CALLBACK dlgNewVolWizard_PropSheetProc(
                           HWND hDlg, 
                           UINT message, 
                           LPARAM lParam
                          );
BOOL CALLBACK dlgNewVolWizard_PropSheetPageProc(
                      HWND hDlg, 
                      UINT msg, 
                      WPARAM wParam, 
                      LPARAM lParam
                     );
void _dlgNewVolWizard_SecZeroParams();
BOOL dlgNewVolWizard_ValidatePage(HWND hDlg);
BOOL dlgNewVolWizard_ValidateAll(HWND hDlg);
void _dlgNewVolWizard_PopulateComboboxes(HWND hDlg);

BOOL _dlgNewVolWizard_QuerySiblings(HWND hDlg);
int _dlgNewVolWizard_GetCurrPageIdx(HWND hDlg);
int _dlgNewVolWizard_GetPageIdxOfPage(HWND hDlg);


// =========================================================================
void DisplayDlgNewVolWizard(HWND hWnd)
{
    PROPSHEETHEADER propHeader;
    PROPSHEETPAGE propPage[NEWVOLWIZARD_STEPS];
    int i;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("DisplayDlgNewVolWizard\n")));

    for (
         i = 0;
         i < (sizeof(G_dlgNewVolWizard_PageCompleted) / sizeof(G_dlgNewVolWizard_PageCompleted[0]));
         i++
        )
        {
        G_dlgNewVolWizard_PageCompleted[i] = FALSE;
        }

    memset(
           &G_dlgNewVolWizard_NewVolParams, 
           0,
           sizeof(G_dlgNewVolWizard_NewVolParams)
          );

    // Zero unused members...
    memset(propPage, 0, sizeof(propPage));


    // Welcome step...
    propPage[PAGE_IDX_WELCOME].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_WELCOME].lParam      = PAGE_IDX_WELCOME;
    propPage[PAGE_IDX_WELCOME].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_WELCOME].pszTitle    = _("Welcome");
    propPage[PAGE_IDX_WELCOME].hInstance   = G_hInstance;
    propPage[PAGE_IDX_WELCOME].pszTemplate = MAKEINTRESOURCE(IDD_WIZARD_WELCOME);
    propPage[PAGE_IDX_WELCOME].pfnDlgProc  = dlgNewVolWizard_PropSheetPageProc;

    // Filename step...
    propPage[PAGE_IDX_FILENAME].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_FILENAME].lParam      = PAGE_IDX_FILENAME;
    propPage[PAGE_IDX_FILENAME].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_FILENAME].pszTitle    = _("Filename");
    propPage[PAGE_IDX_FILENAME].hInstance   = G_hInstance;
    propPage[PAGE_IDX_FILENAME].pszTemplate = MAKEINTRESOURCE(IDD_FILENAME);
    propPage[PAGE_IDX_FILENAME].pfnDlgProc  = dlgNewVolWizard_PropSheetPageProc;

    // Offset step...
    propPage[PAGE_IDX_OFFSET].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_OFFSET].lParam      = PAGE_IDX_OFFSET;
    propPage[PAGE_IDX_OFFSET].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_OFFSET].pszTitle    = _("Offset");
    propPage[PAGE_IDX_OFFSET].hInstance   = G_hInstance;
    propPage[PAGE_IDX_OFFSET].pszTemplate = MAKEINTRESOURCE(IDD_NEWVOLWIZARD_OFFSET);
    propPage[PAGE_IDX_OFFSET].pfnDlgProc  = dlgNewVolWizard_PropSheetPageProc;

    // Size step...
    propPage[PAGE_IDX_SIZE].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_SIZE].lParam      = PAGE_IDX_SIZE;
    propPage[PAGE_IDX_SIZE].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_SIZE].pszTitle    = _("Size");
    propPage[PAGE_IDX_SIZE].hInstance   = G_hInstance;
    propPage[PAGE_IDX_SIZE].pszTemplate = MAKEINTRESOURCE(IDD_NEWVOLWIZARD_SIZE);
    propPage[PAGE_IDX_SIZE].pfnDlgProc  = dlgNewVolWizard_PropSheetPageProc;

    // Hash/cypher/IV step...
    propPage[PAGE_IDX_HASHCYPHERIV].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_HASHCYPHERIV].lParam      = PAGE_IDX_HASHCYPHERIV;
    propPage[PAGE_IDX_HASHCYPHERIV].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_HASHCYPHERIV].pszTitle    = _("Hash/cypher/IV");
    propPage[PAGE_IDX_HASHCYPHERIV].hInstance   = G_hInstance;
    propPage[PAGE_IDX_HASHCYPHERIV].pszTemplate = MAKEINTRESOURCE(IDD_NEWVOLWIZARD_HASHCYPHERIV);
    propPage[PAGE_IDX_HASHCYPHERIV].pfnDlgProc  = dlgNewVolWizard_PropSheetPageProc;

    // Master key length step...
    propPage[PAGE_IDX_MASTERKEYLEN].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_MASTERKEYLEN].lParam      = PAGE_IDX_MASTERKEYLEN;
    propPage[PAGE_IDX_MASTERKEYLEN].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_MASTERKEYLEN].pszTitle    = _("Master key length");
    propPage[PAGE_IDX_MASTERKEYLEN].hInstance   = G_hInstance;
    propPage[PAGE_IDX_MASTERKEYLEN].pszTemplate = MAKEINTRESOURCE(IDD_NEWVOLWIZARD_MASTERKEYLEN);
    propPage[PAGE_IDX_MASTERKEYLEN].pfnDlgProc  = dlgNewVolWizard_PropSheetPageProc;

    // RNG select step...
    propPage[PAGE_IDX_RNGSELECT].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_RNGSELECT].lParam      = PAGE_IDX_RNGSELECT;
    propPage[PAGE_IDX_RNGSELECT].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_RNGSELECT].pszTitle    = _("RNG select");
    propPage[PAGE_IDX_RNGSELECT].hInstance   = G_hInstance;
    propPage[PAGE_IDX_RNGSELECT].pszTemplate = MAKEINTRESOURCE(IDD_RNGSELECT);
    propPage[PAGE_IDX_RNGSELECT].pfnDlgProc  = dlgNewVolWizard_PropSheetPageProc;

    // RNG: Mouse movement step...
    propPage[PAGE_IDX_RNGMOUSEMOVEMENT].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_RNGMOUSEMOVEMENT].lParam      = PAGE_IDX_RNGMOUSEMOVEMENT;
    propPage[PAGE_IDX_RNGMOUSEMOVEMENT].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_RNGMOUSEMOVEMENT].pszTitle    = _("RNG: Mouse movement");
    propPage[PAGE_IDX_RNGMOUSEMOVEMENT].hInstance   = G_hInstance;
    propPage[PAGE_IDX_RNGMOUSEMOVEMENT].pszTemplate = MAKEINTRESOURCE(IDD_RNGMOUSEMOVEMENT);
    propPage[PAGE_IDX_RNGMOUSEMOVEMENT].pfnDlgProc  = dlgNewVolWizard_PropSheetPageProc;

    // Password step...
    propPage[PAGE_IDX_READYFORNEWVOL].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_READYFORNEWVOL].lParam      = PAGE_IDX_READYFORNEWVOL;
    propPage[PAGE_IDX_READYFORNEWVOL].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_READYFORNEWVOL].pszTitle    = _("Ready");
    propPage[PAGE_IDX_READYFORNEWVOL].hInstance   = G_hInstance;
    propPage[PAGE_IDX_READYFORNEWVOL].pszTemplate = MAKEINTRESOURCE(IDD_WIZARD_WELCOME);
    propPage[PAGE_IDX_READYFORNEWVOL].pfnDlgProc  = dlgNewVolWizard_PropSheetPageProc;

    // Password step...
    propPage[PAGE_IDX_PASSWORD].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_PASSWORD].lParam      = PAGE_IDX_PASSWORD;
    propPage[PAGE_IDX_PASSWORD].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_PASSWORD].pszTitle    = _("Password");
    propPage[PAGE_IDX_PASSWORD].hInstance   = G_hInstance;
    propPage[PAGE_IDX_PASSWORD].pszTemplate = MAKEINTRESOURCE(IDD_NEWVOLWIZARD_PASSWORD);
    propPage[PAGE_IDX_PASSWORD].pfnDlgProc  = dlgNewVolWizard_PropSheetPageProc;

    // Key iterations step...
    propPage[PAGE_IDX_KEYITERATIONS].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_KEYITERATIONS].lParam      = PAGE_IDX_KEYITERATIONS;
    propPage[PAGE_IDX_KEYITERATIONS].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_KEYITERATIONS].pszTitle    = _("Key Iterations");
    propPage[PAGE_IDX_KEYITERATIONS].hInstance   = G_hInstance;
    propPage[PAGE_IDX_KEYITERATIONS].pszTemplate = MAKEINTRESOURCE(IDD_NEWVOLWIZARD_KEYITERATIONS);
    propPage[PAGE_IDX_KEYITERATIONS].pfnDlgProc  = dlgNewVolWizard_PropSheetPageProc;

    // Salt step...
    propPage[PAGE_IDX_SALT].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_SALT].lParam      = PAGE_IDX_SALT;
    propPage[PAGE_IDX_SALT].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_SALT].pszTitle    = _("Salt");
    propPage[PAGE_IDX_SALT].hInstance   = G_hInstance;
    propPage[PAGE_IDX_SALT].pszTemplate = MAKEINTRESOURCE(IDD_NEWVOLWIZARD_SALT);
    propPage[PAGE_IDX_SALT].pfnDlgProc  = dlgNewVolWizard_PropSheetPageProc;

    // CDB location step...
    propPage[PAGE_IDX_CDBLOCATION].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_CDBLOCATION].lParam      = PAGE_IDX_CDBLOCATION;
    propPage[PAGE_IDX_CDBLOCATION].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_CDBLOCATION].pszTitle    = _("CDB location");
    propPage[PAGE_IDX_CDBLOCATION].hInstance   = G_hInstance;
    propPage[PAGE_IDX_CDBLOCATION].pszTemplate = MAKEINTRESOURCE(IDD_NEWVOLWIZARD_CDBLOCATION);
    propPage[PAGE_IDX_CDBLOCATION].pfnDlgProc  = dlgNewVolWizard_PropSheetPageProc;

    // Summary step...
    propPage[PAGE_IDX_SUMMARY].dwSize      = sizeof(propPage[1]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_SUMMARY].lParam      = PAGE_IDX_SUMMARY;
    propPage[PAGE_IDX_SUMMARY].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_SUMMARY].pszTitle    = _("Summary");
    propPage[PAGE_IDX_SUMMARY].hInstance   = G_hInstance;
    propPage[PAGE_IDX_SUMMARY].pszTemplate = MAKEINTRESOURCE(IDD_NEWVOLWIZARD_SUMMARY);
    propPage[PAGE_IDX_SUMMARY].pfnDlgProc  = dlgNewVolWizard_PropSheetPageProc;

    // Common...
    propHeader.dwSize = sizeof (PROPSHEETHEADER);
    propHeader.dwFlags     = (
                              PSH_NOAPPLYNOW | 
                              PSH_USECALLBACK |   // Needed to get WinCE
                                                  // look & feel control
                              PSH_PROPSHEETPAGE | // Use templates not handles
                              PSH_MAXIMIZE        // All WinCE dialogs are maximised
                             );
    propHeader.pszCaption  = _("New volume");
    propHeader.hwndParent  = hWnd;
    propHeader.hInstance   = G_hInstance;
    propHeader.nPages      = NEWVOLWIZARD_STEPS;
    propHeader.nStartPage  = 0;
    propHeader.ppsp        = propPage;
    propHeader.pfnCallback = dlgNewVolWizard_PropSheetProc; // Needed to get WinCE 
                                                            // look & feel control
       
    // Returns +ve value on success
    PropertySheet(&propHeader);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("DisplayDlgNewVolWizard\n")));
}                


// =========================================================================
// Returns 0 based index of current page specified
// Pass this the HWND for the *main* dialog
int _dlgNewVolWizard_GetCurrPageIdx(HWND hDlg)
{
    return _dlgNewVolWizard_GetPageIdxOfPage(PropSheet_GetCurrentPageHwnd(hDlg));
}


// =========================================================================
// Returns 0 based index of current page specified
int _dlgNewVolWizard_GetPageIdxOfPage(HWND hDlg)
{
    int retval;

    retval = GetWindowLong(hDlg, GWL_USERDATA);

    return retval;
}


// =========================================================================
// Returns 0 based index of the last page
int _dlgNewVolWizard_GetPageIdxOfLastPage()
{
    int retval;

    retval = PAGE_IDX_SUMMARY;

    return retval;
}


// =========================================================================
void _dlgNewVolWizard_SetWizardButtons(int PageIdx)
{
    UINT leftSoftkeyCmdID;

    CommandBar_ItemEnable(
                     G_dlgNewVolWizard_MenuBar,
                     ID_WIZ_BACK,
                     (PageIdx > 0)
                    );

    CommandBar_ItemEnable(
                     G_dlgNewVolWizard_MenuBar,
                     ID_WIZ_NEXT,
                     (PageIdx < _dlgNewVolWizard_GetPageIdxOfLastPage())
                    );

    CommandBar_ItemEnable(
                     G_dlgNewVolWizard_MenuBar,
                     IDOK,
                     G_dlgNewVolWizard_AllowFinish
                    );


    // Default to left softkey being "Next >"
    leftSoftkeyCmdID = ID_WIZ_NEXT;
    // If user can finish with wizard, set left softkey to "Finish"
    if (G_dlgNewVolWizard_AllowFinish)
        {
        leftSoftkeyCmdID = IDOK;
        }
    ChangeLeftSoftkey(
                      G_dlgNewVolWizard_MenuBar,
                      leftSoftkeyCmdID,
                      NULL
                     );

}


// =========================================================================
BOOL _dlgNewVolWizard_IsHidden()
{
    BOOL retval;

    retval = SDUCheckFileExists(G_dlgNewVolWizard_NewVolParams.Filename);

    return retval;
}


// =========================================================================
void _dlgNewVolWizard_WriteSummary(HWND hWnd)
{
    LARGE_INTEGER totalSize;
    LARGE_INTEGER tmpLargeInt;
    WCHAR strPrettySectorIVGenMethod[MAX_PRETTYPRINTED_SECTORIVGENMETHOD];
    WCHAR* tmpGUIDStr;
    BOOL firstInList;
    int tmpInt;

    SetDlgItemText(hWnd, IDC_EDIT_SUMMARY, TEXT(""));

    AppendStaticText(
                     hWnd, 
                     IDC_EDIT_SUMMARY, 
                     _("Your new volume will be created with the following properties:")
                    );
    AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);
    AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);

    AppendStaticText(
                     hWnd, 
                     IDC_EDIT_SUMMARY, 
                     SDUParamSubstituteW(
                             _("Filename: %1"),
                             TEXT("%s"), G_dlgNewVolWizard_NewVolParams.Filename,
                             NULL
                             )
                    );
    AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);

    if (_dlgNewVolWizard_IsHidden())
        {
        AppendStaticText(
                         hWnd, 
                         IDC_EDIT_SUMMARY, 
                         SDUParamSubstituteW(
                             _("Hidden volume starting at offset: %1"),
                             TEXT("%d"), G_dlgNewVolWizard_NewVolParams.Offset.LowPart,
                             NULL
                             )                         
                        );
        AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);
        }

    if (!(G_dlgNewVolWizard_NewVolParams.UseKeyfile))
        {
        tmpLargeInt.QuadPart = (CRITICAL_DATA_LENGTH / 8);
        totalSize.QuadPart = G_dlgNewVolWizard_NewVolParams.Size.QuadPart +
                             tmpLargeInt.QuadPart;

        AppendStaticText(
                         hWnd, 
                         IDC_EDIT_SUMMARY,                         
                         SDUParamSubstituteW(
                             _("Volume size: %1 + %2 (for CDB) = %3 bytes"),
                             TEXT("%d"), G_dlgNewVolWizard_NewVolParams.Size.LowPart,
                             TEXT("%d"), tmpLargeInt.LowPart,
                             TEXT("%d"), totalSize.LowPart,
                             NULL
                             )                         
                        );
        AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);


        AppendStaticText(
                         hWnd, 
                         IDC_EDIT_SUMMARY, 
                         _("CDB stored: At start of volume file")
                        );
        AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);
        }
    else
        {
        AppendStaticText(
                         hWnd, 
                         IDC_EDIT_SUMMARY, 
                         SDUParamSubstituteW(
                             _("Volume size: %1 bytes"),
                             TEXT("%d"), G_dlgNewVolWizard_NewVolParams.Size.LowPart,
                             NULL
                             )                         
                        );
        AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);


        AppendStaticText(
                         hWnd, 
                         IDC_EDIT_SUMMARY, 
                         _("CDB stored: In separate keyfile")
                        );
        AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);

        AppendStaticText(
                         hWnd, 
                         IDC_EDIT_SUMMARY,                         
                         SDUParamSubstituteW(
                             _("CDB keyfile: %1"),
                             TEXT("%s"), G_dlgNewVolWizard_NewVolParams.Keyfile,
                             NULL
                             )                         
                        );
        AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);
        }

    AppendStaticText(
                     hWnd, 
                     IDC_EDIT_SUMMARY, 
                     SDUParamSubstituteW(
                         _("Hash algorithm: %1"),
                         TEXT("%s"), G_dlgNewVolWizard_NewVolParams.PrettyNameHash,
                         NULL
                         )                         
                    );
    AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);
    AppendStaticText(
                     hWnd, 
                     IDC_EDIT_SUMMARY, 
                     TEXT("  ")
                    );
    AppendStaticText(
                     hWnd, 
                     IDC_EDIT_SUMMARY, 
                     SDUParamSubstituteW(
                         _("[Hash driver: %1]"),
                         TEXT("%s"), G_dlgNewVolWizard_NewVolParams.DriverHash,
                         NULL
                         )                         
                    );
    AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);
    GUIDToWCHAR(
                &G_dlgNewVolWizard_NewVolParams.ImplGUIDHash,
                &tmpGUIDStr
               );
    AppendStaticText(
                     hWnd, 
                     IDC_EDIT_SUMMARY, 
                     TEXT("  ")
                    );
    AppendStaticText(
                     hWnd, 
                     IDC_EDIT_SUMMARY, 
                     SDUParamSubstituteW(
                         _("[Hash GUID: %1]"),
                         TEXT("%s"), tmpGUIDStr,
                         NULL
                         )                         
                    );
    SecZeroAndFreeMemory(
                         tmpGUIDStr,
                         (wcslen(tmpGUIDStr) * sizeof(*tmpGUIDStr))
                        );
    AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);

    AppendStaticText(
                     hWnd, 
                     IDC_EDIT_SUMMARY, 
                     SDUParamSubstituteW(
                         _("Key iterations: %1"),
                         TEXT("%d"), G_dlgNewVolWizard_NewVolParams.KeyIterations,
                         NULL
                         )                         
                    );
    AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);

    if (G_dlgNewVolWizard_NewVolParams.BlockSizeSelectedCypher <= 0)
        {
        AppendStaticText(
                         hWnd, 
                         IDC_EDIT_SUMMARY, 
                         _("Sector IVs will not be used (cypher has zero or variable blocksize)")
                        );
        AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);
        }
    else
        {
        AppendStaticText(
                         hWnd, 
                         IDC_EDIT_SUMMARY, 
                         _("Cypher has fixed, defined blocksize; sector IVs will be used")
                        );
        AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);
        driver_PrettyprintSectorIVGenMethod(
                            G_dlgNewVolWizard_NewVolParams.SectorIVGenMethod,
                            strPrettySectorIVGenMethod,
                            sizeof(strPrettySectorIVGenMethod)
                           );
        AppendStaticText(
                         hWnd, 
                         IDC_EDIT_SUMMARY, 
                         SDUParamSubstituteW(
                             _("Sector IV generation method: %1"),
                             TEXT("%s"), strPrettySectorIVGenMethod,
                             NULL
                             )                         
                        );
        AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);

        if (G_dlgNewVolWizard_NewVolParams.XORIV)
            {
            AppendStaticText(
                             hWnd, 
                             IDC_EDIT_SUMMARY, 
                             _("Sector IVs will be XORd with a per-volume IV before use")
                            );
            AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);
            }
        else
            {
            AppendStaticText(
                             hWnd, 
                             IDC_EDIT_SUMMARY, 
                             _("A per-volume IV will not be used")
                            );
            AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);
            }
        }


    AppendStaticText(
                     hWnd, 
                     IDC_EDIT_SUMMARY, 
                     SDUParamSubstituteW(
                         _("Cypher algorithm: %1"),
                         TEXT("%s"), G_dlgNewVolWizard_NewVolParams.PrettyNameCypher,
                         NULL
                         )                         
                    );
    AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);
    AppendStaticText(
                     hWnd, 
                     IDC_EDIT_SUMMARY, 
                     TEXT("  ")
                    );
    AppendStaticText(
                     hWnd, 
                     IDC_EDIT_SUMMARY, 
                     SDUParamSubstituteW(
                         _("[Cypher driver: %1]"),
                         TEXT("%s"), G_dlgNewVolWizard_NewVolParams.DriverCypher,
                         NULL
                         )                         
                    );
    AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);
    GUIDToWCHAR(
                &G_dlgNewVolWizard_NewVolParams.ImplGUIDHash,
                &tmpGUIDStr
               );
    AppendStaticText(
                     hWnd, 
                     IDC_EDIT_SUMMARY, 
                     TEXT("  ")
                    );
    AppendStaticText(
                     hWnd, 
                     IDC_EDIT_SUMMARY, 
                     SDUParamSubstituteW(
                         _("[Cypher GUID: %1]"),
                         TEXT("%s"), tmpGUIDStr,
                         NULL
                         )                         
                    );
    SecZeroAndFreeMemory(
                         tmpGUIDStr,
                         (wcslen(tmpGUIDStr) * sizeof(*tmpGUIDStr))
                        );
    AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);

    tmpInt = G_dlgNewVolWizard_NewVolParams.KeySizeUser;
    if (G_dlgNewVolWizard_NewVolParams.KeySizeSelectedCypher >= 0)
        {
        tmpInt = G_dlgNewVolWizard_NewVolParams.KeySizeSelectedCypher;
        }
    AppendStaticText(
                     hWnd, 
                     IDC_EDIT_SUMMARY, 
                     SDUParamSubstituteW(
                         _("Master key length: %1"),
                         TEXT("%d"), tmpInt,
                         NULL
                         )                         
                    );
    AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);

    AppendStaticText(
                     hWnd, 
                     IDC_EDIT_SUMMARY, 
                     _("RNG: ")
                    );
    firstInList = TRUE;
    if (G_dlgNewVolWizard_NewVolParams.SelectedRNG.UseMSCryptoAPI)
        {
        if (!(firstInList))
            {
            AppendStaticText(
                             hWnd, 
                             IDC_EDIT_SUMMARY, 
                             TEXT(", ")
                            );
            }
        AppendStaticText(
                         hWnd, 
                         IDC_EDIT_SUMMARY, 
                         _("Microsoft CryptoAPI")
                        );
        firstInList = FALSE;
        }
    if (G_dlgNewVolWizard_NewVolParams.SelectedRNG.UseUserInput)
        {
        if (!(firstInList))
            {
            AppendStaticText(
                             hWnd, 
                             IDC_EDIT_SUMMARY, 
                             TEXT(", ")
                            );
            }
        AppendStaticText(
                         hWnd, 
                         IDC_EDIT_SUMMARY, 
                         _("User input")
                        );
        firstInList = FALSE;
        }
    AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);

    AppendStaticText(
                     hWnd, 
                     IDC_EDIT_SUMMARY, 
                     _("Password: <entered>")
                    );
    AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);

    AppendStaticText(
                     hWnd, 
                     IDC_EDIT_SUMMARY, 
                     SDUParamSubstituteW(
                         _("Salt length: %1 bits"),
                         TEXT("%d"), G_dlgNewVolWizard_NewVolParams.SaltLength,
                         NULL
                         )                         
                    );
    AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);


    AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);
    AppendStaticText(
                     hWnd, 
                     IDC_EDIT_SUMMARY, 
                     _("Select \"Finish\" to create the above volume")
                    );
    AppendStaticText(hWnd, IDC_EDIT_SUMMARY, EDIT_CRLF);

    SecZeroMemory(&totalSize, sizeof(totalSize));
    SecZeroMemory(&tmpLargeInt, sizeof(tmpLargeInt));
    SecZeroMemory(
                  strPrettySectorIVGenMethod, 
                  sizeof(strPrettySectorIVGenMethod)
                 );
}


// =========================================================================
BOOL CALLBACK dlgNewVolWizard_HandleMsg_WM_NOTIFY(HWND hDlg, WPARAM wParam, LPARAM lParam)
{
    BOOL retval = FALSE;
    int idCtrl;
    NMHDR* notifHdr;
    NMUPDOWN* lpnmud;
    NMUSERRNG_BYTEGEN* lpnmRNG;
    int currPage;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgNewVolWizard_HandleMsg_WM_NOTIFY\n")));

    idCtrl = (int)wParam;
    notifHdr = (NMHDR*)lParam;

    switch (notifHdr->code)
        {
        case URNG_BYTEGEN:
            {
            if (idCtrl == IDCTRL_USERRNG)
                {
                lpnmRNG = (NMUSERRNG_BYTEGEN*)lParam;

                if ((G_dlgNewVolWizard_RandomUserInput_Filled / 8) < sizeof(G_dlgNewVolWizard_RandomUserInput_Data))
                    {
                    // Store random byte...
                    G_dlgNewVolWizard_RandomUserInput_Data[G_dlgNewVolWizard_RandomUserInput_Filled / 8] = lpnmRNG->Byte;
                    G_dlgNewVolWizard_RandomUserInput_Filled += 8;

                    // Update display of bits generated...
                    SetDlgItemText_WithResize(
                                     hDlg, 
                                     IDC_STATIC_USERRNG_GENERATED_REQUIRED, 
                                     SDUParamSubstituteW(
                                             _("Random bits generated: %1/%2"),
                                             TEXT("%d"), G_dlgNewVolWizard_RandomUserInput_Filled,
                                             TEXT("%d"), (sizeof(G_dlgNewVolWizard_RandomUserInput_Data) * 8),
                                             NULL
                                             )
                                    );

                    // Disable control if the userRNG pool is full...
                    if ((G_dlgNewVolWizard_RandomUserInput_Filled / 8) >= sizeof(G_dlgNewVolWizard_RandomUserInput_Data))
                        {
                        SetControlEnabled(hDlg, IDCTRL_USERRNG, FALSE);
                        }

                    }

                break;
                }

            }

        // Spin edit up/down button
        case UDN_DELTAPOS:
            {
            switch(idCtrl)
                {
                case IDC_SPIN_OFFSET:
                    {
                    lpnmud = (NMUPDOWN*)lParam;
                    AdjustSpinControl(
                                      hDlg, 
                                      IDC_EDIT_OFFSET, 
                                      0, 
                                      INT_MAX, 
                                      -(lpnmud->iDelta)
                                     );
                    retval = TRUE;
                    break;
                    }

                case IDC_SPIN_SALTLENGTH:
                    {
                    lpnmud = (NMUPDOWN*)lParam;
                    AdjustSpinControl(
                                      hDlg, 
                                      IDC_EDIT_SALTLENGTH, 
                                      0, 
                                      INT_MAX, 
                                      (-(lpnmud->iDelta) * DELTA_SALT_LENGTH)
                                     );
                    retval = TRUE;
                    break;
                    }

                case IDC_SPIN_KEYITERATIONS:
                    {
                    lpnmud = (NMUPDOWN*)lParam;
                    AdjustSpinControl(
                                      hDlg, 
                                      IDC_EDIT_KEYITERATIONS, 
                                      MIN_KEYITERATIONS, 
                                      INT_MAX, 
                                      (-(lpnmud->iDelta) * DELTA_KEY_ITERATIONS)
                                     );
                    retval = TRUE;
                    break;
                    }

                case IDC_SPIN_SIZE:
                    {
                    lpnmud = (NMUPDOWN*)lParam;
                    AdjustSpinControl(
                                      hDlg, 
                                      IDC_EDIT_SIZE, 
                                      MIN_VOLUME_SIZE, 
                                      INT_MAX, 
                                      -(lpnmud->iDelta)
                                     );
                    retval = TRUE;
                    break;
                    }

                case IDC_SPIN_MASTERKEYLEN:
                    {
                    lpnmud = (NMUPDOWN*)lParam;
                    AdjustSpinControl(
                                      hDlg, 
                                      IDC_EDIT_MASTERKEYLEN, 
                                      0, 
                                      INT_MAX, 
                                      (-(lpnmud->iDelta) * DELTA_CYPHERKEYLENGTH)
                                     );
                    retval = TRUE;
                    break;
                    }

                }
            break;
            }

        case PSN_SETACTIVE:
            {
            currPage = _dlgNewVolWizard_GetPageIdxOfPage(hDlg);
            if (currPage == PAGE_IDX_SUMMARY)
                {
                _dlgNewVolWizard_WriteSummary(hDlg);
                }
            else if (currPage == PAGE_IDX_READYFORNEWVOL)
                {
                // If the user got to this page, we have the minimum set of
                // information we mandate that the user enters
                // Allow "Finish" button
                G_dlgNewVolWizard_AllowFinish = TRUE;
                }

            _dlgNewVolWizard_SetWizardButtons(currPage);

            break; 
            }

        case PSN_KILLACTIVE:
            {
            // (Validate if necessary here)
            // Set the DWL_MSGRESULT to PSNRET_NOERROR to receive PSN_APPLY
            SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_NOERROR);
            break; 
            }

        case PSN_APPLY:
            {
            // This notification is received when the user clicks
            // "OK"/"Apply"
            // This is called once for each of the tabs.
            // When called, we validate the tab it is called for (this
            // has the effect that the first invalid tab will be automatically
            // selected)
            // If the tab being called for is the last one (the summary),
            // we create the new volume.
            currPage = _dlgNewVolWizard_GetPageIdxOfPage(hDlg);
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("PSN_APPLY for page idx: %d\n"), currPage));

            SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_NOERROR);
            if (dlgNewVolWizard_ValidatePage(hDlg))
                {
                // If it's the last page, only succeed if the volume can be
                // created
                if (currPage != PAGE_IDX_SUMMARY)
                    {
                    SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_NOERROR);
                    }
                else
                    {
                    if (dlgNewVolWizard_CreateNew(hDlg))
                        {
                        if (G_dlgNewVolWizard_Mountpoint != NULL)
                            {
                            MsgInfo(hDlg, _("New volume created and mounted successfully."));

                            // Don't need it anymore...
                            SecZeroAndFreeWCHARMemory(G_dlgNewVolWizard_Mountpoint);
                            G_dlgNewVolWizard_Mountpoint = NULL;
                            }
                        else
                            {
                            MsgInfo(hDlg, _("New volume created successfully.\n\nPlease tap \"File | Mount\" to begin using it"));
                            }
                        SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_NOERROR);
                        }
                    else
                        {
                        MsgError(hDlg, _("New volume could not be created; please check your settings, and ensure you have sufficient storage to hold the new volume"));
                        SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_INVALID);
                        }
                    }
                }
            else
                {
                SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_INVALID);
                }

            break; 
            }

        case PSN_RESET:
            {
            // This notification is received when the user clicks
            // "Close"/"Cancel"
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Dialog cancelled by user\n")));
            SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_NOERROR);
            break; 
            }

        }


    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgNewVolWizard_HandleMsg_WM_NOTIFY\n")));
    return TRUE;
}


// =========================================================================
// This function is required in order to get the WinCE look & feel to the
// tabbed dialog.
int CALLBACK dlgNewVolWizard_PropSheetProc(
  HWND hwndDlg, 
  UINT uMsg, 
  LPARAM lParam 
)
{
    int retval = 0;
    switch (uMsg)
        {
        case PSCB_GETVERSION:
            {
            // We do *NOT* set the version here. The resize causes the page
            // to be displayed *over* the property sheet's tabs. Setting
            // this version prevents this from working correctly
            // retval = COMCTL32_VERSION;
            break; 
            }

        }

    return retval;
}


// =========================================================================
// The first time the user RNG is setup, it must use the designtime 
// positioned placeholder. Subsequently, it can just resize it's width and
// depth itsself (having already got it's top border from the placeholder)
void _dlgNewVolWizard_ResizeUserRNG(
    HWND hDlg,
    BOOL usePlaceholder
)
{
    HWND hWndTmp;

    // We resize the userRNG such that it's as wide and deep as we
    // can make it, but with the top as being where the top of the
    // placeholder text was; this makes it easier to position on the
    // dialog at design time
    if (usePlaceholder)
        {
        SizeControlMaxWidth(hDlg, IDC_STATIC_USERRNGPLACEHOLDER);
        SizeControlMaxDepth(hDlg, IDC_STATIC_USERRNGPLACEHOLDER);

        hWndTmp = GetDlgItem(hDlg, IDC_STATIC_USERRNGPLACEHOLDER);
        SizeWindowToOverlapWindow(hWndTmp, G_dlgNewVolWizard_UserRNG);

        SetControlVisible(hDlg, IDC_STATIC_USERRNGPLACEHOLDER, FALSE);
        }
    else
        {
        SizeControlMaxWidth(hDlg, IDCTRL_USERRNG);
        SizeControlMaxDepth(hDlg, IDCTRL_USERRNG);
        }
}


// =========================================================================
BOOL CALLBACK dlgNewVolWizard_HandleMsg_WM_INITDIALOG(HWND hDlg, LPARAM lParam)
{
    SHINITDLGINFO shidi;
    int i;
    UINT menuID;
    HCURSOR csrHourglass;
    HCURSOR csrPrevious;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgNewVolWizard_HandleMsg_WM_INITDIALOG\n")));

    csrHourglass = LoadCursor(NULL, IDC_WAIT);
    csrPrevious = SetCursor(csrHourglass);

    // Setup globals...

    G_dlgNewVolWizard_Mountpoint = NULL;

    G_dlgNewVolWizard_RandomUserInput_Filled = 0;
    memset(
        G_dlgNewVolWizard_RandomUserInput_Data, 
        0, 
        sizeof(G_dlgNewVolWizard_RandomUserInput_Data)
       );

    // Don't recreate menu if it's already been done
    if (G_dlgNewVolWizard_MenuBar == NULL)
        {
        menuID = IDR_MENU_WIZARD;
        // If softkeys are supported, we use a menu with separators
        if (G_Options->SoftkeyMenus == TRUE)
            {
            menuID = IDR_MENU_WIZARD_SOFTKEY;
            }

        G_dlgNewVolWizard_MenuBar = SetupMenu(
                                              hDlg,
                                              menuID,
                                              ID_WIZ_NEXT,
                                              NULL
                                             );
        }

    SDUi18n_TranslateWindow(hDlg);
    SDUi18n_TranslateCommandBar(G_dlgNewVolWizard_MenuBar);

    for (i = 0; i < NEWVOLWIZARD_STEPS; i++)
        {
        G_dlgNewVolWizard_PageCompleted[i] = FALSE;
        }
    memset(
           &G_dlgNewVolWizard_NewVolParams, 
           0,
           sizeof(G_dlgNewVolWizard_NewVolParams)
          );


//xxx - get rid of this?
    // This isn't actually needed for property sheet dialog...
    shidi.dwMask = SHIDIM_FLAGS;
    shidi.dwFlags = (
                     SHIDIF_DONEBUTTON | 
                     SHIDIF_SIPDOWN | 
                     SHIDIF_SIZEDLGFULLSCREEN
                    );
    shidi.hDlg = hDlg;
    SHInitDialog(&shidi);

    // Set userdata on the page being initialized; we use this to identify
    // the different pages
    // Note: You will get a "Data Abort" error here if lParam is dereferenced    
    //       here and it's NULL
    SetWindowLong(
                  hDlg,
                  GWL_USERDATA,
                  ((LPPROPSHEETPAGE)lParam)->lParam
                 );

    // Default values for controls...
    SetDlgItemText(hDlg, IDC_EDIT_FILENAME, DEFAULT_FILENAME);
    SetDlgItemText(hDlg, IDC_EDIT_KEYFILE, DEFAULT_KEYFILE);
    SetDlgItemText(hDlg, IDC_EDIT_PASSWORD, DEFAULT_PASSWORD);
    SetDlgItemText(hDlg, IDC_EDIT_PASSWORDCONFIRM, DEFAULT_PASSWORD);
    SetDlgItemInt(hDlg, IDC_EDIT_OFFSET, DEFAULT_OFFSET, TRUE);

    // CDB location radiobutton...
    if (DEFAULT_CDBSTOREDINVOLUME == TRUE)
        {
        CheckDlgButton(
                       hDlg,    
                       IDC_RADIO_CDBINVOLUME,
                       BST_CHECKED
                      );
        }
    else
        {
        CheckDlgButton(
                       hDlg, 
                       IDC_RADIO_CDBINKEYFILE,
                       BST_CHECKED
                      );
        }

    // Use volume IV checkbox...
    if (DEFAULT_VOLUMEIV == TRUE)
        {
        CheckDlgButton(
                       hDlg,    
                       IDC_CHECK_VOLUMEIV,
                       BST_CHECKED
                      );
        }
    else
        {
        CheckDlgButton(
                       hDlg, 
                       IDC_CHECK_VOLUMEIV,
                       BST_UNCHECKED
                      );
        }

    // RNG location checkbox...
    if (DEFAULT_RND_USERINPUT)
        {
        CheckDlgButton(
                       hDlg, 
                       IDC_CHECK_RNGUSERINPUT,
                       BST_CHECKED
                      );
        }
    if (DEFAULT_RNG_MSCRYPTOAPI)
        {
        CheckDlgButton(
                       hDlg, 
                       IDC_CHECK_RNGMSCRYPTOAPI,
                       BST_CHECKED
                      );
        }

    // Automount after volume creation...
    if (DEFAULT_AUTOMOUNT_AFTER_CREATE == TRUE)
        {
        CheckDlgButton(
                       hDlg,    
                       IDC_CHECK_AUTOMOUNTAFTERCREATE,
                       BST_CHECKED
                      );
        }
    else
        {
        CheckDlgButton(
                       hDlg, 
                       IDC_CHECK_AUTOMOUNTAFTERCREATE,
                       BST_UNCHECKED
                      );
        }

    SetDlgItemInt(hDlg, IDC_EDIT_SALTLENGTH, DEFAULT_SALTLENGTH, TRUE);
    SetDlgItemInt(hDlg, IDC_EDIT_KEYITERATIONS, DEFAULT_KEYITERATIONS, TRUE);
    SetDlgItemInt(hDlg, IDC_EDIT_SIZE, DEFAULT_VOLUME_SIZE, TRUE);
    SetDlgItemInt(hDlg, IDC_EDIT_MASTERKEYLEN, DEFAULT_CYPHERKEYLENGTH, TRUE);

    _dlgNewVolWizard_PopulateComboboxes(hDlg);

    if (_dlgNewVolWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_WELCOME)
        {
        SDUTextSetBold(hDlg, IDC_STATIC_WELCOME_TITLE, TRUE);

        SetDlgItemString_WithResize(
                         G_hInstance,
                         hDlg,
                         IDC_STATIC_WELCOME_TITLE,
                         IDS_STRING_WIZTITLE_NEWVOL
                        );
        SetDlgItemString_WithResize(
                         G_hInstance,
                         hDlg,
                         IDC_STATIC_WELCOME_DESC,
                         IDS_STRING_WIZDESC_NEWVOL
                        );
        }
    else if (_dlgNewVolWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_FILENAME)
        {
        SetupInstructions(
                         G_hInstance,
                         hDlg,
                         IDC_STATIC_INSTRUCTION,
                         IDS_STRING_INSTR_FILENAME
                        );
        }
    else if (_dlgNewVolWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_OFFSET)
        {
        // Make warning more prominent...
        SizeControlMaxWidthBorder(
                                  hDlg,
                                  IDC_STATIC_WARNING_BLOCK_1,
                                  FREEOTFE_DLG_BORDER
                                 );
        SDUTextSetBold(hDlg, IDC_STATIC_WARNING_BLOCK_1, TRUE);
        SDUTextSetUnderline(hDlg, IDC_STATIC_WARNING_BLOCK_1, TRUE);
        SizeControlMaxWidthBorder(
                                  hDlg,
                                  IDC_STATIC_WARNING_BLOCK_2,
                                  FREEOTFE_DLG_BORDER
                                 );
        SDUTextSetBold(hDlg, IDC_STATIC_WARNING_BLOCK_2, TRUE);

        SetupInstructions(
                         G_hInstance,
                         hDlg,
                         IDC_STATIC_INSTRUCTION,
                         IDS_STRING_INSTR_OFFSET
                        );
        }
    else if (_dlgNewVolWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_SIZE)
        {
        SetupInstructions(
                         G_hInstance,
                         hDlg,
                         IDC_STATIC_INSTRUCTION,
                         IDS_STRING_INSTR_SIZE
                        );
        }
    else if (_dlgNewVolWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_HASHCYPHERIV)
        {
        SetupInstructions(
                         G_hInstance,
                         hDlg,
                         IDC_STATIC_INSTRUCTION,
                         IDS_STRING_INSTR_HASHCYPHERIV
                        );
        _dlgNewVolWizard_EnableDisableControls_HashCypherIV(hDlg);
        }
    else if (_dlgNewVolWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_MASTERKEYLEN)
        {
        SetupInstructions(
                         G_hInstance,
                         hDlg,
                         IDC_STATIC_INSTRUCTION,
                         IDS_STRING_INSTR_MASTERKEYLEN
                        );
        }
    else if (_dlgNewVolWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_RNGSELECT)
        {
        SetupInstructions(
                         G_hInstance,
                         hDlg,
                         IDC_STATIC_INSTRUCTION,
                         IDS_STRING_INSTR_RNGSELECT
                        );
        }
    else if (_dlgNewVolWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_RNGMOUSEMOVEMENT)
        {
        G_dlgNewVolWizard_UserRNG = CreateWindow(
                              USERRNG_CONTROL,      // Class name
                              USERRNG_CONTROL,      // Window title       
                              (WS_VISIBLE | WS_BORDER),     // Style
                              CW_USEDEFAULT,  // X
                              CW_USEDEFAULT,  // Y
                              CW_USEDEFAULT,  // Width
                              CW_USEDEFAULT,  // Height
                              hDlg,           // Parent window
                              (HMENU)IDCTRL_USERRNG, // Menu/CtrlID
                              G_hInstance, 
                              NULL
                             );

        if (G_dlgNewVolWizard_UserRNG == NULL)      
            {
            MsgError(hDlg, _("Unable to create user RNG"));
            }
        else
            {
            _dlgNewVolWizard_ResizeUserRNG(hDlg, TRUE);
            }

        SetDlgItemText_WithResize(
                         hDlg, 
                         IDC_STATIC_USERRNG_GENERATED_REQUIRED, 
                         SDUParamSubstituteW(
                                 _("Random bits generated: %1/%2"),
                                 TEXT("%d"), G_dlgNewVolWizard_RandomUserInput_Filled,
                                 TEXT("%d"), (sizeof(G_dlgNewVolWizard_RandomUserInput_Data) * 8),
                                 NULL
                                 )
                        );

        SetupInstructions(
                         G_hInstance,
                         hDlg,
                         IDC_STATIC_INSTRUCTION,
                         IDS_STRING_INSTR_RNGMOUSEMOVEMENT
                        );
        }
    else if (_dlgNewVolWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_PASSWORD)
        {
        SetupInstructions(
                         G_hInstance,
                         hDlg,
                         IDC_STATIC_INSTRUCTION,
                         IDS_STRING_INSTR_PASSWORD
                        );
        }
    else if (_dlgNewVolWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_READYFORNEWVOL)
        {
        SDUTextSetBold(hDlg, IDC_STATIC_WELCOME_TITLE, TRUE);

        SetDlgItemString_WithResize(
                         G_hInstance,
                         hDlg,
                         IDC_STATIC_WELCOME_TITLE,
                         IDS_STRING_INSTR_READYFORNEWVOL_TITLE
                        );
        SetDlgItemString_WithResize(
                         G_hInstance,
                         hDlg,
                         IDC_STATIC_WELCOME_DESC,
                         IDS_STRING_INSTR_READYFORNEWVOL_DESC
                        );

        // Ditch the "Please press "Next >"" prompt
        SetControlVisible(hDlg, IDC_STATIC_WELCOME_NEXT, FALSE);
        
        // Resize the description/instructions so they'll fit in heightwise
        SizeControlMaxDepth(hDlg, IDC_STATIC_WELCOME_DESC);
        }
    else if (_dlgNewVolWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_KEYITERATIONS)
        {
        SetupInstructions(
                         G_hInstance,
                         hDlg,
                         IDC_STATIC_INSTRUCTION,
                         IDS_STRING_INSTR_KEYITERATIONS
                        );
        }
    else if (_dlgNewVolWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_SALT)
        {
        SetupInstructions(
                         G_hInstance,
                         hDlg,
                         IDC_STATIC_INSTRUCTION,
                         IDS_STRING_INSTR_SALT
                        );
        }
    else if (_dlgNewVolWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_CDBLOCATION)
        {
        SetupInstructions(
                         G_hInstance,
                         hDlg,
                         IDC_STATIC_INSTRUCTION,
                         IDS_STRING_INSTR_CDBLOCATION
                        );
        _dlgNewVolWizard_EnableDisableControls_CDBLocation(hDlg);
        }

    // Set menu buttons as on the first page
    _dlgNewVolWizard_SetWizardButtons(0);

    SetCursor(csrPrevious);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgNewVolWizard_HandleMsg_WM_INITDIALOG\n")));
    return TRUE;
}


// =========================================================================
void _dlgNewVolWizard_PopulateComboboxes(HWND hDlg)
{
    int i;
    SECTOR_IV_GEN_METHOD currSectIVGenMethod;
    WCHAR strPrettySectorIVGenMethod[MAX_PRETTYPRINTED_SECTORIVGENMETHOD];
    int implIdx;
    int cbItemIdx;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_dlgNewVolWizard_PopulateComboboxes\n")));


    if (!(G_dlgNewVolWizard_GotImplDetails))
        {
        G_dlgNewVolWizard_GotImplDetails =
                       driver_GetAllAlgorithmDriverOptions(
                                                &G_dlgNewVolWizard_CountImplHash,
                                                &G_dlgNewVolWizard_ImplHash,

                                                &G_dlgNewVolWizard_CountImplCypher,
                                                &G_dlgNewVolWizard_ImplCypher,

                                                TRUE
                                               );
        }

    if (G_dlgNewVolWizard_GotImplDetails)
        {
        // Add hashes to combobox...
        for (
             implIdx = 0; 
             implIdx < G_dlgNewVolWizard_CountImplHash; 
             implIdx++
            )
            {
            // Strip out all hashes which have:
            //   length <= 0 or blocksize <= 0
            //  - they cannot be used to create new FreeOTFE volumes as they use
            // PBKDF2 (HMAC) to derive the critical data key; and PBKDF2 with HMAC
            // requires that the hash used has a defined length of greater than zero
            if (
                (G_dlgNewVolWizard_ImplHash[implIdx].Hash.Length > 0) &&
                (G_dlgNewVolWizard_ImplHash[implIdx].Hash.BlockSize > 0)
               )
                {
                cbItemIdx = Combobox_AddItem(
                                        hDlg,
                                        IDC_COMBO_HASH,
                                        G_dlgNewVolWizard_ImplHash[implIdx].PrettyName,
                                        (&G_dlgNewVolWizard_ImplHash[implIdx])
                                       );
                }
            }
        // Set default (if possible)
        SetComboBoxSelected(hDlg, IDC_COMBO_HASH, DEFAULT_HASH);


        // Add cyphers to combobox...
        for (
             implIdx = 0;
             implIdx < G_dlgNewVolWizard_CountImplCypher;
             implIdx++
            )
            {
            cbItemIdx = Combobox_AddItem(
                                    hDlg,
                                    IDC_COMBO_CYPHER,
                                    G_dlgNewVolWizard_ImplCypher[implIdx].PrettyName,
                                    (&G_dlgNewVolWizard_ImplCypher[implIdx])
                                   );
            }
        // Set default (if possible)
        SetComboBoxSelected(hDlg, IDC_COMBO_CYPHER, DEFAULT_CYPHER);
        }


    // Sector IV generation methods...
    for (
         i = 0;
         i < (sizeof(ARR_SECTOR_IV_GEN_METHOD) / sizeof(ARR_SECTOR_IV_GEN_METHOD[0]));
         i++
        )
        {
        currSectIVGenMethod = ARR_SECTOR_IV_GEN_METHOD[i]; 

        // Skip the "<Unknown>" method!
        if (currSectIVGenMethod != SCTRIVGEN_UNKNOWN)
            {
            driver_PrettyprintSectorIVGenMethod(
                                currSectIVGenMethod,
                                strPrettySectorIVGenMethod,
                                sizeof(strPrettySectorIVGenMethod)
                               );

            cbItemIdx = Combobox_AddItem(
                                    hDlg,
                                    IDC_COMBO_SECTORIV,
                                    strPrettySectorIVGenMethod,
                                    (void*)currSectIVGenMethod                                    
                                   );
            }
        }
    // Set default (if possible)
    SetComboBoxSelected(hDlg, IDC_COMBO_SECTORIV, (const WCHAR*)DEFAULT_SECTORIVGEN);


    SecZeroMemory(
                  strPrettySectorIVGenMethod, 
                  sizeof(strPrettySectorIVGenMethod)
                 );

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_dlgNewVolWizard_PopulateComboboxes\n")));
}


// =========================================================================
BOOL CALLBACK dlgNewVolWizard_HandleMsg_WM_DESTROY(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgNewVolWizard_HandleMsg_WM_DESTROY\n")));

    if (G_dlgNewVolWizard_MenuBar != NULL)
        {
        DestroyWindow(G_dlgNewVolWizard_MenuBar);
        G_dlgNewVolWizard_MenuBar = NULL;
        }

    if (G_dlgNewVolWizard_UserRNG != NULL)
        {
        DestroyWindow(G_dlgNewVolWizard_UserRNG);
        G_dlgNewVolWizard_UserRNG = NULL;
        }

    if (G_dlgNewVolWizard_GotImplDetails)
        {
        driver_FreeAllAlgorithmDriverOptions(
                                            &G_dlgNewVolWizard_CountImplHash,
                                            &G_dlgNewVolWizard_ImplHash,

                                            &G_dlgNewVolWizard_CountImplCypher,
                                            &G_dlgNewVolWizard_ImplCypher
                                           );

        G_dlgNewVolWizard_GotImplDetails = FALSE;
        }

    G_dlgNewVolWizard_GotImplDetails = FALSE;
    G_dlgNewVolWizard_CountImplCypher = 0;
    G_dlgNewVolWizard_ImplCypher = NULL;  // Array of options
    G_dlgNewVolWizard_CountImplHash = 0;
    G_dlgNewVolWizard_ImplHash = NULL;  // Array of options

    G_dlgNewVolWizard_RandomUserInput_Filled = 0;
    SecZeroMemory(
        G_dlgNewVolWizard_RandomUserInput_Data, 
        sizeof(G_dlgNewVolWizard_RandomUserInput_Data)
       );

    G_dlgNewVolWizard_AllowFinish = FALSE;

    SDUi18n_FreeOriginalStrings(hWnd);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgNewVolWizard_HandleMsg_WM_DESTROY\n")));
    return TRUE;
}


// =========================================================================
// Resize the property sheet page so that the tabs at the top are
// hidden by it; we don't want them shown for our wizard
BOOL CALLBACK dlgNewVolWizard_HandleMsg_WM_SIZE(HWND hDlg)
{
    RECT rcParent;
#if DBG
    // Nothing; tab header displayed under debug
#else
    RECT rcClient;
#endif
    static BOOL resizing = FALSE;
    HWND hWndTmp;
    int chkBoxHeight;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgNewVolWizard_HandleMsg_WM_SIZE\n")));

    // Prevent infinite recursion as we resize the dialog...
    if (!(resizing))
        {
        resizing = TRUE;

#if DBG
        // Nothing; tab header displayed under debug
#else
        // Get size of display area...
        GetClientRect(GetParent(hDlg), &rcClient);

        // Resize...
        SetWindowPos(
                     hDlg, 
                     NULL, 
                     0, 
                     0, 
                     rcClient.right,
                     rcClient.bottom,
                     SWP_NOZORDER
                    );
#endif

        // Resize specific property sheets...
        GetClientRect(hDlg, &rcParent);

        switch (_dlgNewVolWizard_GetPageIdxOfPage(hDlg))
            {
            case PAGE_IDX_WELCOME:
                {
                SizeControlMaxWidthBorder(
                                          hDlg, 
                                          IDC_STATIC_WELCOME_TITLE, 
                                          FREEOTFE_DLG_BORDER
                                         );
                SizeControlMaxWidthBorder(
                                          hDlg, 
                                          IDC_STATIC_WELCOME_DESC, 
                                          FREEOTFE_DLG_BORDER
                                         );
                SizeControlMaxWidthBorder(
                                          hDlg, 
                                          IDC_STATIC_WELCOME_NEXT, 
                                          FREEOTFE_DLG_BORDER
                                         );

                break;
                }

            case PAGE_IDX_RNGMOUSEMOVEMENT:
                {
                _dlgNewVolWizard_ResizeUserRNG(hDlg, FALSE);
                break;
                }

            case PAGE_IDX_READYFORNEWVOL:
                {
                SizeControlMaxWidthBorder(
                                          hDlg, 
                                          IDC_STATIC_WELCOME_TITLE, 
                                          FREEOTFE_DLG_BORDER
                                         );
                SizeControlMaxWidthBorder(
                                          hDlg, 
                                          IDC_STATIC_WELCOME_DESC, 
                                          FREEOTFE_DLG_BORDER
                                         );

                break;
                }

            case PAGE_IDX_SUMMARY:
                {
                hWndTmp = GetDlgItem(hDlg, IDC_CHECK_AUTOMOUNTAFTERCREATE); 
                chkBoxHeight = 0;
                if (hWndTmp != NULL)
                    {
                    GetClientRect(hWndTmp, &rcClient);
                    chkBoxHeight = (rcClient.bottom - rcClient.top);
                    SetWindowPos(
                                 hWndTmp, 
                                 NULL, 
                                 FREEOTFE_DLG_BORDER, 
                                 (rcParent.bottom - chkBoxHeight - FREEOTFE_DLG_BORDER),
                                 (rcClient.right - rcClient.left),
                                 chkBoxHeight, 
                                 SWP_NOZORDER
                                );

                    CenterWindowToWindowClient(hDlg, hWndTmp);
                    }
                
                hWndTmp = GetDlgItem(hDlg, IDC_EDIT_SUMMARY); 
                if (hWndTmp != NULL)
                    {
                    SetWindowPos(
                                 hWndTmp, 
                                 NULL, 
                                 0, 
                                 0, 
                                 rcParent.right,
                                 (rcParent.bottom - chkBoxHeight - (2 * FREEOTFE_DLG_BORDER)), 
                                 SWP_NOZORDER
                                );
                    }

                break;
                }
            }

        resizing = FALSE;
        }


    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgNewVolWizard_HandleMsg_WM_SIZE\n")));
    return TRUE;
}


// =========================================================================
void _dlgNewVolWizard_SecZeroParams()
{
    if (G_dlgNewVolWizard_NewVolParams.UserPassword != NULL)
        {
        SecZeroAndFreeMemory(
                    G_dlgNewVolWizard_NewVolParams.UserPassword,
                    strlen(G_dlgNewVolWizard_NewVolParams.UserPassword)
                   );
        G_dlgNewVolWizard_NewVolParams.UserPassword = NULL;
        }

    if (G_dlgNewVolWizard_NewVolParams.UserPasswordConfirm != NULL)
        {
        SecZeroAndFreeMemory(
                    G_dlgNewVolWizard_NewVolParams.UserPasswordConfirm,
                    strlen(G_dlgNewVolWizard_NewVolParams.UserPasswordConfirm)
                   );
        G_dlgNewVolWizard_NewVolParams.UserPasswordConfirm = NULL;
        }

    SecZeroMemory(
                  &G_dlgNewVolWizard_NewVolParams, 
                  sizeof(G_dlgNewVolWizard_NewVolParams)
                 );
}


// =========================================================================
// Pass this the HWND for the *main* dialog
BOOL _dlgNewVolWizard_QuerySiblings(HWND hDlg)
{
    BOOL retval = FALSE;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_dlgNewVolWizard_QuerySiblings\n")));

    G_dlgNewVolWizard_NewVolParams.ParamsOK = TRUE;

    if (PropSheet_QuerySiblings(hDlg, 0, 0) != 0)
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("PropSheet_QuerySiblings returned non-zero; not proceeding with new volume\n")));
        // Do nothing; retval already set to FALSE
        }
    // Not clear why, but regardless of whether the dlgProc returns
    // TRUE/FALSE for the QuerySiblings call, the above always returns zero?!
    // Therefore, we use a kludge to get the *real* result...
    else if (G_dlgNewVolWizard_NewVolParams.ParamsOK == FALSE)
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("ParamsOK set to FALSE; not proceeding with new volume\n")));
        // Do nothing; retval already set to FALSE
        }
    else
        {
        retval = TRUE;
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_dlgNewVolWizard_QuerySiblings\n")));
    return retval;
}


// =========================================================================
// Dialog OK'd; automount volume
MOUNT_RESULT CALLBACK dlgNewVolWizard_AutoMountNew(HWND hDlg)
{
    MOUNT_RESULT retval = MR_UNKNOWN;
    HCURSOR csrHourglass;
    HCURSOR csrPrevious;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgNewVolWizard_AutoMountNew\n")));

    G_dlgNewVolWizard_Mountpoint = GenerateDefaultMountpoint(
                                           hDlg,
                                           G_dlgNewVolWizard_NewVolParams.Filename
                                          );
    if (G_dlgNewVolWizard_Mountpoint != NULL)
        {
        csrHourglass = LoadCursor(NULL, IDC_WAIT);
        csrPrevious = SetCursor(csrHourglass);
        retval = driver_Mount(
                              G_dlgNewVolWizard_Mountpoint,
                              G_dlgNewVolWizard_NewVolParams.Filename,
                              G_dlgNewVolWizard_NewVolParams.Keyfile,
                              G_dlgNewVolWizard_NewVolParams.Offset,
                              (!(G_dlgNewVolWizard_NewVolParams.UseKeyfile)),
                              G_dlgNewVolWizard_NewVolParams.UserPassword,
                              G_dlgNewVolWizard_NewVolParams.SaltLength,
                              G_dlgNewVolWizard_NewVolParams.KeyIterations,
                              FALSE,
                              G_Options->RevertVolTimestamps
                             );

        if (retval != MR_MOUNTED_OK)
            {
            SecZeroAndFreeWCHARMemory(G_dlgNewVolWizard_Mountpoint);
            G_dlgNewVolWizard_Mountpoint = NULL;
            }

        SetCursor(csrPrevious);
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgNewVolWizard_AutoMountNew\n")));
    return retval;
}


// =========================================================================
// Dialog OK'd; new volume
BOOL CALLBACK dlgNewVolWizard_CreateNew(HWND hDlg)
{
    BOOL retval = FALSE;
    CDB_DETAILS CDBDetails;
    CDB_METADATA CDBMetaData;
    FREEOTFEBYTE* remainingRandomPool;
    WCHAR* useKeyFilename;
    FREEOTFEBYTE randomPool[(CRITICAL_DATA_LENGTH / 8)];

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgNewVolWizard_CreateNew\n")));

    memset(
           &G_dlgNewVolWizard_NewVolParams, 
           0,
           sizeof(G_dlgNewVolWizard_NewVolParams)
          );
    G_dlgNewVolWizard_NewVolParams.ParamsOK = TRUE;

    // Check the user's input is OK...
    if (!(dlgNewVolWizard_ValidateAll(GetParent(hDlg))))
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Can't validate all of user input successfully\n")));
        // retval already set to FALSE, just switch to the appropriate tab
        PropSheet_SetCurSel(
                            GetParent(hDlg), 
                            NULL, 
                            G_dlgNewVolWizard_NewVolParams.ErrorPageIdx
                           );
        }
    else
        {
        HCURSOR csrHourglass;
        HCURSOR csrPrevious;

        csrHourglass = LoadCursor(NULL, IDC_WAIT);
        csrPrevious = SetCursor(csrHourglass);

        // Populate CDB details...
        CDBDetails.PartitionSize = G_dlgNewVolWizard_NewVolParams.Size;
        CDBDetails.VolumeFlags = 0;

        CDBDetails.SectorIVGenMethod = G_dlgNewVolWizard_NewVolParams.SectorIVGenMethod;

        if (G_dlgNewVolWizard_NewVolParams.KeySizeSelectedCypher >= 0)
            {
            CDBDetails.MasterKeyLength = G_dlgNewVolWizard_NewVolParams.KeySizeSelectedCypher;
            }
        else
            {
            CDBDetails.MasterKeyLength = G_dlgNewVolWizard_NewVolParams.KeySizeUser;
            }

        CDBDetails.VolumeIVLength = 0;
        if (G_dlgNewVolWizard_NewVolParams.XORIV)
            {
            CDBDetails.VolumeIVLength = max(
                                            0,
                                            G_dlgNewVolWizard_NewVolParams.BlockSizeSelectedCypher
                                           );
            }

        CDBDetails.DefaultDrive = 0;  // Not used on PDA version

        // Combine random data buffers as configured by the user...
        memset(randomPool, 0, sizeof(randomPool));
        if (G_dlgNewVolWizard_NewVolParams.SelectedRNG.UseUserInput)
            {
            XORBlock(
                     randomPool,
                     G_dlgNewVolWizard_NewVolParams.RandomPool_UserInput,
                     randomPool,
                     (G_dlgNewVolWizard_NewVolParams.RandomPoolFilled_UserInput / 8)
                    );
            }

        if (G_dlgNewVolWizard_NewVolParams.SelectedRNG.UseMSCryptoAPI)
            {
            XORBlock(
                     randomPool,
                     G_dlgNewVolWizard_NewVolParams.RandomPool_MSCryptoAPI,
                     randomPool,
                     (G_dlgNewVolWizard_NewVolParams.RandomPoolFilled_MSCryptoAPI / 8)
                    );
            }

        CDBDetails.MasterKey = randomPool;
        CDBDetails.VolumeIV = CDBDetails.MasterKey +
                              (CDBDetails.MasterKeyLength / 8);
        remainingRandomPool = CDBDetails.VolumeIV + 
                              (CDBDetails.VolumeIVLength / 8);


        // Populate CDB metadata...
        wcscpy(
               CDBMetaData.HashDeviceName,
               G_dlgNewVolWizard_NewVolParams.DriverHash
              );
        CDBMetaData.HashGUID = G_dlgNewVolWizard_NewVolParams.ImplGUIDHash;

        wcscpy(
               CDBMetaData.CypherDeviceName,
               G_dlgNewVolWizard_NewVolParams.DriverCypher
              );
        CDBMetaData.CypherGUID = G_dlgNewVolWizard_NewVolParams.ImplGUIDCypher;

        useKeyFilename = NULL;
        if (G_dlgNewVolWizard_NewVolParams.UseKeyfile)
            {
            useKeyFilename = G_dlgNewVolWizard_NewVolParams.Keyfile;
            }


        retval = driver_CreateVolumeFile(
                        G_dlgNewVolWizard_NewVolParams.Filename,
                        useKeyFilename,
                        G_dlgNewVolWizard_NewVolParams.Offset,
                        G_dlgNewVolWizard_NewVolParams.UserPassword,
                        G_dlgNewVolWizard_NewVolParams.SaltLength,  
                        G_dlgNewVolWizard_NewVolParams.KeyIterations,
                        remainingRandomPool,
                        &CDBDetails,
                        &CDBMetaData
                       );

        SetCursor(csrPrevious);
        }

    if (retval)
        {
        if (G_dlgNewVolWizard_NewVolParams.MountAfterCreation)
            {
            dlgNewVolWizard_AutoMountNew(hDlg);
            }
        }

    // Cleardown sensitive information
    _dlgNewVolWizard_SecZeroParams();
    SecZeroMemory(&CDBDetails, sizeof(CDBDetails));
    SecZeroMemory(&CDBMetaData, sizeof(CDBMetaData));
    SecZeroMemory(randomPool, sizeof(randomPool));

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgNewVolWizard_CreateNew\n")));
    return retval;
}


// =========================================================================
void _dlgNewVolWizard_EnableDisableControls_CDBLocation(HWND hDlg)
{
    BOOL enableCtrl;

    enableCtrl = (IsDlgButtonChecked(
                                     hDlg, 
                                     IDC_RADIO_CDBINKEYFILE
                                    ) == BST_CHECKED);

    SetControlEnabled(hDlg, IDC_STATIC_KEYFILE, enableCtrl);
    SetControlEnabled(hDlg, IDC_EDIT_KEYFILE, enableCtrl);
    SetControlEnabled(hDlg, IDC_BUTTON_BROWSEKEYFILE, enableCtrl);
}

// =========================================================================
void _dlgNewVolWizard_EnableDisableControls_HashCypherIV(HWND hDlg)
{
    DRIVER_AND_ALG* driverAndAlg;
    BOOL useSectorIVs;
    WCHAR strPrettySectorIVGenMethod[MAX_PRETTYPRINTED_SECTORIVGENMETHOD];
    BOOL itemSelected;

    // Determine if we can use sector IVs...
    // If:
    //  *) The selected cypher's blocksize is zero of variable, or
    //  *) The cypher uses LRW, or
    //  *) The cypher uses LRW
    // sector IVs aren't/cannot be used
    useSectorIVs = FALSE;
    if (!(Combobox_GetItem(
                             hDlg,
                             IDC_COMBO_CYPHER,
                             &driverAndAlg
                            )))
        {
        memset(
               G_dlgNewVolWizard_NewVolParams.DriverCypher,
               0,
               sizeof(G_dlgNewVolWizard_NewVolParams.DriverCypher)
              );
        }
    else
        {
        useSectorIVs = (driverAndAlg->Cypher.BlockSize > 0) &&
                       (driverAndAlg->Cypher.Mode != CYPHER_MODE_LRW) &&
                       (driverAndAlg->Cypher.Mode != CYPHER_MODE_XTS);
        }

    // We don't use sector IVs; set the selected as appropriate
    if (!(useSectorIVs))
        {
        driver_PrettyprintSectorIVGenMethod(
                            SCTRIVGEN_NONE,
                            strPrettySectorIVGenMethod,
                            sizeof(strPrettySectorIVGenMethod)
                           );
        SetComboBoxSelected(hDlg, IDC_COMBO_SECTORIV, strPrettySectorIVGenMethod);

        CheckDlgButton(
                       hDlg, 
                       IDC_CHECK_VOLUMEIV,
                       BST_UNCHECKED
                      );
        }

    // We do/don't use sector IVs; set controls enabled as appropriate
    SetControlEnabled(hDlg, IDC_STATIC_SECTORIV, useSectorIVs);
    SetControlEnabled(hDlg, IDC_COMBO_SECTORIV, useSectorIVs);
    SetControlEnabled(hDlg, IDC_CHECK_VOLUMEIV, useSectorIVs);

    itemSelected = Combobox_GetItem(
                             hDlg,
                             IDC_COMBO_HASH,
                             &driverAndAlg
                            );
    SetControlEnabled(hDlg, IDC_BUTTON_HASHINFO, itemSelected);

    itemSelected = Combobox_GetItem(
                             hDlg,
                             IDC_COMBO_CYPHER,
                             &driverAndAlg
                            );
    SetControlEnabled(hDlg, IDC_BUTTON_CYPHERINFO, itemSelected);

}


// =========================================================================
// Navigate forwards/backwards in the wizard
// PageDirection - Set to +1 to go onto the next page; -1 to go back
void CALLBACK _dlgNewVolWizard_PageChange(
    HWND hDlg, 
    int PageDirection
)
{
    int currPage;
    int newPage;
    BOOL identifiedNewPage;


    currPage = _dlgNewVolWizard_GetPageIdxOfPage(PropSheet_GetCurrentPageHwnd(GetParent(hDlg)));
    newPage = currPage;

    // Get the latest information entered by the user...
    _dlgNewVolWizard_QuerySiblings(hDlg);

    identifiedNewPage = FALSE;
    while (!(identifiedNewPage))
        {
        if (PageDirection < 0) 
            {
            newPage = max((newPage - 1), 0);
            }
        else if (PageDirection > 0)
            {
            // Note: Any error message will have been displayed to user
            //       during validation
            if (dlgNewVolWizard_ValidatePage(PropSheet_GetCurrentPageHwnd(GetParent(hDlg))))
                {
                // -1 here as NEWVOLWIZARD_STEPS is the total number; not
                // the largest page indexe
                newPage = min((newPage + 1), (NEWVOLWIZARD_STEPS - 1));
                }
            }

        // Check if the new page is skipped
        identifiedNewPage = TRUE;
        if (newPage == PAGE_IDX_OFFSET)
            {
            identifiedNewPage = _dlgNewVolWizard_IsHidden();
            if (identifiedNewPage)
                {
                if (MsgPromptWarn(
                           hDlg,
                           _("A file with the name specified already exists.\n\nIf you continue, a hidden volume will be created within this file, overwriting part of it.\n\nDo you wish to proceed?"),
                           MB_YESNO
                          ) == IDNO)
                    {
                    // Back up to the previous page
                    identifiedNewPage = FALSE;
                    PageDirection = -1;
                    }

                }

            }
        else if (newPage == PAGE_IDX_MASTERKEYLEN)
            {
            identifiedNewPage = (G_dlgNewVolWizard_NewVolParams.KeySizeSelectedCypher < 0);
            }
        else if (newPage == PAGE_IDX_RNGMOUSEMOVEMENT)
            {
            // Skip mouse RNG page if user doesn't want to use it
            identifiedNewPage = G_dlgNewVolWizard_NewVolParams.SelectedRNG.UseUserInput;
            }

        }

    if (currPage != newPage)
        {
        PropSheet_SetCurSel(
                            GetParent(hDlg), 
                            NULL, 
                            newPage
                           );

        if (newPage == PAGE_IDX_RNGMOUSEMOVEMENT)
            {
            // Identify if the user needs instructions...
            if (
                (G_dlgNewVolWizard_RandomUserInput_Filled / 8) <
                sizeof(G_dlgNewVolWizard_RandomUserInput_Data)
               )
                {
                MsgInfoString(
                              hDlg,
                              G_hInstance,
                              IDS_STRING_INSTR_RNGMOUSEMOVEMENT
                             );
                }
            }
        }

}


// =========================================================================
HBRUSH _dlgNewVolWizard_HandleMsg_WM_CTLCOLORSTATIC(HWND hDlg, WPARAM wParam, LPARAM lParam)
{
    // Put warning message in red
    if (
        ((HWND)lParam == GetDlgItem(hDlg, IDC_STATIC_WARNING_BLOCK_1)) ||
        ((HWND)lParam == GetDlgItem(hDlg, IDC_STATIC_WARNING_BLOCK_2))
       )
        {
        SetTextColor((HDC)wParam, COLOR_WARNING);
        }

    return (GetStockObject(WHITE_BRUSH));
}


// =========================================================================
BOOL CALLBACK dlgNewVolWizard_HandleMsg_WM_COMMAND(HWND hDlg, WPARAM wParam)
{
    BOOL retval = FALSE;
    int ctrlID;
    DRIVER_AND_ALG* driverAndAlg;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgNewVolWizard_HandleMsg_WM_COMMAND\n")));
    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: %d\n"), LOWORD(wParam)));

    ctrlID = LOWORD(wParam);

    switch(ctrlID)
        {
        case IDOK:
            {
            // IMPORTANT: EndDialog *not* called for property sheet dialog
            // We just forward the msg received by the *page* to the owning
            // parent; the property sheet dialog
            PropSheet_PressButton(GetParent(hDlg), PSBTN_OK);
            retval = TRUE;
            break;
            }

        case IDCANCEL:
            {
            // IMPORTANT: EndDialog *not* called for property sheet dialog
            // We just forward the msg received by the *page* to the owning
            // parent; the property sheet dialog
            PropSheet_PressButton(GetParent(hDlg), PSBTN_CANCEL);
            retval = TRUE;
            break;
            }

        case ID_WIZ_BACK:
            {
            _dlgNewVolWizard_PageChange(hDlg, -1);
            retval = TRUE;
            break;
            }

        case ID_WIZ_NEXT:
            {
            _dlgNewVolWizard_PageChange(hDlg, +1);
            retval = TRUE;
            break;
            }

        case IDC_BUTTON_BROWSEFILENAME:
        case IDC_BUTTON_BROWSEKEYFILE:
            {
            OPENFILENAME ofn;
            DWORD flags;
            WCHAR filename[FREEOTFE_MAX_FILENAME_LENGTH];
            WCHAR* useFilter;

            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Browse...\n")));

            memset(filename, 0, sizeof(filename));

            ofn.lStructSize       = sizeof(ofn);
            ofn.hwndOwner         = hDlg;
            ofn.hInstance         = NULL;
            ofn.lpstrCustomFilter = NULL;
            ofn.lpstrFile         = filename;
            ofn.nMaxFile          = (sizeof(filename) / sizeof(filename[0]));
            ofn.lpstrFileTitle    = NULL;
            ofn.nMaxFileTitle     = 0;
            ofn.lpstrInitialDir   = NULL;
            ofn.lpstrTitle        = NULL;

            if (ctrlID == IDC_BUTTON_BROWSEFILENAME)
                {
                useFilter          = FileFilterGenerate(FILE_FILTER_FLT_VOLUMES);
                ofn.nMaxCustFilter = FILE_FILTER_CNT_VOLUMES;
                ofn.nFilterIndex   = FILE_FILTER_DFLT_VOLUMES;
                }
            else
                {
                useFilter          = FileFilterGenerate(FILE_FILTER_FLT_KEYFILES);
                ofn.nMaxCustFilter = FILE_FILTER_CNT_KEYFILES;
                ofn.nFilterIndex   = FILE_FILTER_DFLT_KEYFILES;
                }

            ofn.lpstrFilter = useFilter;

            flags = (
                     // OFN_HIDEREADONLY |
                     OFN_FILEMUSTEXIST | 
                     OFN_PATHMUSTEXIST
                    );
            ofn.Flags             = flags;
            ofn.nFileOffset       = 0;
            ofn.nFileExtension    = 0;
            ofn.lpstrDefExt       = NULL;
            ofn.lCustData         = 0;
            ofn.lpfnHook          = NULL;
            ofn.lpTemplateName    = NULL;

            if (GetSaveFileName(&ofn))
                {
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Open file dialog OK'd...\n")));
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Filename: %ls\n"), filename));
                if (ctrlID == IDC_BUTTON_BROWSEFILENAME)
                    {
                    SetDlgItemText(hDlg, IDC_EDIT_FILENAME, filename);
                    }
                else
                    {
                    SetDlgItemText(hDlg, IDC_EDIT_KEYFILE, filename);
                    }
                }
            else
                {
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Open file dialog cancelled\n")));
                }

            FileFilterFree(useFilter);

            break;
            }

        case IDC_BUTTON_HASHINFO:
            {
            if (Combobox_GetItem(
                                 hDlg,
                                 IDC_COMBO_HASH,
                                 &driverAndAlg
                                ))
                {
                DisplayDlgHashInfo(
                                    hDlg, 
                                    driverAndAlg->DriverFilename, 
                                    driverAndAlg->DriverImplAlgGUID
                                   );
                }

            break;
            }

        case IDC_BUTTON_CYPHERINFO:
            {
            if (Combobox_GetItem(
                                 hDlg,
                                 IDC_COMBO_CYPHER,
                                 &driverAndAlg
                                ))
                {
                DisplayDlgCypherInfo(
                                    hDlg, 
                                    driverAndAlg->DriverFilename, 
                                    driverAndAlg->DriverImplAlgGUID
                                   );
                }

            break;
            }

        case IDC_RADIO_CDBINVOLUME:
        case IDC_RADIO_CDBINKEYFILE:
            {
            _dlgNewVolWizard_EnableDisableControls_CDBLocation(hDlg);
            break;
            }

        case IDC_COMBO_CYPHER:
            {
            _dlgNewVolWizard_EnableDisableControls_HashCypherIV(hDlg);
            break;
            }

        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgNewVolWizard_HandleMsg_WM_COMMAND\n")));
    return TRUE;
}


// =========================================================================
// Supply with the *main* dialog
BOOL dlgNewVolWizard_ValidatePageIdx(HWND hDlg, int PageIdx)
{
    BOOL retval = FALSE;
    UINT tmpUINT;
    LARGE_INTEGER fileSize;
    int errorPage;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgNewVolWizard_ValidatePage\n")));

    errorPage = G_dlgNewVolWizard_NewVolParams.ErrorPageIdx;

    if (!(_dlgNewVolWizard_QuerySiblings(hDlg)))
        {
        // Do nothing; retval already set to FALSE
        }
    else
        {
        switch (PageIdx)
            {
            case PAGE_IDX_WELCOME:
                {
                // Always completed
                retval = TRUE;
                break;
                }

            case PAGE_IDX_FILENAME:
                {
                // Ensure that filename has been specified
                retval = (wcslen(G_dlgNewVolWizard_NewVolParams.Filename) > 0);
                if (!(retval))
                    {
                    MsgError(hDlg, _("Please enter a filename for the new volume"));
                    errorPage = PAGE_IDX_FILENAME;
                    break;
                    }

                break;
                }

            case PAGE_IDX_OFFSET:
                {
                // Ensure offset specified is less than the size of the file
                retval = (wcslen(G_dlgNewVolWizard_NewVolParams.Filename) > 0);
                if (!(retval))
                    {
                    // Ignore error; handled by PAGE_IDX_FILENAME
                    break;
                    }
                else
                    {
                    // Only carry out this check if the file already exists
                    if (!(_dlgNewVolWizard_IsHidden()))
                        {
                        // New file...
                        retval = TRUE;
                        }
                    else
                        {
                        // File exists...
                        if (!(GetFileSize_Filename(
                                  G_dlgNewVolWizard_NewVolParams.Filename,
                                  &fileSize
                                 )))
                            {
                            MsgError(hDlg, _("Unable to get filesize"));
                            errorPage = PAGE_IDX_OFFSET;
                            break;
                            }
                        else
                            {
                            retval = (fileSize.QuadPart > G_dlgNewVolWizard_NewVolParams.Offset.QuadPart);
                            if (!(retval))
                                {
                                MsgError(hDlg, _("The offset specified is larger than the file selected!"));
                                errorPage = PAGE_IDX_OFFSET;
                                break;
                                }
                            }
                        }
                    }

                break;
                }

            case PAGE_IDX_SIZE:
                {
                // If creating a hidden container, ensure that the existing file
                // is large enough to store the hidden container
                // Ensure offset specified is less than the size of the file
                retval = (wcslen(G_dlgNewVolWizard_NewVolParams.Filename) > 0);
                if (!(retval))
                    {
                    // Ignore error; handled by PAGE_IDX_FILENAME
                    break;
                    }
                else
                    {
                    // Only carry out this check if the file already exists
                    if (!(_dlgNewVolWizard_IsHidden()))
                        {
                        // New file...

                        retval = (G_dlgNewVolWizard_NewVolParams.Size.QuadPart >= (MIN_VOLUME_SIZE * BYTES_IN_MEGABYTE));
                        if (!(retval))
                            {
                            MsgError(hDlg, _("The size specified is too small; please enter a larger value."));
                            errorPage = PAGE_IDX_SIZE;
                            break;
                            }

                        // It would be *nice* to check that the volume the filename is stored on
                        // has enough storage for the requested size volume, but that is not
                        // a priority to implement right now...
                        // xxx - implement this
                        }
                    else
                        {
                        // File exists...
                        if (!(GetFileSize_Filename(
                                  G_dlgNewVolWizard_NewVolParams.Filename,
                                  &fileSize
                                 )))
                            {
                            MsgError(hDlg, _("Unable to get filesize"));
                            errorPage = PAGE_IDX_SIZE;
                            break;
                            }
                        else
                            {
                            tmpUINT = 0;
                            if (!(G_dlgNewVolWizard_NewVolParams.UseKeyfile))
                                {
                                tmpUINT = (CRITICAL_DATA_LENGTH / 8);
                                }
                            retval = (
                                      fileSize.QuadPart >
                                      (
                                       G_dlgNewVolWizard_NewVolParams.Size.QuadPart +
                                       G_dlgNewVolWizard_NewVolParams.Offset.QuadPart +
                                       tmpUINT
                                      )
                                     );
                            if (!(retval))
                                {
                                MsgError(hDlg, _("The host file is not large enough for a hidden volume with this offset and size"));
                                errorPage = PAGE_IDX_SIZE;
                                break;
                                }
                            }
                        }
                    }

                break;
                }

            case PAGE_IDX_HASHCYPHERIV:
                {
                // Ensure user has selected a hash, cypher and the number of
                // key iterations is greater than zero

                retval = (wcslen(G_dlgNewVolWizard_NewVolParams.DriverHash) > 0);
                if (!(retval))
                    {
                    MsgError(hDlg, _("Please select the hash to be used"));
                    errorPage = PAGE_IDX_HASHCYPHERIV;
                    break;
                    }

                retval = (wcslen(G_dlgNewVolWizard_NewVolParams.DriverCypher) > 0);
                if (!(retval))
                    {
                    MsgError(hDlg, _("Please select the cypher to be used"));
                    errorPage = PAGE_IDX_HASHCYPHERIV;
                    break;
                    }

                retval = (G_dlgNewVolWizard_NewVolParams.SectorIVGenMethod != SCTRIVGEN_UNKNOWN);
                if (!(retval))
                    {
                    MsgError(hDlg, _("Please select the sector IV generation method"));
                    errorPage = PAGE_IDX_HASHCYPHERIV;
                    break;
                    }

                // Everything OK...
                break;
                }

            case PAGE_IDX_MASTERKEYLEN:
                {
                // If the user's selected cypher has a keysize >= 0, skip this
                // page

                retval = (G_dlgNewVolWizard_NewVolParams.KeySizeSelectedCypher >= 0);
                if (!(retval))
                    {
                    // Ensure user entered master keylength is a multiple of 8
                    retval = ((G_dlgNewVolWizard_NewVolParams.KeySizeUser % 8) ==0);
                    if (!(retval))
                        {
                        MsgError(hDlg, _("The keylength must be a multiple of 8 bits"));
                        errorPage = PAGE_IDX_MASTERKEYLEN;
                        break;
                        }
                    }

                break;
                }

            case PAGE_IDX_RNGSELECT:
                {
                // Always completed (RNG defaults; this should never be FALSE)
                retval = (
                          G_dlgNewVolWizard_NewVolParams.SelectedRNG.UseUserInput ||
                          G_dlgNewVolWizard_NewVolParams.SelectedRNG.UseMSCryptoAPI
                         );
                if (!(retval))
                    {
                    MsgError(hDlg, _("Please select an RNG"));
                    errorPage = PAGE_IDX_RNGSELECT;
                    break;
                    }

                if (retval)
                    {
                    // Check RNG functioned correctly
                    if (
                        G_dlgNewVolWizard_NewVolParams.SelectedRNG.UseMSCryptoAPI &&
                        (G_dlgNewVolWizard_NewVolParams.RandomPoolFilled_MSCryptoAPI < CRITICAL_DATA_LENGTH)
                       )
                        {
                        MsgError(hDlg, _("Microsoft CryptoAPI failed to produce sufficient random data."));
                        errorPage = PAGE_IDX_RNGSELECT;
                        retval = FALSE;
                        break;
                        }
                    }

                break;
                }

            case PAGE_IDX_RNGMOUSEMOVEMENT:
                {
                retval = TRUE;
                if (
                    G_dlgNewVolWizard_NewVolParams.SelectedRNG.UseUserInput &&
                    (G_dlgNewVolWizard_NewVolParams.RandomPoolFilled_UserInput < CRITICAL_DATA_LENGTH)
                   )
                    {
                    MsgError(hDlg, _("User input has not yet generated sufficient random data."));
                    errorPage = PAGE_IDX_RNGMOUSEMOVEMENT;
                    retval = FALSE;
                    break;
                    }

                break;
                }

            case PAGE_IDX_PASSWORD:
                {
                // Ensure the password is confirmed, and something has been
                // entered as a password
                if (
                    (G_dlgNewVolWizard_NewVolParams.UserPassword == NULL) ||
                    (G_dlgNewVolWizard_NewVolParams.UserPasswordConfirm == NULL)
                   )
                    {
                    // <shrugs>
                    MsgError(hDlg, _("Error getting user password!"));                    
                    errorPage = PAGE_IDX_PASSWORD;
                    // retval already set to FALSE
                    break;
                    }
                if (strlen(G_dlgNewVolWizard_NewVolParams.UserPassword) == 0)
                    {
                    MsgError(hDlg, _("Please enter a password"));
                    errorPage = PAGE_IDX_PASSWORD;
                    // retval already set to FALSE
                    break;
                    }
                else if (strcmp(
                                G_dlgNewVolWizard_NewVolParams.UserPassword,
                                G_dlgNewVolWizard_NewVolParams.UserPasswordConfirm
                               ) != 0)
                    {
                    MsgError(hDlg, _("Passwords entered do not match"));
                    errorPage = PAGE_IDX_PASSWORD;
                    // retval already set to FALSE
                    break;
                    }
                else
                    {
                    retval = TRUE;
                    }

                break;
                }

            case PAGE_IDX_READYFORNEWVOL:
                {
                // Always completed
                retval = TRUE;
                break;
                }

            case PAGE_IDX_KEYITERATIONS:
                {
                retval = (G_dlgNewVolWizard_NewVolParams.KeyIterations > 0);
                if (!(retval))
                    {
                    MsgError(hDlg, _("Please enter a number of key iterations greater than one"));
                    errorPage = PAGE_IDX_HASHCYPHERIV;
                    break;
                    }

                break;
                }

            case PAGE_IDX_SALT:
                {
                // Ensure salt is a multiple of 8
                retval = ((G_dlgNewVolWizard_NewVolParams.SaltLength % 8) == 0);
                if (!(retval))
                    {
                    MsgError(hDlg, _("The salt length must be a multiple of 8"));
                    errorPage = PAGE_IDX_SALT;
                    break;
                    }

                break;
                }

            case PAGE_IDX_CDBLOCATION:
                {
                // Either the user's selected to include the CDB in their volume
                // file, or they've specified a keyfilename
                if (!(G_dlgNewVolWizard_NewVolParams.UseKeyfile))
                    {
                    retval = TRUE;
                    }
                else
                    {
                    retval = (wcslen(G_dlgNewVolWizard_NewVolParams.Keyfile) > 0);
                    if (!(retval))
                        {
                        MsgError(hDlg, _("If you wish to use a keyfile, please specify it's filename"));
                        errorPage = PAGE_IDX_CDBLOCATION;
                        break;
                        }
                    else
                        {
                        retval = (!(SDUCheckFileExists(G_dlgNewVolWizard_NewVolParams.Keyfile)));
                        if (!(retval))
                            {
                            MsgError(hDlg, _("A file with the filename specified already exists; please enter different filename"));
                            errorPage = PAGE_IDX_CDBLOCATION;
                            break;
                            }
                        }

                    }

                break;
                }

            case PAGE_IDX_SUMMARY:
                {
                // Always completed
                retval = TRUE;
                break;
                }

            default:
                {
                // This can't happen?!
                MsgError(hDlg, TEXT("Validation missed a page!"));
                MsgError(hDlg, _("Please report seeing this error!"));
                errorPage = PageIdx;
                retval = FALSE;
                }

            }
        }

    G_dlgNewVolWizard_PageCompleted[PageIdx] = retval;
    if (!(retval))
        {
        G_dlgNewVolWizard_NewVolParams.ErrorPageIdx = errorPage;
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgNewVolWizard_ValidatePage\n")));
    return retval;
}


// =========================================================================
// Supply with the dialog to be validated
BOOL dlgNewVolWizard_ValidatePage(HWND hDlg)
{
    BOOL retval;
    int pageIdx;

    pageIdx = _dlgNewVolWizard_GetPageIdxOfPage(hDlg);
    retval = dlgNewVolWizard_ValidatePageIdx(GetParent(hDlg), pageIdx);

    return retval;
}


// =========================================================================
// Supply with the *main* dialog
BOOL dlgNewVolWizard_ValidateAll(HWND hDlg)
{
    BOOL retval = TRUE;
    int i;

    for (i = 0; i < NEWVOLWIZARD_STEPS; i++)
        {
        retval = retval && dlgNewVolWizard_ValidatePageIdx(hDlg, i);
        }

    return retval;
}


// =========================================================================
// Note: This *purely* gets the data the user entered. No validation. No
//       corrections for values that can be autodetermined (validation does
//       that, where needed)
BOOL CALLBACK dlgNewVolWizard_HandleMsg_PSM_QUERYSIBLINGS(HWND hDlg, WPARAM wParam, LPARAM lParam)
{
    BOOL retval = TRUE;
    int pageIdx;
    DRIVER_AND_ALG* driverAndAlg;
    
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgNewVolWizard_HandleMsg_PSM_QUERYSIBLINGS\n")));

    pageIdx = _dlgNewVolWizard_GetPageIdxOfPage(hDlg);

    switch (pageIdx)
        {
        case PAGE_IDX_WELCOME:
            {
            // Do nothing
            retval = TRUE;
            break;
            }

        case PAGE_IDX_FILENAME:
            {
            GetDlgItemText( 
                           hDlg, 
                           IDC_EDIT_FILENAME,
                           G_dlgNewVolWizard_NewVolParams.Filename,
                           sizeof(G_dlgNewVolWizard_NewVolParams.Filename)
                          );
            retval = TRUE;
            break;
            }

        case PAGE_IDX_OFFSET:
            {
            G_dlgNewVolWizard_NewVolParams.Offset.QuadPart = GetDlgItemInt(
                                   hDlg,
                                   IDC_EDIT_OFFSET,
                                   NULL,
                                   FALSE
                                  );
            retval = TRUE;
            break;
            }

        case PAGE_IDX_SIZE:
            {
            G_dlgNewVolWizard_NewVolParams.Size.QuadPart = GetDlgItemInt(
                                   hDlg,
                                   IDC_EDIT_SIZE,
                                   NULL,
                                   FALSE
                                  );

            G_dlgNewVolWizard_NewVolParams.Size.QuadPart =
                 G_dlgNewVolWizard_NewVolParams.Size.QuadPart * BYTES_IN_MEGABYTE;
            retval = TRUE;
            break;
            }

        case PAGE_IDX_HASHCYPHERIV:
            {
            // Selected hash...
            if (!(Combobox_GetItem(
                                     hDlg,
                                     IDC_COMBO_HASH,
                                     &driverAndAlg
                                    )))
                {
                memset(
                       G_dlgNewVolWizard_NewVolParams.DriverHash,
                       0,
                       sizeof(G_dlgNewVolWizard_NewVolParams.DriverHash)
                      );
                }
            else
                {
                wcscpy(
                       G_dlgNewVolWizard_NewVolParams.PrettyNameHash,
                       driverAndAlg->PrettyName
                      );
                wcscpy(
                       G_dlgNewVolWizard_NewVolParams.DriverHash,
                       driverAndAlg->DriverFilename
                      );
                G_dlgNewVolWizard_NewVolParams.ImplGUIDHash = driverAndAlg->DriverImplAlgGUID;
                }


            // Selected cypher...
            if (!(Combobox_GetItem(
                                     hDlg,
                                     IDC_COMBO_CYPHER,
                                     &driverAndAlg
                                    )))
                {
                memset(
                       G_dlgNewVolWizard_NewVolParams.DriverCypher,
                       0,
                       sizeof(G_dlgNewVolWizard_NewVolParams.DriverCypher)
                      );
                }
            else
                {
                wcscpy(
                       G_dlgNewVolWizard_NewVolParams.PrettyNameCypher,
                       driverAndAlg->PrettyName
                      );
                wcscpy(
                       G_dlgNewVolWizard_NewVolParams.DriverCypher,
                       driverAndAlg->DriverFilename
                      );
                G_dlgNewVolWizard_NewVolParams.ImplGUIDCypher = driverAndAlg->DriverImplAlgGUID;
                G_dlgNewVolWizard_NewVolParams.KeySizeSelectedCypher = driverAndAlg->Cypher.KeySizeRequired;
                G_dlgNewVolWizard_NewVolParams.BlockSizeSelectedCypher = driverAndAlg->Cypher.BlockSize;
                }


            // Sector IV generation method...
            if (!(Combobox_GetItem(
                                     hDlg,
                                     IDC_COMBO_SECTORIV,
                                     (void*)&G_dlgNewVolWizard_NewVolParams.SectorIVGenMethod
                                    )))
                {
                G_dlgNewVolWizard_NewVolParams.SectorIVGenMethod = SCTRIVGEN_UNKNOWN;
                }

            // Volume IV...
            G_dlgNewVolWizard_NewVolParams.XORIV = (IsDlgButtonChecked(
                                               hDlg, 
                                               IDC_CHECK_VOLUMEIV
                                              ) == BST_CHECKED);


            retval = TRUE;
            break;
            }

        case PAGE_IDX_MASTERKEYLEN:
            {
            G_dlgNewVolWizard_NewVolParams.KeySizeUser = GetDlgItemInt(
                                   hDlg,
                                   IDC_EDIT_MASTERKEYLEN,
                                   NULL,
                                   FALSE
                                  );
            retval = TRUE;
            break;
            }

        case PAGE_IDX_RNGSELECT:
            {
            G_dlgNewVolWizard_NewVolParams.SelectedRNG.UseUserInput =
                            (IsDlgButtonChecked(
                                                hDlg, 
                                                IDC_CHECK_RNGUSERINPUT
                                               ) == BST_CHECKED);

            G_dlgNewVolWizard_NewVolParams.SelectedRNG.UseMSCryptoAPI = 
                            (IsDlgButtonChecked(
                                                hDlg, 
                                                IDC_CHECK_RNGMSCRYPTOAPI
                                               ) == BST_CHECKED);

            // Generate MS CryptoAPI random data, if needed
            if (
                G_dlgNewVolWizard_NewVolParams.SelectedRNG.UseMSCryptoAPI &&
                (G_dlgNewVolWizard_NewVolParams.RandomPoolFilled_MSCryptoAPI < CRITICAL_DATA_LENGTH)
               )
                {
                G_dlgNewVolWizard_NewVolParams.RandomPoolFilled_MSCryptoAPI = 0;
                if (GenerateRNGDataMSCryptoAPI(
                                sizeof(G_dlgNewVolWizard_NewVolParams.RandomPool_MSCryptoAPI),
                                G_dlgNewVolWizard_NewVolParams.RandomPool_MSCryptoAPI
                                ))
                    {
                    G_dlgNewVolWizard_NewVolParams.RandomPoolFilled_MSCryptoAPI  = (sizeof(G_dlgNewVolWizard_NewVolParams.RandomPool_MSCryptoAPI) * 8);
                    }

                }

            retval = TRUE;
            break;
            }

        case PAGE_IDX_RNGMOUSEMOVEMENT:
            {
            G_dlgNewVolWizard_NewVolParams.RandomPoolFilled_UserInput = G_dlgNewVolWizard_RandomUserInput_Filled; 
            memcpy(
                   G_dlgNewVolWizard_NewVolParams.RandomPool_UserInput,
                   G_dlgNewVolWizard_RandomUserInput_Data,
                   (G_dlgNewVolWizard_RandomUserInput_Filled / 8)
                  );

            retval = TRUE;
            break;
            }

        case PAGE_IDX_PASSWORD:
            {
            // Ditch any previously obtained password...
            if (G_dlgNewVolWizard_NewVolParams.UserPassword != NULL)
                {
                SecZeroAndFreeMemory(
                            G_dlgNewVolWizard_NewVolParams.UserPassword,
                            strlen(G_dlgNewVolWizard_NewVolParams.UserPassword)
                           );
                G_dlgNewVolWizard_NewVolParams.UserPassword = NULL;
                }

            // Ditch any previously obtained confirmation password...
            if (G_dlgNewVolWizard_NewVolParams.UserPasswordConfirm != NULL)
                {
                SecZeroAndFreeMemory(
                            G_dlgNewVolWizard_NewVolParams.UserPasswordConfirm,
                            strlen(G_dlgNewVolWizard_NewVolParams.UserPasswordConfirm)
                           );
                G_dlgNewVolWizard_NewVolParams.UserPasswordConfirm = NULL;
                }
            G_dlgNewVolWizard_NewVolParams.UserPassword = GetPassword(
                                                     hDlg,
                                                     IDC_EDIT_PASSWORD
                                                    );

            if (G_dlgNewVolWizard_NewVolParams.UserPassword == NULL)
                {
                // Do nothing; no point getting confirmation password
                }
            else
                {
                G_dlgNewVolWizard_NewVolParams.UserPasswordConfirm = GetPassword(
                                                         hDlg,
                                                         IDC_EDIT_PASSWORDCONFIRM
                                                        );
                }

            retval = TRUE;
            break;
            }

        case PAGE_IDX_READYFORNEWVOL:
            {
            // Do nothing
            retval = TRUE;
            break;
            }

        case PAGE_IDX_KEYITERATIONS:
            {
            // Key iterations...
            G_dlgNewVolWizard_NewVolParams.KeyIterations = GetDlgItemInt(
                                          hDlg,
                                          IDC_EDIT_KEYITERATIONS,
                                          NULL,
                                          FALSE
                                         );

            retval = TRUE;
            break;
            }

        case PAGE_IDX_SALT:
            {
            G_dlgNewVolWizard_NewVolParams.SaltLength = GetDlgItemInt(
                                   hDlg,
                                   IDC_EDIT_SALTLENGTH,
                                   NULL,
                                   FALSE
                                  );
            retval = TRUE;
            break;
            }

        case PAGE_IDX_CDBLOCATION:
            {
            G_dlgNewVolWizard_NewVolParams.UseKeyfile = (IsDlgButtonChecked(
                                               hDlg, 
                                               IDC_RADIO_CDBINKEYFILE
                                              ) == BST_CHECKED);

            GetDlgItemText( 
                           hDlg, 
                           IDC_EDIT_KEYFILE,
                           G_dlgNewVolWizard_NewVolParams.Keyfile,
                           sizeof(G_dlgNewVolWizard_NewVolParams.Keyfile)
                          );

            retval = TRUE;
            break;
            }

        case PAGE_IDX_SUMMARY:
            {
            G_dlgNewVolWizard_NewVolParams.MountAfterCreation =
                            (IsDlgButtonChecked(
                                                hDlg, 
                                                IDC_CHECK_AUTOMOUNTAFTERCREATE
                                               ) == BST_CHECKED);

            retval = TRUE;
            break;
            }

        default:
            {
            // This can't happen?!
            MsgError(hDlg, TEXT("Unknown page?!"));
            retval = FALSE;
            }

        }


    if (!(retval))
        {
        G_dlgNewVolWizard_NewVolParams.ParamsOK = FALSE;
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgNewVolWizard_HandleMsg_PSM_QUERYSIBLINGS\n")));
    return retval;
}


// =========================================================================
BOOL CALLBACK dlgNewVolWizard_PropSheetPageProc(
                      HWND hDlg, 
                      UINT msg, 
                      WPARAM wParam, 
                      LPARAM lParam
                     )
{
    BOOL retval = FALSE;
    //static SHACTIVATEINFO s_sai = {0};

    switch(msg)
        {
        case WM_COMMAND:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_COMMAND\n")));
            retval = dlgNewVolWizard_HandleMsg_WM_COMMAND(hDlg, wParam);
            break;
            }

        case WM_DESTROY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_DESTROY\n")));
            retval = dlgNewVolWizard_HandleMsg_WM_DESTROY(hDlg);
            break;
            }

        case WM_INITDIALOG:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_INITDIALOG\n")));
            //s_sai.cbSize = sizeof(SHACTIVATEINFO);
            retval = dlgNewVolWizard_HandleMsg_WM_INITDIALOG(hDlg, lParam);
            break;
            }

        case WM_NOTIFY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_NOTIFY\n")));
            retval = dlgNewVolWizard_HandleMsg_WM_NOTIFY(hDlg, wParam, lParam);
            break;
            }

        case WM_SIZE:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_SIZE\n")));
            retval = dlgNewVolWizard_HandleMsg_WM_SIZE(hDlg);
            break;
            }

        //case WM_ACTIVATE:
            //{
            //DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_ACTIVATE\n")));
            //SHHandleWMActivate(hDlg, wParam, lParam, &s_sai, 0);
            //break;
            //}

        //case WM_SETTINGCHANGE:
            //{
            //DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_SETTINGCHANGE\n")));
            //SHHandleWMSettingChange(hDlg, wParam, lParam, &s_sai);
            //break;
            //}

        case PSM_QUERYSIBLINGS:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: PSM_QUERYSIBLINGS\n")));
            retval = dlgNewVolWizard_HandleMsg_PSM_QUERYSIBLINGS(hDlg, wParam, lParam);
            break;
            }

        //case WM_CTLCOLOR:
        case WM_CTLCOLORSTATIC:
        //case WM_CTLCOLOREDIT:
        //case WM_CTLCOLORLISTBOX:
        //case WM_CTLCOLORBTN:
        //case WM_CTLCOLORDLG:
        //case WM_CTLCOLORSCROLLBAR:
        //case WM_CTLCOLORMSGBOX:
            {
            retval = (BOOL)_dlgNewVolWizard_HandleMsg_WM_CTLCOLORSTATIC(hDlg, wParam, lParam);
            break;
            }

        }

    return retval;
}


// =========================================================================
// =========================================================================
