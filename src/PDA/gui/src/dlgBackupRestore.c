// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "main.h"
#include "dlgBackupRestore.h"
#include "FreeOTFEDebug.h"
#include "FreeOTFElib.h"
#include "FreeOTFEGUIlib.h"
#include "FreeOTFE4PDAGUIlib.h"
#include "FreeOTFE4PDAlib.h"
#include "DriverInterface.h"
#include "DriverInterfaceTwo.h"
#include "DriverInterfaceCommon.h"
#include "SDUi18n.h"
#include "SDUi18n_GUI.h"

#include "resource.h"

#include "SDUGeneral.h"

#include <stdio.h> // Required for _snwprintf_s 
#include <winuser.h>  // Required for IDOK, etc
#include <Commdlg.h>  // Required from OPENFILENAME
#include <aygshell.h>  // Required for SH... functions/definitions/etc
#pragma comment(lib, "aygshell") // Link in aygshell.lib


// Local to this file...
HWND G_dlgBackupRestore_MenuBar = NULL;


// =========================================================================
// Enums...

// Mode
typedef enum _DLG_MODE_BACKUPRESTORE {
    DLGMODE_BACKUPRESTORE_BACKUP   =    1,
    DLGMODE_BACKUPRESTORE_RESTORE  =    2
} DLG_BACKUPRESTORE, *PDLG_BACKUPRESTORE;


// =========================================================================
// Structures...

typedef struct _BACKUPRESTORE_PARAMETERS {
    BOOL ParamsOK;

    WCHAR SrcFilename[FREEOTFE_MAX_FILENAME_LENGTH];
    LARGE_INTEGER SrcOffset;
    WCHAR DestFilename[FREEOTFE_MAX_FILENAME_LENGTH];
    LARGE_INTEGER DestOffset;

} BACKUPRESTORE_PARAMETERS, *PBACKUPRESTORE_PARAMETERS;


// =========================================================================
// Local to this file...

DLG_BACKUPRESTORE G_dlgBackupRestore_DlgMode;
BACKUPRESTORE_PARAMETERS G_dlgBackupRestore_Params;

// =========================================================================
// Forward declarations...
BOOL CALLBACK dlgBackupRestore_Proc(
                           HWND hDlg, 
                           UINT message, 
                           WPARAM wParam, 
                           LPARAM lParam
                          );

void _DisplayDlgBackupRestore(HWND hWnd);


// =========================================================================
void DisplayDlgBackup(HWND hWnd)
{
	G_dlgBackupRestore_DlgMode = DLGMODE_BACKUPRESTORE_BACKUP;
	_DisplayDlgBackupRestore(hWnd);
}


// =========================================================================
void DisplayDlgRestore(HWND hWnd)
{
	G_dlgBackupRestore_DlgMode = DLGMODE_BACKUPRESTORE_RESTORE;
	_DisplayDlgBackupRestore(hWnd);
}


// =========================================================================
void _DisplayDlgBackupRestore(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_DisplayDlgBackupRestore\n")));
    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Displaying \"Backup/restore...\" dialog\n")));

    DialogBox(
              G_hInstance, 
              MAKEINTRESOURCE(IDD_BACKUPRESTORE), 
              hWnd, 
              (DLGPROC)dlgBackupRestore_Proc
             );

	DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_DisplayDlgBackupRestore\n")));
}                


// =========================================================================
BOOL CALLBACK dlgBackupRestore_HandleMsg_WM_INITDIALOG(HWND hDlg)
{
    SHINITDLGINFO shidi;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgBackupRestore_HandleMsg_WM_INITDIALOG\n")));

    shidi.dwMask = SHIDIM_FLAGS;
    shidi.dwFlags = (
                     SHIDIF_DONEBUTTON | 
                     SHIDIF_SIPDOWN | 
                     SHIDIF_SIZEDLGFULLSCREEN
                    );
    shidi.hDlg = hDlg;
    SHInitDialog(&shidi);

    G_dlgBackupRestore_MenuBar = SetupMenu_Simple(hDlg, IDR_MENU_DONECANCEL);

    // Populate display...
	if (G_dlgBackupRestore_DlgMode == DLGMODE_BACKUPRESTORE_BACKUP)
		{
		SetDlgItemText(hDlg, IDC_GROUP_SRC, _("Volume details"));
        SetStaticText(hDlg, IDC_STATIC_SRCFILENAME, _("&Volume:"));

		SetDlgItemText(hDlg, IDC_GROUP_DEST, _("Backup details"));
        SetStaticText(hDlg, IDC_STATIC_DESTFILENAME, _("Backup:"));

		SetControlEnabled(hDlg, IDC_STATIC_DESTOFFSET, FALSE);
		SetControlEnabled(hDlg, IDC_EDIT_DESTOFFSET, FALSE);
		SetControlEnabled(hDlg, IDC_SPIN_DESTOFFSET, FALSE);
		SetControlEnabled(hDlg, IDC_STATIC_DESTOFFSETBYTES, FALSE);
        }
	else
		{
		SetDlgItemText(hDlg, IDC_GROUP_SRC, _("Backup details"));
        SetStaticText(hDlg, IDC_STATIC_SRCFILENAME, _("Backup:"));

        SetDlgItemText(hDlg, IDC_GROUP_DEST, _("Volume details"));
        SetStaticText(hDlg, IDC_STATIC_DESTFILENAME, _("&Volume:"));

        SetControlEnabled(hDlg, IDC_STATIC_SRCOFFSET, FALSE);
		SetControlEnabled(hDlg, IDC_EDIT_SRCOFFSET, FALSE);
		SetControlEnabled(hDlg, IDC_SPIN_SRCOFFSET, FALSE);
		SetControlEnabled(hDlg, IDC_STATIC_SRCOFFSETBYTES, FALSE);
		}

	SetDlgItemInt(hDlg, IDC_EDIT_SRCOFFSET, 0, FALSE);
	SetDlgItemInt(hDlg, IDC_EDIT_DESTOFFSET, 0, FALSE);

    SDUi18n_TranslateWindow(hDlg);
    SDUi18n_TranslateCommandBar(G_dlgBackupRestore_MenuBar);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgBackupRestore_HandleMsg_WM_INITDIALOG\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgBackupRestore_HandleMsg_WM_DESTROY(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgBackupRestore_HandleMsg_WM_DESTROY\n")));

    SDUi18n_FreeOriginalStrings(hWnd);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgBackupRestore_HandleMsg_WM_DESTROY\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgBackupRestore_HandleMsg_WM_CLOSE(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgBackupRestore_HandleMsg_WM_CLOSE\n")));
    if (G_dlgBackupRestore_MenuBar != NULL)
        {
        DestroyWindow(G_dlgBackupRestore_MenuBar);
        G_dlgBackupRestore_MenuBar = NULL;
        }
    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgBackupRestore_HandleMsg_WM_CLOSE\n")));
    return TRUE;
}


// =========================================================================
void
dlgBackupRestore_GetParams(HWND hDlg)
{
	// Source file details...
    GetDlgItemText( 
                   hDlg, 
                   IDC_EDIT_SRCFILENAME,
                   G_dlgBackupRestore_Params.SrcFilename,
                   sizeof(G_dlgBackupRestore_Params.SrcFilename)
                 ); 
    G_dlgBackupRestore_Params.SrcOffset.QuadPart = GetDlgItemInt(
                                              hDlg,
                                              IDC_EDIT_SRCOFFSET,
                                              NULL,
                                              FALSE
                                             );

	// Destination file details...
	GetDlgItemText( 
                   hDlg, 
                   IDC_EDIT_DESTFILENAME,
                   G_dlgBackupRestore_Params.DestFilename,
                   sizeof(G_dlgBackupRestore_Params.DestFilename)
                 ); 
    G_dlgBackupRestore_Params.DestOffset.QuadPart = GetDlgItemInt(
                                              hDlg,
                                              IDC_EDIT_DESTOFFSET,
                                              NULL,
                                              FALSE
                                             );
}


// =========================================================================
BOOL
dlgBackupRestore_ValidateInput(HWND hDlg)
{
	BOOL retval;
    BOOL fileExists;
    WCHAR* volFilename;
    LARGE_INTEGER volOffset;
    LARGE_INTEGER volFilesize;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgBackupRestore_ValidateInput\n")));

	retval = TRUE;

	dlgBackupRestore_GetParams(hDlg);

	if (retval)
	    {
        retval = SDUCheckFileExists(G_dlgBackupRestore_Params.SrcFilename);

        if (!(retval))
            {
            if (G_dlgBackupRestore_DlgMode == DLGMODE_BACKUPRESTORE_BACKUP)
                {
                MsgError(hDlg, _("Volume file to backup from does not exist"));
                }
            else
                {
                MsgError(hDlg, _("CDB backup file does not exist"));
                }
            }
        }

	if (retval)
	    {
        fileExists = SDUCheckFileExists(G_dlgBackupRestore_Params.DestFilename);

        if (
            (G_dlgBackupRestore_DlgMode == DLGMODE_BACKUPRESTORE_BACKUP) &&
            (fileExists)
           )
            {
            MsgError(hDlg, _("CDB backup file already exists; please specify a different filename to backup to"));
            retval = FALSE;
            }
        else if (
                 (G_dlgBackupRestore_DlgMode == DLGMODE_BACKUPRESTORE_RESTORE) &&
                 (!(fileExists))
                )
            {
            MsgError(hDlg, _("Volume file to restore to does not exist"));
            retval = FALSE;
            }
        }

    if (retval)
        {
        if (G_dlgBackupRestore_DlgMode == DLGMODE_BACKUPRESTORE_BACKUP)
            {
            volFilename = G_dlgBackupRestore_Params.SrcFilename;
            volOffset = G_dlgBackupRestore_Params.SrcOffset;
            }
        else
            {
            volFilename = G_dlgBackupRestore_Params.DestFilename;
            volOffset = G_dlgBackupRestore_Params.DestOffset;
            }

        retval = GetFileSize_Filename(
                               volFilename,
                               &volFilesize
                              );
        if (!(retval))
            {
            MsgError(hDlg, _("Unable to get file size of volume file"));
            }
        else
            {
            if ((volOffset.QuadPart + (CRITICAL_DATA_LENGTH / 8)) >
                volFilesize.QuadPart)
                {
                MsgError(hDlg, _("Volume file too small to contain CDB"));
                retval = FALSE;
                }

            }
        }


	G_dlgBackupRestore_Params.ParamsOK = retval;

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgBackupRestore_ValidateInput\n")));
	return G_dlgBackupRestore_Params.ParamsOK;
}


// =========================================================================
BOOL CALLBACK dlgBackupRestore_HandleMsg_WM_COMMAND(HWND hDlg, WPARAM wParam)
{
    BOOL retval = FALSE;
	int ctrlID;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgBackupRestore_HandleMsg_WM_COMMAND\n")));
    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: %d\n"), LOWORD(wParam)));

    ctrlID = LOWORD(wParam);

    switch(ctrlID)
        {
        case IDOK:
			{
			BOOL allOK;

			if (dlgBackupRestore_ValidateInput(hDlg))
				{
				if (G_dlgBackupRestore_DlgMode == DLGMODE_BACKUPRESTORE_BACKUP)
					{
					allOK = driver_BackupCDB(
											 G_dlgBackupRestore_Params.SrcFilename,
											 G_dlgBackupRestore_Params.SrcOffset,
											 G_dlgBackupRestore_Params.DestFilename
											);
					}
				else
					{
					allOK = driver_RestoreCDB(
											 G_dlgBackupRestore_Params.SrcFilename,
											 G_dlgBackupRestore_Params.DestFilename,
											 G_dlgBackupRestore_Params.DestOffset
											);
					}

				if (allOK)
					{
					if (G_dlgBackupRestore_DlgMode == DLGMODE_BACKUPRESTORE_BACKUP)
						{
						MsgInfo(hDlg, _("Backup operation completed successfully."));
						}
					else
						{
						MsgInfo(hDlg, _("Restore operation completed successfully."));
						}

					EndDialog(hDlg, TRUE);
					}
				else
					{
					if (G_dlgBackupRestore_DlgMode == DLGMODE_BACKUPRESTORE_BACKUP)
						{
						MsgInfo(hDlg, _("Backup operation failed."));
						}
					else
						{
						MsgInfo(hDlg, _("Restore operation failed."));
						}

					}
				}

            retval = TRUE;
            break;
            }

        case IDCANCEL:
            {
            EndDialog(hDlg, TRUE);
            retval = TRUE;
            break;
            }

        case IDC_BUTTON_BROWSESRCFILE:
        case IDC_BUTTON_BROWSEDESTFILE:
            {
            OPENFILENAME ofn;
            DWORD flags;
            WCHAR filename[FREEOTFE_MAX_FILENAME_LENGTH];
            WCHAR* useFilter;
			BOOL fileDlgOK;

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

            if (
				(
				 (ctrlID == IDC_BUTTON_BROWSESRCFILE) &&
				 (G_dlgBackupRestore_DlgMode == DLGMODE_BACKUPRESTORE_RESTORE)
				) || 
				(
				 (ctrlID == IDC_BUTTON_BROWSEDESTFILE) &&
				 (G_dlgBackupRestore_DlgMode == DLGMODE_BACKUPRESTORE_BACKUP)
				)
			   )
                {
                useFilter          = FileFilterGenerate(FILE_FILTER_FLT_CDBBACKUPS);
                ofn.nMaxCustFilter = FILE_FILTER_CNT_CDBBACKUPS;
                ofn.nFilterIndex   = FILE_FILTER_DFLT_CDBBACKUPS;
                }
            else
                {
                useFilter          = FileFilterGenerate(FILE_FILTER_FLT_VOLUMESANDKEYFILES);
                ofn.nMaxCustFilter = FILE_FILTER_CNT_VOLUMESANDKEYFILES;
                ofn.nFilterIndex   = FILE_FILTER_DFLT_VOLUMESANDKEYFILES;
                }

            ofn.lpstrFilter = useFilter;

            if (
				(ctrlID == IDC_BUTTON_BROWSEDESTFILE) &&
				(G_dlgBackupRestore_DlgMode == DLGMODE_BACKUPRESTORE_BACKUP)
			   )
				{
				fileDlgOK = GetSaveFileName(&ofn);
				}
			else
				{
				fileDlgOK = GetOpenFileName(&ofn);
				}

			if (fileDlgOK)
                {
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Open file dialog OK'd...\n")));
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Filename: %ls\n"), filename));
                if (ctrlID == IDC_BUTTON_BROWSESRCFILE)
                    {
                    SetDlgItemText(hDlg, IDC_EDIT_SRCFILENAME, filename);
                    }
                else
                    {
                    SetDlgItemText(hDlg, IDC_EDIT_DESTFILENAME, filename);
                    }

                }
            else
                {
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Open file dialog cancelled\n")));
                }

            FileFilterFree(useFilter);

            break;
            }

        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgBackupRestore_HandleMsg_WM_COMMAND\n")));
    return retval;
}


// =========================================================================
BOOL CALLBACK dlgBackupRestore_HandleMsg_WM_NOTIFY(HWND hDlg, WPARAM wParam, LPARAM lParam)
{
    BOOL retval = FALSE;
    int idCtrl;
    NMHDR* notifHdr;
    NMUPDOWN* lpnmud;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgBackupRestore_HandleMsg_WM_NOTIFY\n")));

    idCtrl = (int)wParam;
    notifHdr = (NMHDR*)lParam;

    switch (notifHdr->code)
        {
        // Spin edit up/down button
        case UDN_DELTAPOS:
            {
            switch(idCtrl)
                {
                case IDC_SPIN_SRCOFFSET:
                    {
                    lpnmud = (NMUPDOWN*)lParam;
                    AdjustSpinControl(
                                      hDlg, 
                                      IDC_EDIT_SRCOFFSET, 
                                      0, 
                                      INT_MAX, 
                                      -(lpnmud->iDelta)
                                     );
                    retval = TRUE;
                    break;
                    }

                case IDC_SPIN_DESTOFFSET:
                    {
                    lpnmud = (NMUPDOWN*)lParam;
                    AdjustSpinControl(
                                      hDlg, 
                                      IDC_EDIT_DESTOFFSET, 
                                      0, 
                                      INT_MAX, 
                                      -(lpnmud->iDelta)
                                     );
                    retval = TRUE;
                    break;
                    }

                }
            break;
            }

        }


    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgBackupRestore_HandleMsg_WM_NOTIFY\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgBackupRestore_Proc(
                           HWND hDlg, 
                           UINT msg, 
                           WPARAM wParam, 
                           LPARAM lParam
                          )
{
    BOOL retval = FALSE;

    switch(msg)
        {
        case WM_NOTIFY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_NOTIFY\n")));
            retval = dlgBackupRestore_HandleMsg_WM_NOTIFY(hDlg, wParam, lParam);
            break;
            }

        case WM_INITDIALOG:
            {
            retval = dlgBackupRestore_HandleMsg_WM_INITDIALOG(hDlg);
            break;
            }

        case WM_DESTROY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_DESTROY\n")));
            retval = dlgBackupRestore_HandleMsg_WM_DESTROY(hDlg);
            break;
            }

        case WM_CLOSE:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_CLOSE\n")));
            retval = dlgBackupRestore_HandleMsg_WM_CLOSE(hDlg);
            break;
            }

        case WM_COMMAND:
            {
            retval = dlgBackupRestore_HandleMsg_WM_COMMAND(hDlg, wParam);
            break;
            }

        }

    return retval;
}


// =========================================================================
// =========================================================================
