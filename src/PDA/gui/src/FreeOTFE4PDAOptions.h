// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFE4PDAOptions_H
#define _FreeOTFE4PDAOptions_H   1

#include "FreeOTFE4PDADriverList.h"

// =========================================================================
// Structures...

typedef struct _OPTIONS {
    int ConfigVersion;

    BOOL MntPntUseVolFilename;
    WCHAR* MntPntDefault;

    WCHAR* ExplorerExe;
    BOOL ExploreOnMount;

    BOOL LargeIcons;
    BOOL SoftkeyMenus;
    BOOL RevertVolTimestamps;

    BOOL SaveSettings;

    WCHAR* LanguageCode;

    LNKLIST_DISABLED_DRIVER* DisabledHashes;
    LNKLIST_DISABLED_DRIVER* DisabledCyphers;

} OPTIONS, *POPTIONS;


// =========================================================================
// Functions...

OPTIONS* OptionsRead();
BOOL OptionsWrite(OPTIONS* Opts);
void OptionsFree(OPTIONS* Opts);
OPTIONS* OptionsCopy(OPTIONS* SrcOpts);

// =========================================================================
// =========================================================================

#endif

