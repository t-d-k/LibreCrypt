
#include "SDUi18n.h"

#include <stdio.h>
#include <winnls.h> // Required for MultiByteToWideChar(...) (WinCE)

#include "SDUGeneral.h"
#include "SDULinkedList.h"


// =========================================================================
// Constants...

// Must be large enough to store full path and filename to translation file
#define BUFF_PATH_AND_FILENAME 2048
const char DEFAULT_MO_FILENAME[]                   = "default.mo";
const WCHAR DEFAULT_MO_FILENAMEW[]                 = TEXT("default.mo");
const char  SDUI18N_TRANSLATION_DIR_LOCALE[]       = "locale";
const WCHAR SDUI18N_TRANSLATION_DIR_LOCALEW[]      = TEXT("locale");
const char  SDUI18N_TRANSLATION_DIR_LC_MESSAGES[]  = "LC_MESSAGES";
const WCHAR SDUI18N_TRANSLATION_DIR_LC_MESSAGESW[] = TEXT("LC_MESSAGES");

const char  SDUI18N_ISO639_ALPHA2_ENGLISH[]  = "en";
const WCHAR SDUI18N_ISO639_ALPHA2_ENGLISHW[] = TEXT("en");

// Metadata within .po/.mo files stored as the translation of an 
// empty string
const char  SDUI18N_TRANSLATION_METADATA[]  = "";
const WCHAR SDUI18N_TRANSLATION_METADATAW[] = TEXT("");
// Metadata name/value pairs separated by newlines
const char  SDUI18N_TRANSLATION_METADATA_SEPNVPAIR[]  = "\n";
const WCHAR SDUI18N_TRANSLATION_METADATA_SEPNVPAIRW[] = TEXT("\n");
// Metadata name:value pairs separated by ":"
const char  SDUI18N_TRANSLATION_METADATA_SEPNV[]  = ":";
const WCHAR SDUI18N_TRANSLATION_METADATAW_SEPNV[] = TEXT(":");

// IMPORTANT: DON'T CHANGE "ENGLISH" TO A #define!
// The word "English" on the next line ***MUST*** be literal - NOT TRANSLATABLE!
const WCHAR SDUI18N_ENGLISH_LANG_TEXT[] = TEXT("English");
// IMPORTANT: DON'T CHANGE "ENGLISH" TO A #define!
// The word "English" is only here to get the word "English" into translation files
#define ENGLISH_TRANSLATABLE _("English")

// Number of chars in metadata
// This should be big enough to cover all
#define MAX_METADATA_LENGTH 4096

// Used to retrieve metadata for the specified language code
BOOL _SDUi18n_GetMetadata_ForCodeW(
    const WCHAR* code, 
    WCHAR* MetadataBuffer,
    int MetadataBufferSize  // In chars
);

// =========================================================================
// Structures...

typedef struct _MO_HEADER {
    unsigned int Magic;
    unsigned int FileFormatRevision;
    unsigned int NumberOfStrings;
    unsigned int OffsetOriginal;
    unsigned int OffsetTranslated;
    unsigned int HashTableSize;
    unsigned int HashTableOffset;
} MO_HEADER, *PMO_HEADER;

typedef struct _MO_DETAILS {
    char* LangCode;
    WCHAR* LangCodeW;

    MO_HEADER Header;   

    int ContentSize;
    byte* Content;
    WCHAR* ContentW;
  
} MO_DETAILS, *PMO_DETAILS;

struct _SDUi18n_NAME_VALUE_PAIRW {
    WCHAR* name;
    WCHAR* value;
};
typedef struct _SDUi18n_NAME_VALUE_PAIRW SDUi18n_NAME_VALUE_PAIRW;

// =========================================================================
// Globals...

static MO_DETAILS SDUi18n_UseDetails = {NULL, NULL, {0, 0, 0, 0, 0, 0, 0}, 0, NULL};


// =========================================================================
// Forward declarations...

void _SDUi18n_Init(MO_DETAILS* moDetails);
void _SDUi18n_Deinit(MO_DETAILS* moDetails);
BOOL _SDUi18n_Load_Code(MO_DETAILS* moDetails, const char* code);
BOOL _SDUi18n_Load_CodeW(MO_DETAILS* moDetails, const WCHAR* code);
BOOL _SDUi18n_Load_File(MO_DETAILS* moDetails, const char* filename);
BOOL _SDUi18n_Load_FileW(MO_DETAILS* moDetails, const WCHAR* filename);
BOOL _SDUi18n_Load_FileForCode(
    MO_DETAILS* moDetails, 
    const char* code, 
    const char* filename
);
BOOL _SDUi18n_Load_FileForCodeW(
    MO_DETAILS* moDetails, 
    const WCHAR* code, 
    const WCHAR* filename
);
BOOL _SDUi18n_Load_FileHandle(MO_DETAILS* moDetails, FILE* hFile);
void _SDUi18n_Unload(MO_DETAILS* moDetails);
int _SDUi18n_GetIdxOfText(const MO_DETAILS* moDetails, const char* text);
int _SDUi18n_GetIdxOfTextW(const MO_DETAILS* moDetails, const WCHAR* text);
const char* _SDUi18n_GetText(const MO_DETAILS* moDetails, const char* text);
const char* _SDUi18n_GetText_n(const MO_DETAILS* moDetails, const char* text, const int plural);
const WCHAR* _SDUi18n_GetTextW(const MO_DETAILS* moDetails, const WCHAR* text);
const WCHAR* _SDUi18n_GetTextW_n(const MO_DETAILS* moDetails, const WCHAR* text, const int plural);

// Get pointer to original text by index
// Returns: Pointer to text, or NULL if not valid
char* _GetText_Original(const MO_DETAILS* moDetails, const int idx, int* length, char** text);
WCHAR* _GetText_OriginalW(const MO_DETAILS* moDetails, const int idx, int* length, WCHAR** text);

// Get pointer to translated text by index
// Returns: Pointer to text, or NULL if not valid
char* _GetText_Translated(const MO_DETAILS* moDetails, const int idx, int* length, char** text);
WCHAR* _GetText_TranslatedW(const MO_DETAILS* moDetails, const int idx, int* length, WCHAR** text);

void _SDUi18n_ConvertToW(MO_DETAILS* moDetails);

// Returns NULL on failure
const WCHAR* _SDUi18n_GetMetadataW(const MO_DETAILS* moDetails);

// As _SDUi18n_GetTextW, but allow retrieval of ""
const WCHAR* _SDUi18n_GetText_AllowMetadataW(const MO_DETAILS* moDetails, const WCHAR* text);

// Metadata parsing related...
LNKLIST_ITEM* _SDUi18n_ParseMetadata(WCHAR* metadataBlock);
WCHAR* _SDUi18n_GetMetaDataValueW(LNKLIST_ITEM* metaHead, WCHAR* name);
void _SDUi18n_FreeParsedMetadata(LNKLIST_ITEM** metaHead);

// Copy just the translators name from one buffer to another
void _CopyTranslatorNameW(
    WCHAR* Translator,  // Copy from here
    WCHAR* TranslatorName  // Copy to here
);

// =========================================================================

// =========================================================================
// Initialize i18n system
void SDUi18n_Init()
{
  _SDUi18n_Init(&SDUi18n_UseDetails);
}

void _SDUi18n_Init(MO_DETAILS* moDetails)
{
    memset(moDetails, 0, sizeof(moDetails));

    moDetails->LangCode = NULL;
    moDetails->LangCodeW = NULL;
    memset(&(moDetails->Header), 0, sizeof(moDetails->Header));
    moDetails->ContentSize = 0;
    moDetails->Content = NULL;
    moDetails->ContentW = NULL;

}


// =========================================================================
// Deinitialize i18n system
void SDUi18n_Deinit()
{
    _SDUi18n_Deinit(&SDUi18n_UseDetails);
}

void _SDUi18n_Deinit(MO_DETAILS* moDetails)
{
    _SDUi18n_Unload(moDetails);
}


// =========================================================================
// Populate the structure passed in with the details for the translation 
// indicated by LanguageCode
BOOL SDUi18n_GetTranslationDetailsW(
    const WCHAR* LanguageCode,
    SDUI18N_LANGUAGEW* TranslationDetails
)
{
    BOOL retval;
    WCHAR translationFile[BUFF_PATH_AND_FILENAME];
    WCHAR languageName[MAX_LANG_NAME];
    WCHAR metadataBlock[MAX_METADATA_LENGTH];
    LNKLIST_ITEM* parsedMetadata;
    WCHAR* tmp;

    retval = FALSE;

    // Initialize structure passed in, in case some members can't be 
    // retrieved
    memset(TranslationDetails, 0, sizeof(*TranslationDetails));

    // Check for "default.mo" file...
    SDUCpyExeDir(translationFile);
    wcscat(translationFile, TEXT("\\"));
    wcscat(translationFile, SDUI18N_TRANSLATION_DIR_LOCALEW);
    wcscat(translationFile, TEXT("\\"));
    wcscat(translationFile, LanguageCode);
    wcscat(translationFile, TEXT("\\"));
    wcscat(translationFile, SDUI18N_TRANSLATION_DIR_LC_MESSAGESW);
    wcscat(translationFile, TEXT("\\"));                
    wcscat(translationFile, DEFAULT_MO_FILENAMEW);
    if (SDUCheckFileExists(translationFile))
        {
        // Sanity - we can fit the code into out buffer?
        // "<" due to terminating NULL
        if (wcslen(LanguageCode) < 
                (sizeof(TranslationDetails->LanguageCode) / sizeof((TranslationDetails->LanguageCode)[0])))
            {
            wcscpy(TranslationDetails->LanguageCode, LanguageCode);

            // Fallback in case of problem...
            wcscpy(TranslationDetails->LanguageName, TranslationDetails->LanguageCode);

            if (SDUi18n_GetText_ForCodeW(
                   TranslationDetails->LanguageCode,
                   SDUI18N_ENGLISH_LANG_TEXT,
                   languageName,
                   (sizeof(languageName) / sizeof(languageName[0]))
                   ))
                {
                wcscpy(TranslationDetails->LanguageName, languageName);
                }

            if (_SDUi18n_GetMetadata_ForCodeW(
                   LanguageCode,
                   metadataBlock,
                   (sizeof(metadataBlock) / sizeof(metadataBlock[0]))
                   ))
                {
                parsedMetadata = _SDUi18n_ParseMetadata((WCHAR*)metadataBlock);

                if (parsedMetadata != NULL)
                    {
                    tmp = _SDUi18n_GetMetaDataValueW(parsedMetadata, TEXT("Last-Translator"));
                    if (tmp != NULL)
                        {
                        wcscpy(TranslationDetails->Translator, tmp);
                        _CopyTranslatorNameW(
                            TranslationDetails->Translator,
                            TranslationDetails->TranslatorName
                            );
                        }

                    _SDUi18n_FreeParsedMetadata(&parsedMetadata);
                    }

                }


            retval = TRUE;
            }
        }

    return retval;
}


// =========================================================================
// Copy just the translators name from one buffer to another
void _CopyTranslatorNameW(
    WCHAR* Translator,  // Copy from here
    WCHAR* TranslatorName  // Copy to here
)
{
    if (
        (wcsstr(Translator, TEXT("<")) > Translator) &&
        (wcsstr(Translator, TEXT("@")) > wcsstr(Translator, TEXT("<"))) && // Sanity, in case of "<Berty>" - not an email addr
        (wcsstr(Translator, TEXT(">")) > wcsstr(Translator, TEXT("@")))    // Sanity, in case of "<Berty>" - not an email addr
       )
        {
        // Trivial version; only handles stuff like "Fred <bert@domain.com>"
        // Really should be able to handle "<ME!> <myaddr@domain.com"
        wcsncpy(TranslatorName, Translator, (wcsstr(Translator, TEXT("<")) - Translator));
        }
    else
        {
        wcscpy(TranslatorName, Translator);
        }
    
}


// =========================================================================
// Populate the link list supplied as "TranslationsList", setting the linked
// list's items to pointers to "LANGUAGEW" structures
// Note: It is the *callers* responsibility to free off the
//       "TranslationsList"'s items
// If "TranslationsList" is set to NULL, just return a count of the 
// translations found
// Returns the number of translations found
int SDUi18n_GetTranslationsW(
    LNKLIST_ITEM** TranslationsList
)
{
    int retval;
    WCHAR searchPatt[BUFF_PATH_AND_FILENAME];
    WIN32_FIND_DATA findRec;
    HANDLE hFind;
    BOOL fileFound;
    SDUI18N_LANGUAGEW* lang;


    retval = 0;

    // We search only the "locale" subdir
    SDUCpyExeDir(searchPatt);
    wcscat(searchPatt, TEXT("\\"));
    wcscat(searchPatt, SDUI18N_TRANSLATION_DIR_LOCALEW);
    wcscat(searchPatt, TEXT("\\*"));

    hFind = FindFirstFile(searchPatt, &findRec);
    if (hFind != INVALID_HANDLE_VALUE)
        {
        fileFound = TRUE;
        while (fileFound)
            {
            // If it's a directory, check further...
            if ((findRec.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0)
                {
                // Assume the subdir holds only translations; process
                lang = malloc(sizeof(*lang));
                if (lang == NULL)
                    {
                    // Um... Oh well!
                    // Ditch *everything*
                    if (TranslationsList != NULL)
                        {
                        SDULLRemoveAllItems(TranslationsList, TRUE);
                        }
                    retval = 0;
                    break;
                    }
                else
                    {
                    if (SDUi18n_GetTranslationDetailsW(findRec.cFileName, lang))
                        {
                        if (TranslationsList == NULL)
                            {
                            free(lang);
                            }
                        else
                            {
                            SDULLAddItem(TranslationsList, lang);
                            }
                        retval++;
                        }
                    else
                        {
                        // Ditch current
                        free(lang);
                        }

                    }
                }

            fileFound = FindNextFile(hFind, &findRec);
            }
        }


    return retval;
}


// =========================================================================
// Load translation
BOOL SDUi18n_Load_Code(const char* code)
{
    return _SDUi18n_Load_Code(&SDUi18n_UseDetails,  code);
}

BOOL SDUi18n_Load_CodeW(const WCHAR* code)
{
    return _SDUi18n_Load_CodeW(&SDUi18n_UseDetails, code);
}

BOOL _SDUi18n_Load_Code(MO_DETAILS* moDetails, const char* code)
{
    return _SDUi18n_Load_FileForCode(moDetails, code, DEFAULT_MO_FILENAME);
}

BOOL _SDUi18n_Load_CodeW(MO_DETAILS* moDetails, const WCHAR* code)
{
    return _SDUi18n_Load_FileForCodeW(moDetails, code, DEFAULT_MO_FILENAMEW);
}


// =========================================================================
// Load translation
BOOL SDUi18n_Load_FileForCode(const char* code, const char* filename)
{
    return _SDUi18n_Load_FileForCode(&SDUi18n_UseDetails, code, filename);
}

BOOL SDUi18n_Load_FileForCodeW(const WCHAR* code, const WCHAR* filename)
{
    return _SDUi18n_Load_FileForCodeW(&SDUi18n_UseDetails, code, filename);
}

BOOL _SDUi18n_Load_FileForCode(
    MO_DETAILS* moDetails, 
    const char* code, 
    const char* filename
)
{
//lplp - todo
return FALSE;
}

BOOL _SDUi18n_Load_FileForCodeW(
    MO_DETAILS* moDetails, 
    const WCHAR* code, 
    const WCHAR* filename
)
{
    BOOL retval = FALSE;
    WCHAR exePath[BUFF_PATH_AND_FILENAME];
    WCHAR useFilename[BUFF_PATH_AND_FILENAME];

    SDUCpyExeDir(exePath);

    moDetails->LangCodeW = calloc(wcslen(code), sizeof(*(moDetails->LangCodeW)));
    if (moDetails->LangCodeW != NULL)
        {
        wcscpy(moDetails->LangCodeW, code);

        wsprintf(
                 useFilename, 
                 TEXT("%s\\%s\\%s\\%s\\%s"), 
                 exePath, 
                 SDUI18N_TRANSLATION_DIR_LOCALEW, 
                 code, 
                 SDUI18N_TRANSLATION_DIR_LC_MESSAGESW,
                 filename
                );

        retval = _SDUi18n_Load_FileW(moDetails, useFilename);
        }

    return retval;
}


// =========================================================================
BOOL SDUi18n_Load_File(const char* filename)
{
    return _SDUi18n_Load_File(&SDUi18n_UseDetails, filename);
}


BOOL _SDUi18n_Load_File(MO_DETAILS* moDetails, const char* filename)
{
    BOOL retval;
    FILE* hFile;

    retval = FALSE;

    hFile = fopen(filename, "rb");
    if (hFile != NULL)
        {
        retval = _SDUi18n_Load_FileHandle(moDetails, hFile);
        fclose(hFile);
        }  

    return retval;
}

BOOL SDUi18n_Load_FileW(const WCHAR* filename)
{
    return _SDUi18n_Load_FileW(&SDUi18n_UseDetails, filename);
}

BOOL _SDUi18n_Load_FileW(MO_DETAILS* moDetails, const WCHAR* filename)
{
    BOOL retval;
    FILE* hFile;

    retval = FALSE;

    hFile = _wfopen(filename, TEXT("rb"));
    if (hFile != NULL)
        {
        retval = _SDUi18n_Load_FileHandle(moDetails, hFile);
        fclose(hFile);
        }  

    return retval;
}


BOOL _SDUi18n_Load_FileHandle(MO_DETAILS* moDetails, FILE* hFile)
{
    BOOL retval;
    int contentSizeW;

    retval = FALSE;

    if (fread(
            &(moDetails->Header),
            sizeof(moDetails->Header), 
            1,
            hFile
            ) == 1)  // One element of size sizeof(moDetails->Header) read
        {
        if (
            (moDetails->Header.Magic == 0x950412de) ||
            (moDetails->Header.Magic == 0xde120495) 
            )
            { 
            fseek(hFile, 0, SEEK_END);
            moDetails->ContentSize = ftell(hFile);

            moDetails->Content = malloc(moDetails->ContentSize);
            if (moDetails->Content != NULL) 
                {        
                fseek(hFile, 0, SEEK_SET);

                if (fread(
                        moDetails->Content,
                        moDetails->ContentSize,
                        1,
                        hFile
                        ) == 1)  // One element of size moDetails->ContentSize read
                    {
                    contentSizeW = (moDetails->ContentSize * sizeof(WCHAR));
                    moDetails->ContentW = malloc(contentSizeW);
                    if (moDetails->ContentW != NULL)
                        {
                        memset(moDetails->ContentW, 0, contentSizeW);

                        _SDUi18n_ConvertToW(moDetails);

                        retval = TRUE;
                        }
                    }
                }

            }
        }


    if (!(retval))
        {
        _SDUi18n_Unload(moDetails);    
        }

    return retval;
}


// =========================================================================
// Unload any loaded translation
void SDUi18n_Unload()
{
    _SDUi18n_Unload(&SDUi18n_UseDetails);
}

void _SDUi18n_Unload(MO_DETAILS* moDetails)
{
    if (moDetails->LangCode != NULL)
        {
        free(moDetails->LangCode);
        moDetails->LangCode = NULL;
        }

    if (moDetails->LangCodeW != NULL)
        {
        free(moDetails->LangCodeW);
        moDetails->LangCodeW = NULL;
        }

    if (moDetails->Content != NULL)
        {
        free(moDetails->Content);
        moDetails->Content = NULL;
        }

    if (moDetails->ContentW != NULL)
        {
        free(moDetails->ContentW);
        moDetails->ContentW = NULL;
        }

    moDetails->ContentSize = 0;

}


// =========================================================================
// Get pointer to original text by index
// Returns: Pointer to text, or NULL if not valid
char* _GetText_Original(const MO_DETAILS* moDetails, const int idx, int* length, char** text)
{
    int offset;
    char* retval;
    int* idxOffset;

    retval = NULL;

    idxOffset = (int*)(
                       moDetails->Content + 
                       moDetails->Header.OffsetOriginal + 
                       (idx * 8)
                      );
    if (length != NULL)
        {
        *length = *idxOffset;
        }

    offset = *(idxOffset + 1);
    retval = (char*)(moDetails->Content + offset);

    if (text != NULL) 
        {
        *text = retval;
        }

    return retval;
}


// =========================================================================
// Get pointer to original text by index
// Returns: Pointer to text, or NULL if not valid
WCHAR* _GetText_OriginalW(const MO_DETAILS* moDetails, const int idx, int* length, WCHAR** text)
{
    int offset;
    WCHAR* retval;
    int* idxOffset;

    retval = NULL;

    idxOffset = (int*)(
                       moDetails->Content + 
                       moDetails->Header.OffsetOriginal + 
                       (idx * 8)
                      );
    if (length != NULL)
        {
        *length = *idxOffset;
        }

    offset = *(idxOffset + 1);
    retval = (WCHAR*)(moDetails->ContentW + offset);

    if (text != NULL) 
        {
        *text = retval;
        }

    return retval;
}


// =========================================================================
// Get pointer to translated text by index
// Returns: Pointer to text, or NULL if not valid
  char* _GetText_Translated(const MO_DETAILS* moDetails, const int idx, int* length, char** text)
{
    int offset;
    char* retval;
    int* idxOffset;

    retval = NULL;

    idxOffset = (int*)(
                       moDetails->Content + 
                       moDetails->Header.OffsetTranslated + 
                       (idx * 8)
                      );
    if (length != NULL)
        {
        *length = *idxOffset;
        }

    offset = *(idxOffset + 1);
    retval = (char*)(moDetails->Content + offset);

    if (text != NULL) 
        {
        *text = retval;
        }

    return retval;
}


// =========================================================================
// Get pointer to translated text by index
// Returns: Pointer to text, or NULL if not valid
WCHAR* _GetText_TranslatedW(const MO_DETAILS* moDetails, const int idx, int* length, WCHAR** text)
{
    int offset;
    WCHAR* retval;
    int* idxOffset;

    retval = NULL;

    idxOffset = (int*)(
                       moDetails->Content + 
                       moDetails->Header.OffsetTranslated +
                       (idx * 8)
                      );
    if (length != NULL)
        {
        *length = *idxOffset;
        }

    offset = *(idxOffset + 1);
    retval = (WCHAR*)(moDetails->ContentW + offset);

    if (text != NULL) 
        {
        *text = retval;
        }

    return retval;
}


// =========================================================================
// Returns: Index of text string, or -1 if not found
int _SDUi18n_GetIdxOfText(const MO_DETAILS* moDetails, const char* text)
{
    unsigned int i;
    int retval;

    retval = -1;

    for (i = 0; i < moDetails->Header.NumberOfStrings; i++)
        {
        if (strcmp(
                    _GetText_Original(moDetails, i, NULL, NULL),
                    text
                    ) == 0)
            {
            retval = i;
            break;
            }

        }

    return retval;
}

// Returns: Index of text string, or -1 if not found
int _SDUi18n_GetIdxOfTextW(const MO_DETAILS* moDetails, const WCHAR* text)
{
    unsigned int i;
    int retval;

    retval = -1;

    for (i = 0; i < moDetails->Header.NumberOfStrings; i++)
        {
        if (wcscmp(
                    _GetText_OriginalW(moDetails, i, NULL, NULL),
                    text
                    ) == 0)
            {
            retval = i;
            break;
            }

        }

    return retval;
}


// =========================================================================
// Get translated text
// Returns: Pointer to translated text, or "text" passed in if either 
//          not found or not translated
const char* SDUi18n_GetText(const char* text)
{
    return  _SDUi18n_GetText(&SDUi18n_UseDetails, text);
}

const char* _SDUi18n_GetText(const MO_DETAILS* moDetails, const char* text)
{
    const char* retval;
    int idx;
    char* tmp;

    retval = text;

    idx = _SDUi18n_GetIdxOfText(moDetails, text);
    if (idx >= 0)
        {
        tmp = _GetText_Translated(moDetails, idx, NULL, NULL);
        if (tmp != NULL)
            {
            retval = tmp;
            }
        }

    return retval;
}


// =========================================================================
const char* SDUi18n_GetText_n(const char* text, const int plural)
{
    return  _SDUi18n_GetText_n(&SDUi18n_UseDetails, text, plural);
}

const char* _SDUi18n_GetText_n(const MO_DETAILS* moDetails, const char* text, const int plural)
{
//lplp
return text;
}


// =========================================================================
const WCHAR* SDUi18n_GetTextW(const WCHAR* text)
{
    return  _SDUi18n_GetTextW(&SDUi18n_UseDetails, text);
}

const WCHAR* _SDUi18n_GetTextW(const MO_DETAILS* moDetails, const WCHAR* text)
{
    const WCHAR* retval;

    retval = text;

    if (wcslen(text) > 0) 
        {
        retval = _SDUi18n_GetText_AllowMetadataW(moDetails, text);
        }

    return retval;
}

const WCHAR* _SDUi18n_GetMetadataW(const MO_DETAILS* moDetails)
{
    return _SDUi18n_GetText_AllowMetadataW(moDetails, SDUI18N_TRANSLATION_METADATAW);
}

const WCHAR* _SDUi18n_GetText_AllowMetadataW(const MO_DETAILS* moDetails, const WCHAR* text)
{
    const WCHAR* retval;
//  char* multibyteInput;
//  int multibyteInputLen;
//  char* multibyteOutput;
//  int multibyteOutputLen;
    int idx;
    WCHAR* tmpW;
//  WCHAR* tmpWTwo;
  
    retval = text;

    idx = _SDUi18n_GetIdxOfTextW(moDetails, text);
    if (idx >= 0)
        {
        tmpW = _GetText_TranslatedW(moDetails, idx, NULL, NULL);
        if (tmpW != NULL)
            {
            retval = tmpW;
            }
        }
    else
        {
/*
        // Convert WCHAR passed in to UTF-8, call GetText(...), the convert UTF-8 
        // returned back into WCHAR
      
  
        // +1 to include terminating NULL
        multibyteInputLen = (wcslen(text)+1) * sizeof(*multibyteInput);
        multibyteInput = malloc(multibyteInputLen);

        if (multibyteInput != NULL)
            {
            if (WideCharToMultiByte(
                              CP_UTF8,
                              0,
                              text,
                              -1,
                              multibyteInput,
                              multibyteInputLen,
                              NULL,
                              NULL
                            ) != 0)
                {
                idx = SDUi18n_GetIdxOfText(multibyteInput);
                if (idx >= 0)
                    {
                    multibyteOutput = _GetText_Translated(moDetails, idx, NULL, NULL);

                    // +1 to include terminating NULL
                    // "* 2" as worst case for UTF-8 to WCHAR conversion
                    multibyteOutputLen = ((strlen(multibyteOutput) + 1) * 2) * sizeof(*multibyteOutput);
                    tmpWTwo = malloc(multibyteOutputLen);
                    if (tmpWTwo != NULL)
                        {
                        if (MultiByteToWideChar(
                                  CP_UTF8,
                                  0,
                                  multibyteOutput,
                                  -1,
                                  tmpWTwo,
                                  multibyteOutputLen
                                 ) > 0)
                            {
                            tmpW = _GetText_OriginalW(moDetails, idx, NULL, NULL);
                            wcscpy(tmpW, text);
                            tmpW = _GetText_TranslatedW(moDetails, idx, NULL, NULL);
                            wcscpy(tmpW, tmpWTwo);
                            retval = tmpW;
                            }
                        free(tmpWTwo);
                        }
                    }
                }

            free(multibyteInput);
            }
*/
        }

    return retval;
}


// =========================================================================
const WCHAR* SDUi18n_GetTextW_n(const WCHAR* text, const int plural)
{
    return  _SDUi18n_GetTextW_n(&SDUi18n_UseDetails, text, plural);
}

const WCHAR* _SDUi18n_GetTextW_n(const MO_DETAILS* moDetails, const WCHAR* text, const int plural)
{
//lplp
    return NULL;
}


// =========================================================================
void _SDUi18n_ConvertToW(MO_DETAILS* moDetails)
{
    unsigned int i;
    char* originalA;
    char* translatedA;
    WCHAR* originalW;
    WCHAR* translatedW;
    int convertedLen;
    int origLen;
    int translatedLen;


    for (i = 0; i < moDetails->Header.NumberOfStrings; i++)
        {
        originalA = _GetText_Original(moDetails, i, &origLen, NULL);
        translatedA = _GetText_Translated(moDetails, i, &translatedLen, NULL);
        originalW = _GetText_OriginalW(moDetails, i, NULL, NULL);
        translatedW = _GetText_TranslatedW(moDetails, i, NULL, NULL);

        convertedLen = ((origLen + 1) * sizeof(WCHAR));

//        mbstowcs(
//                 originalW, 
//                 originalA,
//                 strlen(originalA)
//                ); 
        if (MultiByteToWideChar(
                                CP_UTF8,
                                0,
                                originalA,
                                -1,
                                originalW,
                                convertedLen
                                ) > 0)
            {
            convertedLen = ((translatedLen + 1) * sizeof(WCHAR));
//            mbstowcs(
//                     translatedW, 
//                     translatedA,
//                     strlen(translatedA)
//                    ); 
            MultiByteToWideChar(
                                CP_UTF8,
                                0,
                                translatedA,
                                -1,
                                translatedW,
                                convertedLen
                                );

            }
        }

}


// =========================================================================
BOOL SDUi18n_GetText_ForCode(
    const char* code, 
    const char* text,
    char* TranslationBuffer,
    int TranslationBufferSize  // In chars
)
{
    BOOL retval;
    const char* tmpTranslation;
    MO_DETAILS details;

    retval = FALSE;

    // Fallback, in case of any problem...
    strncpy(TranslationBuffer, text, TranslationBufferSize);

    _SDUi18n_Init(&details);

    if (_SDUi18n_Load_Code(&details, code))
        {
        tmpTranslation = _SDUi18n_GetText(&details, text);
        strncpy(TranslationBuffer, tmpTranslation, TranslationBufferSize);
        _SDUi18n_Unload(&details);

        retval = TRUE;
        }

    _SDUi18n_Deinit(&details);

    return retval;
}

// =========================================================================
BOOL SDUi18n_GetText_ForCodeW(
    const WCHAR* code, 
    const WCHAR* text,
    WCHAR* TranslationBuffer,
    int TranslationBufferSize  // In chars
)
{
    BOOL retval;
    const WCHAR* tmpTranslation;
    MO_DETAILS details;

    retval = FALSE;

    // Fallback, in case of any problem...
    wcsncpy(TranslationBuffer, text, TranslationBufferSize);

    _SDUi18n_Init(&details);

    if (_SDUi18n_Load_CodeW(&details, code))
        {
        tmpTranslation = _SDUi18n_GetTextW(&details, text);
        wcsncpy(TranslationBuffer, tmpTranslation, TranslationBufferSize);
        _SDUi18n_Unload(&details);

        retval = TRUE;
        }

    _SDUi18n_Deinit(&details);

    return retval;
}

// =========================================================================
BOOL _SDUi18n_GetMetadata_ForCodeW(
    const WCHAR* code, 
    WCHAR* MetadataBuffer,
    int MetadataBufferSize  // In chars
)
{
    BOOL retval;
    const WCHAR* tmpTranslation;
    MO_DETAILS details;

    retval = FALSE;

    // Fallback, in case of any problem...
    memset(MetadataBuffer, 0, (MetadataBufferSize * sizeof(*code)));

    _SDUi18n_Init(&details);

    if (_SDUi18n_Load_CodeW(&details, code))
        {
        tmpTranslation = _SDUi18n_GetMetadataW(&details);
        wcsncpy(MetadataBuffer, tmpTranslation, MetadataBufferSize);
        _SDUi18n_Unload(&details);

        retval = TRUE;
        }

    _SDUi18n_Deinit(&details);

    return retval;
}


// =========================================================================
WCHAR* _SDUi18n_GetMetaDataValueW(LNKLIST_ITEM* metaHead, WCHAR* name)
{
    WCHAR* retval;
    int i;
    SDUi18n_NAME_VALUE_PAIRW* nvPair;

    retval = NULL;

    for(i = 0; i < SDULLCountItems(metaHead); i++)
        {
        SDULLGetItem(metaHead, i, &nvPair);
        if (wcscmp(nvPair->name, name) == 0)
            {
            retval = nvPair->value;
            break;
            }
        }

    return retval;
}


// =========================================================================
// This will create a copy of the metadataBlock passed in
// Returns linked list of metadata, or NULL on failure
// Return value MUST be free'd off using _SDUi18n_FreeParsedMetadata(...)
LNKLIST_ITEM* _SDUi18n_ParseMetadata(WCHAR* metadataBlock)
{
    LNKLIST_ITEM* retval;
    WCHAR* copyMetaBlock;
    int copyMetaBlockSize;
    WCHAR* tok;
    SDUi18n_NAME_VALUE_PAIRW* nvPair;
    int i;


    retval = NULL;

    // First we make a copy of the metadata block passed in, as we modify it
    // (break it up using NULL chars)
    // +1 to include terminating NULL
    copyMetaBlockSize = ((wcslen(metadataBlock) + 1) * sizeof(*metadataBlock));
    copyMetaBlock = malloc(copyMetaBlockSize);
    wcscpy(copyMetaBlock, metadataBlock);

    tok = wcstok(copyMetaBlock, SDUI18N_TRANSLATION_METADATA_SEPNVPAIRW);
    while (tok != NULL)
        {
        nvPair = malloc(sizeof(*nvPair));

        // Note: "name" will be replaced with a copy later; for now, it's
        //       just a pointer into the copy
        nvPair->name = tok;
        nvPair->value = NULL;

        SDULLAddItem(&retval, nvPair);

        tok = wcstok(NULL, SDUI18N_TRANSLATION_METADATA_SEPNVPAIRW);
        }

    for(i = 0; i < SDULLCountItems(retval); i++)
        {
        SDULLGetItem(retval, i, &nvPair);

        tok = wcstok(nvPair->name, SDUI18N_TRANSLATION_METADATAW_SEPNV);
        // +1 to include terminating NULL
        nvPair->name = calloc((wcslen(tok) + 1), sizeof(*tok));
        wcscpy(nvPair->name, tok);

        // We use "\n" as this need to return all remaining
        tok = wcstok(NULL, SDUI18N_TRANSLATION_METADATA_SEPNVPAIRW);
        if (tok != NULL)
            {
            // Skip over any initial space (i.e. after ":")
            if (wcsncmp(tok, TEXT(" "), 1) == 0)
                {
                tok++;
                }

            // +1 to include terminating NULL
            nvPair->value = calloc((wcslen(tok) + 1), sizeof(*tok));
            wcscpy(nvPair->value, tok);
            }
        }

    free(copyMetaBlock);

    return retval;
}

// =========================================================================
// Free off structure created by _SDUi18n_ParseMetadata(...)
void _SDUi18n_FreeParsedMetadata(LNKLIST_ITEM** metaHead)
{
    int i;
    SDUi18n_NAME_VALUE_PAIRW* nvPair;
    
    if (metaHead != NULL) 
        {
        for(i = 0; i < SDULLCountItems(*metaHead); i++)
            {
            SDULLGetItem(*metaHead, i, &nvPair);

            free(nvPair->name);
            free(nvPair->value);
            }

        SDULLRemoveAllItems(metaHead, TRUE);
        }

}


// =========================================================================
// Returns TRUE if English, otherwise FALSE
// IMPORTANT: Returns FALSE if code is NULL or blank string
BOOL SDUi18nIsEnglishW()
{
    return SDUi18nIsLanguageCodeEnglishW(SDUi18n_UseDetails.LangCodeW);
}

BOOL SDUi18nIsLanguageCodeEnglishW(
    const WCHAR* code
)
{
    BOOL retval = FALSE;

    if (code != NULL)
        {
        if (wcsstr(code, SDUI18N_ISO639_ALPHA2_ENGLISHW) == code)
            {
            retval = TRUE;
            }
        }

    return retval;
}


// =========================================================================
// =========================================================================
