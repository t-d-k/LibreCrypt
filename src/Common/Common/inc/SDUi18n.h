#ifndef _SDUi18n_H
#define _SDUi18n_H   1

#include <stdio.h>
#include <windows.h> // Required for BOOL type

#include "SDULinkedList.h"


#ifdef UNICODE
#define _(quote) SDUi18n_GetTextW(TEXT(quote))
#else
#define _(quote) SDUi18n_GetText(quote)
#endif

// Reasonable sized buffer capable of storing longest likely...
#define MAX_LANG_CODE  20
#define MAX_LANG_NAME  2048
#define MAX_TRANSLATOR 2048

typedef struct _SDUI18N_LANGUAGE {
    char LanguageCode[MAX_LANG_CODE];   
    char LanguageName[MAX_LANG_NAME];
} SDUI18N_LANGUAGE, *PSDUI18N_LANGUAGE;

typedef struct _SDUI18N_LANGUAGEW {
    WCHAR LanguageCode[MAX_LANG_CODE];
    WCHAR LanguageName[MAX_LANG_NAME];
    WCHAR Translator[MAX_TRANSLATOR];  // Translator - may include email address
    WCHAR TranslatorName[MAX_TRANSLATOR]; // Just the translators name
} SDUI18N_LANGUAGEW, *PSDUI18N_LANGUAGEW;


// Populate the structure passed in with the details for the translation 
// indicated by LanguageCode
BOOL SDUi18n_GetTranslationDetailsW(
    const WCHAR* LanguageCode,
    SDUI18N_LANGUAGEW* TranslationDetails
);

// Populate the link list supplied as "TranslationsList", setting the linked
// list's items to pointers to "LANGUAGEW" structures
// Note: It is the *callers* responsibility to free off the
//       "TranslationsList"'s items
// If "TranslationsList" is set to NULL, just return a count of the 
// translations found
// Returns the number of translations found
int SDUi18n_GetTranslationsW(
    LNKLIST_ITEM** TranslationsList
);

// Initialize i18n system
void SDUi18n_Init();

// Load translation
BOOL SDUi18n_Load_Code(const char* code);
BOOL SDUi18n_Load_FileForCode(const char* code, const char* filename);
BOOL SDUi18n_Load_File(const char* filename);
BOOL SDUi18n_Load_CodeW(const WCHAR* code);
BOOL SDUi18n_Load_FileForCodeW(const WCHAR* code, const WCHAR* filename);
BOOL SDUi18n_Load_FileW(const WCHAR* filename);


// Get translated text
// Returns: Pointer to translated text, or "text" passed in if either 
//          not found or not translated
// IMPORTANT: DON'T MODIFY THE TEXT RETURNED!
const char* SDUi18n_GetText(const char* text);
const char* SDUi18n_GetText_n(const char* text, const int plural);

// Returns: Pointer to translated text converted to WCHAR, or "text" passed 
//          in if either not found or not translated
// IMPORTANT: DON'T MODIFY THE TEXT RETURNED!
const WCHAR* SDUi18n_GetTextW(const WCHAR* text);
const WCHAR* SDUi18n_GetTextW_n(const WCHAR* text, const int plural);

// Unload any loaded translation
void SDUi18n_Unload();


// Deinitialize i18n system
void SDUi18n_Deinit();

// Translate a single string
BOOL SDUi18n_GetText_ForCode(
    const char* code, 
    const char* text,
    char* TranslationBuffer,
    int TranslationBufferSize  // In chars
);
BOOL SDUi18n_GetText_ForCodeW(
    const WCHAR* code, 
    const WCHAR* text,
    WCHAR* TranslationBuffer,
    int TranslationBufferSize  // In chars
);

// Returns TRUE if English, otherwise FALSE
// IMPORTANT: Returns FALSE if code is NULL or blank string
BOOL SDUi18nIsEnglishW();   
BOOL SDUi18nIsLanguageCodeEnglishW(
    const WCHAR* code
);

#endif
