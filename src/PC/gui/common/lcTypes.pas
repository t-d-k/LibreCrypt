unit lcTypes;

 {
 (c) tdk
 licence: dual, gpl and freeotfe

 contains types used by librecrypt
 }
interface

type
  DriveLetterChar   = Char;
  DriveLetterString = String;
  TSDUBytes         = array of Byte;
  TMountDiskType  = (fomaFixedDisk, fomaRemovableDisk, fomaCD, fomaDVD, fomaUnknown);
  TSDUNewline_Enum  = (nlCRLF, nlCR, nlLF);
  TMountResult = (morFail,morOK,morCancel);//result of prompted mount operation (ie dialog)

  // Note: FWIW:
  //   Windows normally uses CRLF
  //   Linux   normally uses LF
  //   MacOSX  normally uses CR

  TSDUNewline = TSDUNewline_Enum;


  TSDUArrayInteger = array of Integer;
  TSDUArrayString  = array of String;

  //  PasswordString       = TSDUBytes;
  PasswordString = Ansistring;

  {$IF CompilerVersion >= 18.5}// Delphi 2007 defined
  // If you have Delphi 2007, use the definition in Windows.pas (i.e. uses Windows)
  // Delphi 7 doesn't have ULONGLONG
  ULONGLONG = Uint64;        //TDK CHANGE
  // ULONGLONG = int64;  // Changed from Uint64 to prevent Delphi internal error
  // c1118 in SDUFormatUnits with Uint64 under Delphi7
  // (from feedback from OracleX <oraclex@mail.ru>)
  // Note: Because it's using int64 here, overloaded
  //       functions which provide ULONGLONG and int64
  //       versions have their ULONGLONG version ifdef'd
  //       out.
{$IFEND}


implementation

end.
