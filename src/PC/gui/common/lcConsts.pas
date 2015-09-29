unit lcConsts;
 // Description: 
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.FreeOTFE.org/
 //
 // -----------------------------------------------------------------------------
 //


interface

uses
//sdu,lcutils
  lctypes;

const
  // Set to "-1" to indicate release/non-beta
   APP_BETA_BUILD = 0;

  { DONE 1 -otdk -cenhancement : update homepage }
  // Online user manual URL...
  URL_USERGUIDE  = 'http://LibreCrypt.eu/LibreCrypt/getting_started.html';

  // PAD file URL ...
  URL_PADFILE =
  {$IFDEF FORCE_LOCAL_PAD}
     'P:\'
  {$ELSE}
    'https://raw.githubusercontent.com/t-d-k/LibreCrypt/master/'
  {$ENDIF} + 'PAD.xml';

//below for explorer only

  // WebDAV related Windows services
  SERVICE_WEBCLIENT = 'WebClient';
  SERVICE_MRXDAV    = 'MRXDAV';

  SDUCR   = #$0D; // #13
  SDULF   = #$0A; // #10
  SDUCRLF = SDUCR + SDULF;

  SDUNEWLINE_TITLE: array [TSDUNewline] of String = ('CRLF', 'CR', 'LF');
 // SDUNEWLINE_TITLEWITHOS: array [TSDUNewline] of String =
//    ('CRLF (Windows)', 'CR (Mac)', 'LF (Linux)');
//  SDUNEWLINE_OSWITHTITLE: array [TSDUNewline] of String =
//    ('Windows (CRLF)', 'Mac (CR)', 'Linux (LF)');


  BYTES_IN_KILOBYTE = 1024;
  BYTES_IN_MEGABYTE = 1024 * BYTES_IN_KILOBYTE;
  BYTES_IN_GIGABYTE = 1024 * BYTES_IN_MEGABYTE;

resourcestring
  RS_DRIVEMAPPING_NOT_SUPPORTED_UNDER_VISTA_AND_7 =
    'For security reasons, drive mapping is not currently supported under Windows Vista and above';
      RS_UNKNOWN = '<unknown>';

implementation

end.
