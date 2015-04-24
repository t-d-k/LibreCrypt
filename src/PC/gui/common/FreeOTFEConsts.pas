unit FreeOTFEConsts;
 // Description: 
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.FreeOTFE.org/
 //
 // -----------------------------------------------------------------------------
 //


interface

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

resourcestring
  RS_DRIVEMAPPING_NOT_SUPPORTED_UNDER_VISTA_AND_7 =
    'For security reasons, drive mapping is not currently supported under Windows Vista/Windows 7';


implementation

end.
