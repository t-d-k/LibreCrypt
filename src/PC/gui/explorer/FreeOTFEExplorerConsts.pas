unit FreeOTFEExplorerConsts;
 // Description: 
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.FreeOTFE.org/
 //
 // -----------------------------------------------------------------------------
 //

     {$ERROR obsolete}
interface

const
  // Set to "-1" to indicate release/non-beta
  APP_BETA_BUILD = 9;

  // Online user manual URL...
  URL_USERGUIDE = 'http://LibreCrypt.eu/docs/Explorer';
  // PAD file URL...
  URL_PADFILE   = 'https://raw.githubusercontent.com/t-d-k/LibreCrypt/master/PAD.xml';

  // WebDAV related Windows services
  SERVICE_WEBCLIENT = 'WebClient';
  SERVICE_MRXDAV    = 'MRXDAV';

resourcestring
  RS_DRIVEMAPPING_NOT_SUPPORTED_UNDER_VISTA_AND_7 =
    'For security reasons, drive mapping is not currently supported under Windows Vista/Windows 7';

implementation

end.
