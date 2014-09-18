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
  { TODO 1 -otdk -cenhancement : update homepage }
  // Online user manual URL...
  URL_USERGUIDE  = 'http://DoxBox.eu/docs/Main';

  // PAD file URL...
  URL_PADFILE =
  {$IFDEF FORCE_LOCAL_PAD}
     'P:\Projects\Delphi\doxbox\'
  {$ELSE}
    'https://raw.githubusercontent.com/t-d-k/doxbox/master/'
  {$ENDIF} + 'PAD.xml';

implementation

end.
