unit OTFETrueCryptMountDevice_U;
// Description:
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  OTFETrueCrypt_U;

type
  TOTFETrueCryptMountDevice_F = class(TForm)
  protected
    partitionDeviceNames: TStringList;
  public
    TrueCryptComponent: TOTFETrueCrypt;
    PartitionDevice: Ansistring;
  end;

implementation

END.


