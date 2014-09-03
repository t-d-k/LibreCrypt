unit OTFEE4MMountDevice_U;
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
  OTFEE4M_U;

type
  TOTFEE4MMountDevice_F = class(TForm)
  protected
    partitionDeviceNames: TStringList;
  public
    E4MComponent: TOTFEE4M;
    PartitionDevice: Ansistring;
  end;

implementation

END.


