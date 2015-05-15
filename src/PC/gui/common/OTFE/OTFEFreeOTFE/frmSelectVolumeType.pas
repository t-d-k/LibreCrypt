unit frmSelectVolumeType;
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
  StdCtrls, ExtCtrls, SDUForms;

type
  TfrmSelectVolumeType = class(TSDUForm)
    rgVolumeType: TRadioGroup;
    pbCancel: TButton;
    pbOK: TButton;
    Label1: TLabel;
  private
    { Private declarations }
  public
    // Returns TRUE only if the user selected FreeOTFE
    function FreeOTFEVolume(): boolean;
    // Returns TRUE only if the user selected Linux
    function LinuxVolume(): boolean;
  end;


implementation

{$R *.DFM}


// Returns TRUE only if the user selected FreeOTFE
function TfrmSelectVolumeType.FreeOTFEVolume(): boolean;
begin
  Result := (rgVolumeType.ItemIndex = 0);

end;


// Returns TRUE only if the user selected Linux
function TfrmSelectVolumeType.LinuxVolume(): boolean;
begin
  Result := (rgVolumeType.ItemIndex = 1);

end;


END.


