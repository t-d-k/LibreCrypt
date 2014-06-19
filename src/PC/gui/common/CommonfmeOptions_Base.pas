unit CommonfmeOptions_Base;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, CommonSettings;

type
  TfmeOptions_Base = class(TFrame)
  private
    { Private declarations }
  protected
  public
    procedure Initialize(); virtual;
    procedure EnableDisableControls(); virtual;
    procedure ReadSettings(config: TSettings); virtual; abstract;
    function  CheckSettings(): boolean; virtual;
    procedure WriteSettings(config: TSettings); virtual; abstract;
  end;

implementation

{$R *.dfm}

procedure TfmeOptions_Base.Initialize();
begin
  // Base class does nothing
end;

procedure TfmeOptions_Base.EnableDisableControls();
begin
  // Base class does nothing
end;

function TfmeOptions_Base.CheckSettings(): boolean; 
begin
  Result := TRUE;
end;

END.

