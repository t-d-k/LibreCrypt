unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin,
{$WARN UNIT_PLATFORM OFF}
  FileCtrl;

type
  TForm1 = class(TForm)
    pbPropertiesDisk: TButton;
    pbPropertiesPartition: TButton;
    seDiskNumber: TSpinEdit;
    Label1: TLabel;
    dcbDrive: TDriveComboBox;
    Label2: TLabel;
    procedure pbPropertiesDiskClick(Sender: TObject);
    procedure pbPropertiesPartitionClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  SDUGeneral,
  SDUDialogs,
  SDUDiskPropertiesDlg,
  SDUPartitionPropertiesDlg;

procedure TForm1.pbPropertiesDiskClick(Sender: TObject);
var
  dlg: TSDUDiskPropertiesDialog;
begin
  dlg:= TSDUDiskPropertiesDialog.Create(nil);
  try
    dlg.DiskNumber := seDiskNumber.Value;
    dlg.ShowModal();
  finally
    dlg.Free();
  end;

end;

procedure TForm1.pbPropertiesPartitionClick(Sender: TObject);
var
  dlg: TSDUPartitionPropertiesDialog;
  partInfo: TSDUPartitionInfo;
begin
  if not(SDUGetPartitionInfo(dcbDrive.drive, partInfo)) then
    begin
    SDUMessageDlg('Unable to get partition information', mtError);
    end
  else
    begin
    dlg:= TSDUPartitionPropertiesDialog.Create(nil);
    try
      dlg.MountedAsDrive := dcbDrive.drive;
      dlg.PartitionInfo := partInfo;

      dlg.ShowModal();
    finally
      dlg.Free();
    end;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  self.caption := Application.Title;
end;

END.



