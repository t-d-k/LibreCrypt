unit SDFATBootSectorPropertiesDlg;

interface

uses
  Classes, Controls, Dialogs, Forms,
  Graphics, Messages, SDFilesystem_FAT, SDUForms,
  StdCtrls,
  SysUtils, Variants, Windows;

type
  TSDFATBootSectorPropertiesDialog = class (TSDUForm)
    Label1:                TLabel;
    Label2:                TLabel;
    Label3:                TLabel;
    Label4:                TLabel;
    Label5:                TLabel;
    Label6:                TLabel;
    Label7:                TLabel;
    Label8:                TLabel;
    Label9:                TLabel;
    Label10:               TLabel;
    Label11:               TLabel;
    Label12:               TLabel;
    edOEMName:             TEdit;
    edBytesPerSector:      TEdit;
    edSectorsPerCluster:   TEdit;
    edReservedSectorCount: TEdit;
    edFATCount:            TEdit;
    edMaxRootEntries:      TEdit;
    edTotalSectors:        TEdit;
    edMediaDescriptor:     TEdit;
    edSectorsPerFAT:       TEdit;
    edSectorsPerTrack:     TEdit;
    edNumberOfHeads:       TEdit;
    edHiddenSectors:       TEdit;
    pbClose:               TButton;
    procedure pbCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    Filesystem: TSDFilesystem_FAT;
  end;

implementation

{$R *.dfm}

procedure TSDFATBootSectorPropertiesDialog.pbCloseClick(Sender: TObject);
begin
  Close();
end;

procedure TSDFATBootSectorPropertiesDialog.FormShow(Sender: TObject);
begin
  edOEMName.Text             := Filesystem.OEMName;
  edBytesPerSector.Text      := IntToStr(Filesystem.BytesPerSector);
  edSectorsPerCluster.Text   := IntToStr(Filesystem.SectorsPerCluster);
  edReservedSectorCount.Text := IntToStr(Filesystem.ReservedSectorCount);
  edFATCount.Text            := IntToStr(Filesystem.FATCount);
  edMaxRootEntries.Text      := IntToStr(Filesystem.MaxRootEntries);
  edTotalSectors.Text        := IntToStr(Filesystem.TotalSectors);
  edMediaDescriptor.Text     := '0x' + inttohex(Filesystem.MediaDescriptor, 2);
  edSectorsPerFAT.Text       := IntToStr(Filesystem.SectorsPerFAT);
  edSectorsPerTrack.Text     := IntToStr(Filesystem.SectorsPerTrack);
  edNumberOfHeads.Text       := IntToStr(Filesystem.NumberOfHeads);
  edHiddenSectors.Text       := IntToStr(Filesystem.HiddenSectors);

end;

end.
