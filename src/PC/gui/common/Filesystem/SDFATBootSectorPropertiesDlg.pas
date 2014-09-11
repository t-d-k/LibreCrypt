unit SDFATBootSectorPropertiesDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  SDUForms,
  SDFilesystem_FAT;

type
  TSDFATBootSectorPropertiesDialog = class(TSDUForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    edOEMName: TEdit;
    edBytesPerSector: TEdit;
    edSectorsPerCluster: TEdit;
    edReservedSectorCount: TEdit;
    edFATCount: TEdit;
    edMaxRootEntries: TEdit;
    edTotalSectors: TEdit;
    edMediaDescriptor: TEdit;
    edSectorsPerFAT: TEdit;
    edSectorsPerTrack: TEdit;
    edNumberOfHeads: TEdit;
    edHiddenSectors: TEdit;
    pbClose: TButton;
    procedure pbCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
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
  edOEMName.text             := Filesystem.OEMName;
  edBytesPerSector.text      := inttostr(Filesystem.BytesPerSector);
  edSectorsPerCluster.text   := inttostr(Filesystem.SectorsPerCluster);
  edReservedSectorCount.text := inttostr(Filesystem.ReservedSectorCount);
  edFATCount.text            := inttostr(Filesystem.FATCount);
  edMaxRootEntries.text      := inttostr(Filesystem.MaxRootEntries);
  edTotalSectors.text        := inttostr(Filesystem.TotalSectors);
  edMediaDescriptor.text     := '0x'+inttohex(Filesystem.MediaDescriptor, 2);
  edSectorsPerFAT.text       := inttostr(Filesystem.SectorsPerFAT);
  edSectorsPerTrack.text     := inttostr(Filesystem.SectorsPerTrack);
  edNumberOfHeads.text       := inttostr(Filesystem.NumberOfHeads);
  edHiddenSectors.text       := inttostr(Filesystem.HiddenSectors);

end;

END.

