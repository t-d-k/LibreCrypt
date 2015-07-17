unit frmSelectDirectory;

interface

uses
  Classes, ComCtrls, Controls, Dialogs, Forms,
  Graphics, Messages, OTFEFreeOTFE_InstructionRichEdit, SDFilesystem, SDFilesystemCtrls, SDUForms,
  StdCtrls, SysUtils, Variants, Windows;

type
  TSelectDirType = (sdtDefault, sdtMove, sdtCopy, sdtStore);

  TfrmSelectDirectory = class (TSDUForm)
    SDFilesystemTreeView1: TSDFilesystemTreeView;
    pbOK:                  TButton;
    pbCancel:              TButton;
    Label2:                TLabel;
    reInstructions:        TRichEdit;
    procedure FormShow(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
    procedure SDFilesystemTreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure SDFilesystemTreeView1DblClick(Sender: TObject);
  PRIVATE
    FSelectedPath: String;
  PROTECTED
    procedure EnableDisableControls();

  PUBLIC
    SelectDirType: TSelectDirType;
    Items:         TStrings;
    Filesystem:    TSDCustomFilesystem;
  PUBLISHED
    property SelectedPath: String Read FSelectedPath;
  end;

function SelectFilesystemDirectory(AOwner: TComponent; Filesystem: TSDCustomFilesystem;
  selectDirType: TSelectDirType; items: TStrings): String;

implementation

{$R *.dfm}

uses
  SDUGeneral, SDUi18n;

function SelectFilesystemDirectory(AOwner: TComponent; Filesystem: TSDCustomFilesystem;
  selectDirType: TSelectDirType; items: TStrings): String;
var
  dlg:    TfrmSelectDirectory;
begin
  Result := '';

  dlg := TfrmSelectDirectory.Create(AOwner);
  try
    dlg.SelectDirType := selectDirType;
    dlg.Items         := Items;
    dlg.Filesystem    := Filesystem;

    if (dlg.ShowModal = mrOk) then begin
      Result := dlg.SelectedPath;
    end;

  finally
    dlg.Free();
  end;


end;

procedure TfrmSelectDirectory.FormShow(Sender: TObject);
var
  dlgCaption:      WideString;
  dlgInstructions: WideString;
  dlgOKButton:     WideString;
  itemText:        String;
begin
  SDFilesystemTreeView1.ReadOnly   := True;
  SDFilesystemTreeView1.Filesystem := Filesystem;
  SDFilesystemTreeView1.Initialize();

  // Expand out the root node
  if (SDFilesystemTreeView1.GoToPath(PATH_SEPARATOR, False) <> nil) then begin
    SDFilesystemTreeView1.Selected.Expand(False);
  end;

  dlgCaption      := _('Select folder');
  dlgInstructions := _('Please select folder:');
  dlgOKButton     := _('OK');

  itemText := Format(_('these %d items'), [Items.Count]);
  if (Items.Count = 1) then begin
    itemText := '''' + ExtractFilename(Items[0]) + '''';
  end;

  case SelectDirType of
    sdtMove:
    begin
      dlgCaption      := _('Move Items');
      dlgInstructions := Format(
        _('Select the place where you want to move %s. Then click the Move button'),
        [itemText]);
      dlgOKButton     := _('Move');
    end;

    sdtCopy:
    begin
      dlgCaption      := _('Copy Items');
      dlgInstructions := Format(
        _('Select the place where you want to copy %s. Then click the Copy button'),
        [itemText]);
      dlgOKButton     := _('Copy');
    end;

    sdtStore:
    begin
      dlgCaption      := _('Store Items');
      dlgInstructions := Format(
        _('Select the place where you want to store %s. Then click the Store button'),
        [itemText]);
      dlgOKButton     := _('Store');
    end;

  end;

  self.Caption        := dlgCaption;
  reInstructions.Text := dlgInstructions;
  pbOK.Caption        := dlgOKButton;

  EnableDisableControls();
end;

procedure TfrmSelectDirectory.pbOKClick(Sender: TObject);
begin
  FSelectedPath := '';
  if (SDFilesystemTreeView1.SelectionCount = 1) then begin
    FSelectedPath := SDFilesystemTreeView1.PathToNode(SDFilesystemTreeView1.Selected);
    ModalResult   := mrOk;
  end;

end;

procedure TfrmSelectDirectory.SDFilesystemTreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  EnableDisableControls();
end;

procedure TfrmSelectDirectory.SDFilesystemTreeView1DblClick(Sender: TObject);
begin
  if (pbOK.Enabled) then begin
    pbOKClick(pbOK);
  end;

end;

procedure TfrmSelectDirectory.EnableDisableControls();
begin
  SDUEnableControl(pbOK, (SDFilesystemTreeView1.SelectionCount = 1));
end;

end.
