unit FreeOTFEExplorerfrmSelectDirectoryDlg;

interface

uses
  Classes, ComCtrls, Controls, Dialogs, Forms,
  Graphics, Messages, OTFEFreeOTFE_InstructionRichEdit, SDFilesystem, SDFilesystemCtrls, SDUForms,
  StdCtrls, SysUtils, Variants, Windows;

type
  TSelectDirType = (sdtDefault, sdtMove, sdtCopy, sdtStore);

  TfrmSelectDirectoryDlg = class (TSDUForm)
    SDFilesystemTreeView1: TSDFilesystemTreeView;
    pbOK:                  TButton;
    pbCancel:              TButton;
    Label2:                TLabel;
    reInstructions:        TOTFEFreeOTFE_InstructionRichEdit;
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
  dlg:    TfrmSelectDirectoryDlg;
  retval: String;
begin
  retval := '';

  dlg := TfrmSelectDirectoryDlg.Create(AOwner);
  try
    dlg.SelectDirType := selectDirType;
    dlg.Items         := Items;
    dlg.Filesystem    := Filesystem;

    if (dlg.ShowModal = mrOk) then begin
      retval := dlg.SelectedPath;
    end;

  finally
    dlg.Free();
  end;

  Result := retval;
end;

procedure TfrmSelectDirectoryDlg.FormShow(Sender: TObject);
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

  itemText := SDUParamSubstitute(_('these %1 items'), [IntToStr(Items.Count)]);
  if (Items.Count = 1) then begin
    itemText := '''' + ExtractFilename(Items[0]) + '''';
  end;

  case SelectDirType of
    sdtMove:
    begin
      dlgCaption      := _('Move Items');
      dlgInstructions := SDUParamSubstitute(
        _('Select the place where you want to move %1. Then click the Move button'),
        [itemText]);
      dlgOKButton     := _('Move');
    end;

    sdtCopy:
    begin
      dlgCaption      := _('Copy Items');
      dlgInstructions := SDUParamSubstitute(
        _('Select the place where you want to copy %1. Then click the Copy button'),
        [itemText]);
      dlgOKButton     := _('Copy');
    end;

    sdtStore:
    begin
      dlgCaption      := _('Store Items');
      dlgInstructions := SDUParamSubstitute(
        _('Select the place where you want to store %1. Then click the Store button'),
        [itemText]);
      dlgOKButton     := _('Store');
    end;

  end;

  self.Caption        := dlgCaption;
  reInstructions.Text := dlgInstructions;
  pbOK.Caption        := dlgOKButton;

  EnableDisableControls();
end;

procedure TfrmSelectDirectoryDlg.pbOKClick(Sender: TObject);
begin
  FSelectedPath := '';
  if (SDFilesystemTreeView1.SelectionCount = 1) then begin
    FSelectedPath := SDFilesystemTreeView1.PathToNode(SDFilesystemTreeView1.Selected);
    ModalResult   := mrOk;
  end;

end;

procedure TfrmSelectDirectoryDlg.SDFilesystemTreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  EnableDisableControls();
end;

procedure TfrmSelectDirectoryDlg.SDFilesystemTreeView1DblClick(Sender: TObject);
begin
  if (pbOK.Enabled) then begin
    pbOKClick(pbOK);
  end;

end;

procedure TfrmSelectDirectoryDlg.EnableDisableControls();
begin
  SDUEnableControl(pbOK, (SDFilesystemTreeView1.SelectionCount = 1));
end;

end.
