unit FreeOTFEExplorerfrmSelectDirectoryDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, SDFilesystemCtrls, SDUForms,
  SDFilesystem, OTFEFreeOTFE_InstructionRichEdit;

type
  TSelectDirType = (sdtDefault, sdtMove, sdtCopy, sdtStore);

  TfrmSelectDirectoryDlg = class(TSDUForm)
    SDFilesystemTreeView1: TSDFilesystemTreeView;
    pbOK: TButton;
    pbCancel: TButton;
    Label2: TLabel;
    reInstructions: TOTFEFreeOTFE_InstructionRichEdit;
    procedure FormShow(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
    procedure SDFilesystemTreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure SDFilesystemTreeView1DblClick(Sender: TObject);
  private
    FSelectedPath: string;
  protected
    procedure EnableDisableControls();

  public
    SelectDirType: TSelectDirType;
    Items: TStrings;
    Filesystem: TSDCustomFilesystem;
  published
    property SelectedPath: string read FSelectedPath;
  end;

function SelectFilesystemDirectory(
                                   AOwner: TComponent;
                                   Filesystem: TSDCustomFilesystem;
                                   selectDirType: TSelectDirType;
                                   items: TStrings
                                  ): string;

implementation

{$R *.dfm}

uses
  SDUi18n,
  SDUGeneral;

function SelectFilesystemDirectory(
                                   AOwner: TComponent;
                                   Filesystem: TSDCustomFilesystem;
                                   selectDirType: TSelectDirType;
                                   items: TStrings
                                  ): string;
var
  dlg: TfrmSelectDirectoryDlg;
  retval: string;
begin
  retval := '';

  dlg:= TfrmSelectDirectoryDlg.Create(AOwner);
  try
    dlg.SelectDirType := selectDirType;
    dlg.Items := Items;
    dlg.Filesystem := Filesystem;

    if (dlg.ShowModal = mrOK) then
      begin
      retval := dlg.SelectedPath;
      end;

  finally
    dlg.Free();
  end;

  Result := retval;
end;

procedure TfrmSelectDirectoryDlg.FormShow(Sender: TObject);
var
  dlgCaption: widestring;
  dlgInstructions: widestring;
  dlgOKButton: widestring;
  itemText: string;
begin
  SDFilesystemTreeView1.ReadOnly := TRUE;
  SDFilesystemTreeView1.Filesystem := Filesystem;
  SDFilesystemTreeView1.Initialize();

  // Expand out the root node
  if (SDFilesystemTreeView1.GoToPath(PATH_SEPARATOR, FALSE) <> nil) then
    begin
    SDFilesystemTreeView1.Selected.Expand(FALSE);
    end;

  dlgCaption      := _('Select folder');
  dlgInstructions := _('Please select folder:');
  dlgOKButton     := _('OK');

  itemText:= SDUParamSubstitute(
                                _('these %1 items'),
                                [inttostr(Items.count)]
                               );
  if (Items.Count = 1) then
    begin
    itemText := ''''+ExtractFilename(Items[0])+'''';
    end;

  case SelectDirType of
    sdtMove:
      begin
      dlgCaption      := _('Move Items');
      dlgInstructions := SDUParamSubstitute(
                                     _('Select the place where you want to move %1. Then click the Move button'),
                                     [itemText]
                                    );
      dlgOKButton     := _('Move');
      end;

    sdtCopy:
      begin
      dlgCaption      := _('Copy Items');
      dlgInstructions := SDUParamSubstitute(
                                     _('Select the place where you want to copy %1. Then click the Copy button'),
                                     [itemText]
                                    );
      dlgOKButton     := _('Copy');
      end;

    sdtStore:
      begin
      dlgCaption      := _('Store Items');
      dlgInstructions := SDUParamSubstitute(
                                     _('Select the place where you want to store %1. Then click the Store button'),
                                     [itemText]
                                    );
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
  if (SDFilesystemTreeView1.SelectionCount = 1) then
    begin
    FSelectedPath := SDFilesystemTreeView1.PathToNode(SDFilesystemTreeView1.Selected);
    ModalResult := mrOK;
    end;

end;

procedure TfrmSelectDirectoryDlg.SDFilesystemTreeView1Change(Sender: TObject;
  Node: TTreeNode);
begin
  EnableDisableControls();
end;

procedure TfrmSelectDirectoryDlg.SDFilesystemTreeView1DblClick(
  Sender: TObject);
begin
  if (pbOK.Enabled) then
    begin
    pbOKClick(pbOK);
    end;

end;

procedure TfrmSelectDirectoryDlg.EnableDisableControls();
begin
  SDUEnableControl(pbOK, (SDFilesystemTreeView1.SelectionCount = 1));
end;

END.


