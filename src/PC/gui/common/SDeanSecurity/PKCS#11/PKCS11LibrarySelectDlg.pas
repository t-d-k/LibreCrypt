unit PKCS11LibrarySelectDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  SDUForms, ComCtrls, SDUComCtrls, StdCtrls,
  PKCS11KnownLibs;

type
  TPKCS11LibrarySelectDialog = class(TSDUForm)
    Label1: TLabel;
    pbOK: TButton;
    pbCancel: TButton;
    lvLibraries: TSDListView;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
    procedure lvLibrariesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  protected
    FKnownLibs: array of TPKCS11KnownLibrary;
    FSelectedLibrary: TPKCS11KnownLibrary;

    procedure AddCol(colCaption: string);
  public
    procedure Add(lib: TPKCS11KnownLibrary);

    procedure EnableDisableControls();
  published
    property SelectedLibrary: TPKCS11KnownLibrary read FSelectedLibrary;
  end;


implementation

{$R *.dfm}

uses
  SDUi18n,
  SDUGeneral;

procedure TPKCS11LibrarySelectDialog.Add(lib: TPKCS11KnownLibrary);
begin
  SetLength(FKnownLibs, length(FKnownLibs)+1);
  FKnownLibs[length(FKnownLibs)-1] := lib;
end;

procedure TPKCS11LibrarySelectDialog.AddCol(colCaption: string);
var
  newCol: TListColumn;
begin
  newCol := lvLibraries.columns.Add();
  newCol.Caption := colCaption;
  // Note: DON'T set AutoSize to TRUE; otherwise if the user resizes the
  //       dialog, any user set column widths will be reverted
  // (n/a in our case)
  newCol.AutoSize := TRUE;
end;

procedure TPKCS11LibrarySelectDialog.EnableDisableControls();
begin
  SDUEnableControl(pbOK, (lvLibraries.SelCount = 1));
end;

procedure TPKCS11LibrarySelectDialog.FormShow(Sender: TObject);
var
  i: integer;
begin
  AddCol(_('Library'));
  AddCol(_('Description'));

  lvLibraries.ResizeColumns();

  for i:=low(FKnownLibs) to high(FKnownLibs) do
    begin
    lvLibraries.AppendRow(
                          [
                           FKnownLibs[i].DLLFilename,
                           PKCS11KnownLibraryPrettyDesc(FKnownLibs[i])
                          ],
                          Pointer(i)
                         );
    end;

  EnableDisableControls();
end;

procedure TPKCS11LibrarySelectDialog.FormCreate(Sender: TObject);
begin
  lvLibraries.RowSelect := TRUE;
end;

procedure TPKCS11LibrarySelectDialog.pbOKClick(Sender: TObject);
var
  arrIdx: integer;
begin
  arrIdx := integer(lvLibraries.Items[lvLibraries.SelectedIdx].Data);
  FSelectedLibrary := FKnownLibs[arrIdx];
  
  ModalResult := mrOK;
end;

procedure TPKCS11LibrarySelectDialog.lvLibrariesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  EnableDisableControls();
end;

END.

