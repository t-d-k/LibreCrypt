unit SDFilesystemCtrls_ColDetails;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin64, CheckLst, SDUCheckLst, SDUForms,
  SDFilesystemCtrls;

type
  TSDFilesystemListView_ColDetails = class(TSDUForm)
    clbColumns: TSDUCheckListBox;
    pbMoveUp: TButton;
    pbMoveDown: TButton;
    pbShow: TButton;
    pbHide: TButton;
    pbOK: TButton;
    pbCancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    se64Width: TSpinEdit64;
    pnlSplitter: TPanel;
    procedure FormShow(Sender: TObject);
    procedure pbMoveUpClick(Sender: TObject);
    procedure pbMoveDownClick(Sender: TObject);
    procedure pbShowClick(Sender: TObject);
    procedure pbHideClick(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure clbColumnsClick(Sender: TObject);
    procedure clbColumnsClickCheck(Sender: TObject);
  private
    FLastSelected: TFilesystemListViewColumn;
    FInternalLayout: TFilesystemListView_Layout;

    function  GetLayout(): TFilesystemListView_Layout;
    procedure SetLayout(newLayout: TFilesystemListView_Layout);

    procedure EnableDisableControls();

  public
    // Note: This is public, not published as array type
    property Layout: TFilesystemListView_Layout read GetLayout write SetLayout;
  end;


implementation

{$R *.dfm}

uses
  SDUGeneral;

procedure TSDFilesystemListView_ColDetails.clbColumnsClick(Sender: TObject);
begin
  if (se64Width.Value > 0) then
    begin
    FInternalLayout[FLastSelected].Width := integer(se64Width.Value);
    end;
    
  FLastSelected := TFilesystemListViewColumn(clbColumns.Items.Objects[clbColumns.ItemIndex]);
  se64Width.Value := FInternalLayout[FLastSelected].Width;

  EnableDisableControls();
end;

procedure TSDFilesystemListView_ColDetails.clbColumnsClickCheck(
  Sender: TObject);
begin
  EnableDisableControls();
end;

procedure TSDFilesystemListView_ColDetails.FormCreate(Sender: TObject);
begin
  FInternalLayout := FILESYSTEMLISTVIEWCOLUMN_DEFAULTS;
end;

procedure TSDFilesystemListView_ColDetails.FormShow(Sender: TObject);
begin
  pnlSplitter.Caption := '';
  pnlSplitter.Height := 2;
  pnlSplitter.BevelOuter := bvLowered;

  EnableDisableControls();
end;

function TSDFilesystemListView_ColDetails.GetLayout(): TFilesystemListView_Layout;
var
  i: integer;
  retval: TFilesystemListView_Layout;
  currColType: TFilesystemListViewColumn;
begin
  retval := FInternalLayout;

  // Width would have been set on FInternalLayout - no need to set it in this
  // loop
  
  for i:=0 to (clbColumns.items.count-1) do
    begin
    currColType := TFilesystemListViewColumn(clbColumns.Items.Objects[i]);

    retval[currColType].Visible := clbColumns.Checked[i];
    retval[currColType].Position := i;
    end;

  Result := retval
end;

procedure TSDFilesystemListView_ColDetails.SetLayout(newLayout: TFilesystemListView_Layout);
var
  colOrder: TFilesystemListView_ColOrder;
  i: integer;
  listIdx: integer;
begin
  FInternalLayout := newLayout;
  
  colOrder := GetLayoutColOrder(newLayout, TRUE);

  for i:=low(colOrder) to high(colOrder) do
    begin
    listIdx := clbColumns.Items.AddObject(FilesystemListViewColumnTitle(colOrder[i]), TObject(ord(colOrder[i])));
    clbColumns.ItemEnabled[listIdx] := (colOrder[i] <> flvcFilename);
    clbColumns.Checked[listIdx] := newLayout[colOrder[i]].Visible;
    end;

  FLastSelected := colOrder[low(colOrder)];
  se64Width.Value := FInternalLayout[FLastSelected].Width;
  clbColumns.ItemIndex := 0;

end;

procedure TSDFilesystemListView_ColDetails.pbMoveUpClick(Sender: TObject);
begin
  clbColumns.SelectedMoveUp();
  EnableDisableControls();

end;

procedure TSDFilesystemListView_ColDetails.pbMoveDownClick(
  Sender: TObject);
begin
  clbColumns.SelectedMoveDown();
  EnableDisableControls();

end;

procedure TSDFilesystemListView_ColDetails.pbShowClick(Sender: TObject);
begin
  clbColumns.SelectedChecked(TRUE);
  EnableDisableControls();

end;

procedure TSDFilesystemListView_ColDetails.pbHideClick(Sender: TObject);
begin
  clbColumns.SelectedChecked(FALSE);
  EnableDisableControls();

end;

procedure TSDFilesystemListView_ColDetails.pbOKClick(Sender: TObject);
begin
  FInternalLayout[FLastSelected].Width := integer(se64Width.Value);
  ModalResult := mrOK;
end;

procedure TSDFilesystemListView_ColDetails.EnableDisableControls();
begin
  SDUEnableControl(pbHide, FALSE);
  SDUEnableControl(pbShow, FALSE);
  if (clbColumns.ItemIndex >= 0) then
    begin
    SDUEnableControl(pbHide, clbColumns.Checked[clbColumns.ItemIndex]);
    SDUEnableControl(pbShow, not(pbHide.Enabled));
    end;

  SDUEnableControl(pbMoveUp,   (clbColumns.ItemIndex > 0));
  SDUEnableControl(pbMoveDown, (clbColumns.ItemIndex < (clbColumns.Items.Count - 1)));

end;

END.

