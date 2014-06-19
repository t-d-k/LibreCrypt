unit SDUCheckLst;
// Description: TCheckListBox with ReadOnly property
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//

interface

uses
  CheckLst;

type
  TSDUCheckListBox = class(TCheckListBox)
  private
    FReadOnly: boolean;
  protected
    procedure SetReadOnly(ro: boolean);
    procedure ClickCheck(); override;

  public
    procedure SwapItems(a: integer; b: integer);

    procedure SelectedMoveUp();
    procedure SelectedMoveDown();
    procedure SelectedDelete();
    procedure SelectedChecked(value: boolean);

  published
    property ReadOnly: boolean read FReadOnly write SetReadOnly;
  end;

procedure Register;


implementation

uses
  Classes, Graphics;

procedure Register;
begin
  RegisterComponents('SDeanUtils', [TSDUCheckListBox]);
end;

procedure TSDUCheckListBox.SetReadOnly(ro: boolean);
begin
  FReadOnly := ro;

  Enabled := not(FReadOnly);
  if FReadOnly then
    begin
    Color := clBtnFace;
    end
  else
    begin
    Color := clWindow;
    end;

end;

procedure TSDUCheckListBox.ClickCheck();
var
  i: integer;
begin
  if FReadOnly then
    begin
    for i:=0 to (items.count-1) do
      begin
      if Selected[i] then
        begin
        Checked[i] := not(Checked[i]);
        end;
      end;
    end
  else
    begin
    inherited;
    end;
    
end;


procedure TSDUCheckListBox.SwapItems(a: integer; b: integer);
var
  tempItem: string;
  tempObj: TObject;
  tempSelected: boolean;
  tempEnabled: boolean;
  tempChecked: boolean;
begin
  Items.BeginUpdate;
  try
    tempItem              := self.Items[a];
    tempObj               := self.Items.Objects[a];
    tempSelected          := self.Selected[a];
    tempEnabled           := self.ItemEnabled[a];
    tempChecked           := self.Checked[a];

    self.Items[a]         := self.Items[b];
    self.Items.Objects[a] := self.Items.Objects[b];
    self.Selected[a]      := self.Selected[b];
    self.ItemEnabled[a]   := self.ItemEnabled[b];
    self.Checked[a]       := self.Checked[b];

    self.Items[b]         := tempItem;
    self.Items.Objects[b] := tempObj;
    self.Selected[b]      := tempSelected;
    self.ItemEnabled[b]   := tempEnabled;
    self.Checked[b]       := tempChecked;

  finally
    Items.EndUpdate
  end;
 
end;


procedure TSDUCheckListBox.SelectedMoveUp();
var
  i: integer;
begin
  // Start from 1 - can't move the first item up
  for i:=1 to (items.count-1) do
    begin
    if selected[i] then
      begin
      SwapItems(i, (i-1));
      end;
    end;

end;

procedure TSDUCheckListBox.SelectedMoveDown();
var
  i: integer;
begin
  // -2 because we can't move the last item down
  for i:=(items.count-2) downto 0 do
    begin
    if selected[i] then
      begin
      SwapItems(i, (i+1));
      end;
    end;

end;

procedure TSDUCheckListBox.SelectedDelete();
var
  i: integer;
begin
  for i:=(items.count-1) downto 0 do
    begin
    if selected[i] then
      begin
      Items.Delete(i);
      end;
    end;

end;

procedure TSDUCheckListBox.SelectedChecked(value: boolean);
var
  i: integer;
begin
  for i:=(items.count-1) downto 0 do
    begin
    if selected[i] then
      begin
      Checked[i] := value;
      end;
    end;

end;

END.

