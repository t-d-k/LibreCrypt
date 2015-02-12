unit SDUMRUList;

interface

uses
  Classes,
  IniFiles,
  Menus,
  SDURegistry;

type
{$TYPEINFO ON}// Needed to allow "published"

  // Forward declaration
  TSDUMRUList = class ;

  TSDUMRUItemClickEvent = procedure(mruList: TSDUMRUList; idx: Integer) of object;

  TSDUMRUList = class
  PROTECTED
    FMaxItems: Cardinal;
    FItems:    TStringList;
    FOnClick:  TSDUMRUItemClickEvent;

    procedure SetMaxItems(cnt: Cardinal);
    procedure ClearItemsBeyondMax();
    procedure MRUItemClick(Sender: TObject);

  PUBLIC
    constructor Create();
    destructor Destroy(); OVERRIDE;

    procedure InsertAfter(mnuItem: TMenuItem);
    procedure InsertUnder(mnuItem: TMenuItem);
    procedure RemoveMenuItems(mnu: TMenu); OVERLOAD;
    procedure RemoveMenuItems(mnu: TMenuItem); OVERLOAD;

    procedure Add(item: String); OVERLOAD;
    procedure Add(items: TStringList); OVERLOAD;

    function Load(iniFile: TCustomIniFile; MRUSection: String = 'MRU'): Boolean; OVERLOAD;
    function Load(reg: TSDURegistry; MRUSubKey: String = 'MRU'): Boolean; OVERLOAD;
    // Warning: The SaveMRU(...) functions will *delete* the section/subkey
    //          specified, and recreate it.
    function Save(iniFile: TCustomIniFile; MRUSection: String = 'MRU'): Boolean; OVERLOAD;
    function Save(reg: TSDURegistry; MRUSubKey: String = 'MRU'): Boolean; OVERLOAD;

  PUBLISHED
    property MaxItems: Cardinal Read FMaxItems Write SetMaxItems;
    property Items: TStringList Read FItems Write FItems;
    property OnClick: TSDUMRUItemClickEvent Read FOnClick Write FOnClick;
  end;

implementation

uses
  SysUtils;

const
  // Tag to identify MRU menuitems.
  // Low WORD is holds index into "Items"
  MRUL_TAG = $12340000;

  DEFAULT_MRU_LENGTH = 5;

constructor TSDUMRUList.Create();
begin
  FItems    := TStringList.Create();
  FMaxItems := DEFAULT_MRU_LENGTH;
end;

destructor TSDUMRUList.Destroy();
begin
  FItems.Free();
end;

procedure TSDUMRUList.InsertAfter(mnuItem: TMenuItem);
var
  i:              Integer;
  tmpItem:        TMenuItem;
  parentMenuItem: TMenuItem;
  mnuIdx:         Integer;
begin
  if (items.Count > 0) then begin
    parentMenuItem := mnuItem.Parent;
    mnuIdx         := mnuItem.MenuIndex;

    tmpItem         := TMenuItem.Create(nil);
    tmpItem.Caption := '-';
    tmpItem.Tag     := MRUL_TAG;
    Inc(mnuIdx);
    parentMenuItem.Insert(mnuIdx, tmpItem);

    for i := 0 to (items.Count - 1) do begin
      tmpItem         := TMenuItem.Create(nil);
      tmpItem.Caption := '&' + IntToStr(i + 1) + ' ' + Items[i];
      tmpItem.Tag     := MRUL_TAG + i;
      tmpItem.OnClick := MRUItemClick;
      Inc(mnuIdx);
      parentMenuItem.Insert(mnuIdx, tmpItem);
    end;
  end;

end;

procedure TSDUMRUList.InsertUnder(mnuItem: TMenuItem);
var
  i:       Integer;
  tmpItem: TMenuItem;
begin
  for i := 0 to (items.Count - 1) do begin
    tmpItem         := TMenuItem.Create(mnuItem);
    tmpItem.Caption := '&' + IntToStr(i + 1) + ' ' + Items[i];
    tmpItem.Tag     := MRUL_TAG + i;
    tmpItem.OnClick := MRUItemClick;
    mnuItem.Add(tmpItem);
  end;

end;

procedure TSDUMRUList.RemoveMenuItems(mnu: TMenu);
var
  i: Integer;
begin
  for i := (mnu.Items.Count - 1) downto 0 do begin
    RemoveMenuItems(mnu.Items[i]);
  end;
end;

// Remove any subitems beneath this menuitem
procedure TSDUMRUList.RemoveMenuItems(mnu: TMenuItem);
var
  i: Integer;
begin
  for i := (mnu.Count - 1) downto 0 do begin
    RemoveMenuItems(mnu.Items[i]);

    if ((mnu.Items[i].tag and MRUL_TAG) = MRUL_TAG) then begin
      mnu.Delete(i);
    end;
  end;
end;

procedure TSDUMRUList.SetMaxItems(cnt: Cardinal);
begin
  FMaxItems := cnt;
  ClearItemsBeyondMax();

end;

procedure TSDUMRUList.Add(items: TStringList);
var
  i: Integer;
begin
  for i := 0 to (items.Count - 1) do begin
    Add(items[i]);
  end;
end;

procedure TSDUMRUList.Add(item: String);
var
  idx: Integer;
begin
  // Delete any existing instance of the item
  idx := Items.IndexOf(item);
  if (idx >= 0) then begin
    Items.Delete(idx);
  end;

  // Add the item to the head of the list
  Items.Insert(0, item);
  ClearItemsBeyondMax();

end;

// If there are more items that the max allowed, delete the oldest
procedure TSDUMRUList.ClearItemsBeyondMax();
var
  i: Integer;
begin
  for i := (Items.Count - 1) downto MaxItems do begin
    Items.Delete(i);
  end;

end;

procedure TSDUMRUList.MRUItemClick(Sender: TObject);
var
  idx: Integer;
begin
  if (Sender is TMenuItem) then begin
    idx := TMenuItem(Sender).Tag and $FFFF;
    if Assigned(FOnClick) then begin
      FOnClick(self, idx);
    end;
  end;

end;

function TSDUMRUList.Load(iniFile: TCustomIniFile; MRUSection: String = 'MRU'): Boolean;
var
  i:        Integer;
  cnt:      Integer;
  readItem: String;
begin
  Result := False;
  try
    MaxItems := iniFile.ReadInteger(MRUSection, 'MaxItems', MaxItems);
    cnt      := iniFile.ReadInteger(MRUSection, 'Items', 0);

    FItems.Clear();
    for i := 0 to (cnt - 1) do begin
      readItem := iniFile.ReadString(MRUSection, IntToStr(i), '');
      FItems.Add(readItem);
    end;

    Result := True;
  except
    on E: Exception do begin
      // Do nothing; just swallow the error - Result already set to FALSE
    end;
  end;


end;


function TSDUMRUList.Load(reg: TSDURegistry; MRUSubKey: String = 'MRU'): Boolean;
var
  i:        Integer;
  cnt:      Integer;
  readItem: String;
begin
  Result := False;
  try
    if reg.OpenKey(MRUSubKey, False) then begin
      MaxItems := reg.ReadInteger('MaxItems', MaxItems);
      cnt      := reg.ReadInteger('Items', 0);

      FItems.Clear();
      for i := 0 to (cnt - 1) do begin
        readItem := reg.ReadString(IntToStr(i), '');
        FItems.Add(readItem);
      end;

      Result := True;
    end;
  except
    on E: Exception do begin
      // Do nothing; just swallow the error - Result already set to FALSE
    end;
  end;


end;


function TSDUMRUList.Save(iniFile: TCustomIniFile; MRUSection: String = 'MRU'): Boolean;
var
  i:      Integer;
begin
  Result := False;
  try
    try
      iniFile.EraseSection(MRUSection);
    except
      on E: Exception do begin
        // Do nothing; section may not have existed
      end;
    end;

    iniFile.WriteInteger(MRUSection, 'MaxItems', MaxItems);
    iniFile.WriteInteger(MRUSection, 'Items', Items.Count);

    for i := 0 to (FItems.Count - 1) do begin
      iniFile.WriteString(MRUSection, IntToStr(i), Items[i]);
    end;

    Result := True;
  except
    on E: Exception do begin
      // Do nothing; just swallow the error - Result already set to FALSE
    end;
  end;


end;


function TSDUMRUList.Save(reg: TSDURegistry; MRUSubKey: String = 'MRU'): Boolean;
var
  i:      Integer;
begin
  Result := False;
  try
    reg.DeleteKey(MRUSubKey);
    if reg.OpenKey(MRUSubKey, True) then begin
      reg.WriteInteger('MaxItems', MaxItems);
      reg.WriteInteger('Items', Items.Count);

      for i := 0 to (FItems.Count - 1) do begin
        reg.WriteString(IntToStr(i), Items[i]);
      end;

      Result := True;
    end;
  except
    on E: Exception do begin
      // Do nothing; just swallow the error - Result already set to FALSE
    end;
  end;


end;

end.
