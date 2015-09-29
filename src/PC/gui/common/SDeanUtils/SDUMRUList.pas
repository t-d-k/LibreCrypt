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
  TSDUMRUList           = class;

  TSDUMRUItemClickEvent = procedure(mruList: TSDUMRUList; idx: Integer)
    of object;

  TSDUMRUList = class
  protected
    FMaxItems: Cardinal;
    FItems:    TStringList;
    FOnClick:  TSDUMRUItemClickEvent;

    procedure SetMaxItems(cnt: Cardinal);
    procedure ClearItemsBeyondMax();
    procedure MRUItemClick(Sender: TObject);

  public
    constructor Create();
    destructor Destroy(); override;

    procedure InsertAfter(mnuItem: TMenuItem);
    procedure InsertUnder(mnuItem: TMenuItem);
    procedure RemoveMenuItems(mnu: TMenu); overload;
    procedure RemoveMenuItems(mnu: TMenuItem); overload;

    procedure Add(item: string); overload;
    procedure Add(items: TStringList); overload;

    function Load(iniFile: TCustomIniFile; MRUSection: string = 'MRU')
      : Boolean; overload;
    function Load(reg: TSDURegistry; MRUSubKey: string = 'MRU')
      : Boolean; overload;
    // Warning: The SaveMRU(...) functions will *delete* the section/subkey
    // specified, and recreate it.
    function Save(iniFile: TCustomIniFile; MRUSection: string = 'MRU')
      : Boolean; overload;
    function Save(reg: TSDURegistry; MRUSubKey: string = 'MRU')
      : Boolean; overload;

  published
    property MaxItems: Cardinal read FMaxItems write SetMaxItems;
    property items:    TStringList read FItems write FItems;
    property OnClick:  TSDUMRUItemClickEvent read FOnClick write FOnClick;
  end;

implementation

uses
  SysUtils;

const
  // Tag to identify MRU menuitems.
  // Low WORD is holds index into "Items"
  MRUL_TAG           = $12340000;

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
    parentMenuItem  := mnuItem.Parent;
    mnuIdx          := mnuItem.MenuIndex;

    tmpItem         := TMenuItem.Create(nil);
    tmpItem.Caption := '-';
    tmpItem.Tag     := MRUL_TAG;
    Inc(mnuIdx);
    parentMenuItem.Insert(mnuIdx, tmpItem);

    for i             := 0 to (items.Count - 1) do begin
      tmpItem         := TMenuItem.Create(nil);
      tmpItem.Caption := '&' + IntToStr(i + 1) + ' ' + items[i];
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
  for i             := 0 to (items.Count - 1) do begin
    tmpItem         := TMenuItem.Create(mnuItem);
    tmpItem.Caption := '&' + IntToStr(i + 1) + ' ' + items[i];
    tmpItem.Tag     := MRUL_TAG + i;
    tmpItem.OnClick := MRUItemClick;
    mnuItem.Add(tmpItem);
  end;

end;

procedure TSDUMRUList.RemoveMenuItems(mnu: TMenu);
var
  i: Integer;
begin
  for i := (mnu.items.Count - 1) downto 0 do begin
    RemoveMenuItems(mnu.items[i]);
  end;
end;

// Remove any subitems beneath this menuitem
procedure TSDUMRUList.RemoveMenuItems(mnu: TMenuItem);
var
  i: Integer;
  cur:TMenuItem;
begin
  for i := (mnu.Count - 1) downto 0 do begin
    RemoveMenuItems(mnu.items[i]);
    if ((mnu.items[i].Tag and MRUL_TAG) = MRUL_TAG) then begin
      cur:= mnu.items[i]; //free removes from parent
      mnu.Delete(i);
      cur.Free; // not deleted by parent delete and no longer owned by anyone
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
  for i := 0 to (items.Count - 1) do
    Add(items[i]);
end;

procedure TSDUMRUList.Add(item: string);
var
  idx: Integer;
begin
  // Delete any existing instance of the item
  idx := items.IndexOf(item);
  if (idx >= 0) then
    items.Delete(idx);

  // Add the item to the head of the list
  items.Insert(0, item);
  ClearItemsBeyondMax();
end;

// If there are more items that the max allowed, delete the oldest
procedure TSDUMRUList.ClearItemsBeyondMax();
var
  i: Integer;
begin
  for i := (items.Count - 1) downto MaxItems do
    items.Delete(i);
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

function TSDUMRUList.Load(iniFile: TCustomIniFile;
  MRUSection: string = 'MRU'): Boolean;
var
  i:        Integer;
  cnt:      Integer;
  readItem: string;
begin
  Result       := False;
  try
    MaxItems   := iniFile.ReadInteger(MRUSection, 'MaxItems', MaxItems);
    cnt        := iniFile.ReadInteger(MRUSection, 'Items', 0);

    FItems.Clear();
    for i      := 0 to (cnt - 1) do begin
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


function TSDUMRUList.Load(reg: TSDURegistry; MRUSubKey: string = 'MRU')
  : Boolean;
var
  i:        Integer;
  cnt:      Integer;
  readItem: string;
begin
  Result         := False;
  try
    if reg.OpenKey(MRUSubKey, False) then begin
      MaxItems   := reg.ReadInteger('MaxItems', MaxItems);
      cnt        := reg.ReadInteger('Items', 0);

      FItems.Clear();
      for i      := 0 to (cnt - 1) do begin
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


function TSDUMRUList.Save(iniFile: TCustomIniFile;
  MRUSection: string = 'MRU'): Boolean;
var
  i: Integer;
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
    iniFile.WriteInteger(MRUSection, 'Items', items.Count);

    for i := 0 to (FItems.Count - 1) do begin
      iniFile.WriteString(MRUSection, IntToStr(i), items[i]);
    end;

    Result := True;
  except
    on E: Exception do begin
      // Do nothing; just swallow the error - Result already set to FALSE
    end;
  end;

end;


function TSDUMRUList.Save(reg: TSDURegistry; MRUSubKey: string = 'MRU')
  : Boolean;
var
  i: Integer;
begin
  Result := False;
  try
    reg.DeleteKey(MRUSubKey);
    if reg.OpenKey(MRUSubKey, True) then begin
      reg.WriteInteger('MaxItems', MaxItems);
      reg.WriteInteger('Items', items.Count);

      for i := 0 to (FItems.Count - 1) do begin
        reg.WriteString(IntToStr(i), items[i]);
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
