unit SDUMRUList;

interface

uses
  Menus,
  Classes,
  IniFiles,
  SDURegistry;

type
{$TYPEINFO ON} // Needed to allow "published"

  // Forward declaration
  TSDUMRUList = class;

  TSDUMRUItemClickEvent = procedure (mruList: TSDUMRUList; idx: integer) of object;

  TSDUMRUList = class
  protected
    FMaxItems: cardinal;
    FItems: TStringList;
    FOnClick: TSDUMRUItemClickEvent;

    procedure SetMaxItems(cnt: cardinal);
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

    function Load(iniFile: TCustomIniFile; MRUSection: string = 'MRU'): boolean; overload;
    function Load(reg: TSDURegistry; MRUSubKey: string = 'MRU'): boolean; overload;
    // Warning: The SaveMRU(...) functions will *delete* the section/subkey
    //          specified, and recreate it.
    function Save(iniFile: TCustomIniFile; MRUSection: string = 'MRU'): boolean; overload;
    function Save(reg: TSDURegistry; MRUSubKey: string = 'MRU'): boolean; overload;

  published
    property MaxItems: cardinal read FMaxItems write SetMaxItems;
    property Items: TStringList read FItems write FItems;
    property OnClick: TSDUMRUItemClickEvent read FOnClick write FOnClick;
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
  FItems := TStringList.Create();
  FMaxItems := DEFAULT_MRU_LENGTH;
end;

destructor TSDUMRUList.Destroy();
begin
  FItems.Free();
end;

procedure TSDUMRUList.InsertAfter(mnuItem: TMenuItem);
var
  i: integer;
  tmpItem: TMenuItem;
  parentMenuItem: TMenuItem;
  mnuIdx: integer;
begin
  if (items.count > 0) then
    begin
    parentMenuItem := mnuItem.Parent;
    mnuIdx := mnuItem.MenuIndex;

    tmpItem := TMenuItem.Create(nil);
    tmpItem.Caption := '-';
    tmpItem.Tag := MRUL_TAG;
    inc(mnuIdx);
    parentMenuItem.Insert(mnuIdx, tmpItem);
    
    for i:=0 to (items.count - 1) do
      begin
      tmpItem := TMenuItem.Create(nil);
      tmpItem.Caption := '&'+inttostr(i+1)+' '+Items[i];
      tmpItem.Tag := MRUL_TAG + i;
      tmpItem.OnClick := MRUItemClick;
      inc(mnuIdx);
      parentMenuItem.Insert(mnuIdx, tmpItem);
      end;
    end;

end;

procedure TSDUMRUList.InsertUnder(mnuItem: TMenuItem);
var
  i: integer;
  tmpItem: TMenuItem;
begin
  for i:=0 to (items.count - 1) do
    begin
    tmpItem := TMenuItem.Create(mnuItem);
    tmpItem.Caption := '&'+inttostr(i+1)+' '+Items[i];
    tmpItem.Tag := MRUL_TAG + i;
    tmpItem.OnClick := MRUItemClick;
    mnuItem.Add(tmpItem);
    end;

end;

procedure TSDUMRUList.RemoveMenuItems(mnu: TMenu);
var
  i: integer;
begin
  for i:=(mnu.Items.Count - 1) downto 0 do
    begin
    RemoveMenuItems(mnu.Items[i]);
    end;
end;

// Remove any subitems beneath this menuitem
procedure TSDUMRUList.RemoveMenuItems(mnu: TMenuItem);
var
  i: integer;
begin
  for i:=(mnu.Count - 1) downto 0 do
    begin
    RemoveMenuItems(mnu.Items[i]);

    if ((mnu.Items[i].tag and MRUL_TAG) = MRUL_TAG) then
      begin
      mnu.Delete(i);
      end;
    end;
end;

procedure TSDUMRUList.SetMaxItems(cnt: cardinal);
begin
  FMaxItems := cnt;
  ClearItemsBeyondMax();

end;

procedure TSDUMRUList.Add(items: TStringList);
var
  i: integer;
begin
  for i:=0 to (items.count - 1) do
    begin
    Add(items[i]);
    end;
end;

procedure TSDUMRUList.Add(item: string);
var
  idx: integer;
begin
  // Delete any existing instance of the item
  idx := Items.IndexOf(item);
  if (idx >= 0) then
    begin
    Items.Delete(idx);
    end;

  // Add the item to the head of the list
  Items.Insert(0, item);
  ClearItemsBeyondMax();

end;

// If there are more items that the max allowed, delete the oldest
procedure TSDUMRUList.ClearItemsBeyondMax();
var
  i: integer;
begin
  for i:=(Items.count - 1) downto MaxItems do
    begin
    Items.Delete(i);
    end;
    
end;

procedure TSDUMRUList.MRUItemClick(Sender: TObject);
var
  idx: integer;
begin
  if (Sender is TMenuItem) then
    begin
    idx := TMenuItem(Sender).Tag and $FFFF;
    if Assigned(FOnClick) then
      begin
      FOnClick(self, idx);
      end;
    end;

end;

function TSDUMRUList.Load(iniFile: TCustomIniFile; MRUSection: string = 'MRU'): boolean;
var
  retval: boolean;
  i: integer;
  cnt: integer;
  readItem: string;
begin
  retval := FALSE;
  try
    MaxItems := iniFile.ReadInteger(MRUSection, 'MaxItems', MaxItems);
    cnt := iniFile.ReadInteger(MRUSection, 'Items', 0);

    FItems.Clear();
    for i:=0 to (cnt-1) do
      begin
      readItem := iniFile.ReadString(MRUSection, inttostr(i), '');
      FItems.Add(readItem);
      end;

    retval := TRUE;
  except
    on E:Exception do
      begin
      // Do nothing; just swallow the error - retval already set to FALSE
      end;
  end;

  Result := retval;
end;


function TSDUMRUList.Load(reg: TSDURegistry; MRUSubKey: string = 'MRU'): boolean;
var
  retval: boolean;
  i: integer;
  cnt: integer;
  readItem: string;
begin
  retval := FALSE;
  try
    if reg.OpenKey(MRUSubKey, FALSE) then
      begin
      MaxItems := reg.ReadInteger('MaxItems', MaxItems);
      cnt := reg.ReadInteger('Items', 0);

      FItems.Clear();
      for i:=0 to (cnt-1) do
        begin
        readItem := reg.ReadString(inttostr(i), '');
        FItems.Add(readItem);
        end;

      retval := TRUE;
      end;
  except
    on E:Exception do
      begin
      // Do nothing; just swallow the error - retval already set to FALSE
      end;
  end;

  Result := retval;
end;


function TSDUMRUList.Save(iniFile: TCustomIniFile; MRUSection: string = 'MRU'): boolean;
var
  retval: boolean;
  i: integer;
begin
  retval := FALSE;
  try
    try
      iniFile.EraseSection(MRUSection);
    except
      on E:Exception do
        begin
        // Do nothing; section may not have existed
        end;
    end;

    iniFile.WriteInteger(MRUSection, 'MaxItems', MaxItems);
    iniFile.WriteInteger(MRUSection, 'Items', Items.count);

    for i:=0 to (FItems.count-1) do
      begin
      iniFile.WriteString(MRUSection, inttostr(i), Items[i]);
      end;

    retval := TRUE;
  except
    on E:Exception do
      begin
      // Do nothing; just swallow the error - retval already set to FALSE
      end;
  end;

  Result := retval;
end;


function TSDUMRUList.Save(reg: TSDURegistry; MRUSubKey: string = 'MRU'): boolean;
var
  retval: boolean;
  i: integer;
begin
  retval := FALSE;
  try
    reg.DeleteKey(MRUSubKey);
    if reg.OpenKey(MRUSubKey, TRUE) then
      begin
      reg.WriteInteger('MaxItems', MaxItems);
      reg.WriteInteger('Items', Items.count);

      for i:=0 to (FItems.count-1) do
        begin
        reg.WriteString(inttostr(i), Items[i]);
        end;
        
      retval := TRUE;
      end;
  except
    on E:Exception do
      begin
      // Do nothing; just swallow the error - retval already set to FALSE
      end;
  end;

  Result := retval;
end;

END.

