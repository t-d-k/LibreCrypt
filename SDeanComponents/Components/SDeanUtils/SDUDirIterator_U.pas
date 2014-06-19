unit SDUDirIterator_U;
// Description: Directory Iterator
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type

  TSDUDirIteratorLevel = class(TObject)
  public
    items: TStringList;
    itemPos: integer;
    parentDir: string;

    constructor Create();
    destructor  Destroy(); override;
  end;

  TSDUDirIterator = class(TComponent)
  private
    searchLevel: TStringList;
  protected
    FRootDirectory: string;
    FDirMask: string;
    FReverseFormat: boolean;
    FIncludeStartDir: boolean;
    function SlashSep(const Path, S: String): String;
    function  StripTrailSlash(theDir: string): string;
    procedure EnterDir(theDir: string);
    procedure KickEnterDir(theDir: string);
    procedure ExitDir();
    procedure SetRootDirectory(const theDir: string);
    procedure SetDirMask(const theDirMask: string);
    procedure ClearSearchLevel();
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy(); override;

    procedure Reset();
    function  Next(): string;
  published
    property Directory: string read FRootDirectory write SetRootDirectory;
    property DirMask: string read FDirMask write SetDirMask;
    property ReverseFormat: boolean read FReverseFormat write FReverseFormat default FALSE;
    property IncludeStartDir: boolean read FIncludeStartDir write FIncludeStartDir default TRUE;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SDeanUtils', [TSDUDirIterator]);
end;

constructor TSDUDirIteratorLevel.Create();
begin
  inherited;
  items := TStringList.Create();
  itemPos:= -1;
  parentDir:= '';

end;

destructor TSDUDirIteratorLevel.Destroy();
begin
  items.Free();
  inherited;

end;

constructor TSDUDirIterator.Create(AOwner: TComponent);
begin
  inherited;
  searchLevel := TStringList.Create();
  FDirMask := '*.*';

end;

destructor TSDUDirIterator.Destroy();
begin
  ClearSearchLevel();
  searchLevel.Free();
  inherited;

end;

procedure TSDUDirIterator.Reset();
begin
  ClearSearchLevel();

  KickEnterDir(FRootDirectory);

end;

function TSDUDirIterator.Next(): string;
var
  dl: TSDUDirIteratorLevel;
begin
  Result := '';
  if searchLevel.count=0 then
    begin
    exit;
    end;

  dl := TSDUDirIteratorLevel(searchLevel.Objects[(searchLevel.count-1)]);

  inc(dl.itemPos);
  if FReverseFormat then
    begin
    if dl.itemPos>((dl.items.count-1)) then
      begin
      ExitDir();
      if searchLevel.count=0 then
        begin
        exit;
        end;
      dl := TSDUDirIteratorLevel(searchLevel.Objects[(searchLevel.count-1)]);
      Result := StripTrailSlash(SlashSep(dl.parentDir, dl.items[dl.itemPos]));
      exit;
      end
    else
      begin
      EnterDir(SlashSep(dl.parentDir, dl.items[dl.itemPos]));
      Result := Next();
      end;
    end // if FReverseFormat
  else
    begin
    if dl.itemPos>((dl.items.count-1)) then
      begin
      ExitDir();
      Result := Next();
      exit;
      end
    else
      begin
      Result := SlashSep(dl.parentDir, dl.items[dl.itemPos]);
      EnterDir(SlashSep(dl.parentDir, dl.items[dl.itemPos]));
      end;
    end;

  Result := StripTrailSlash(Result);

end;


procedure TSDUDirIterator.EnterDir(theDir: string);
var
  dl: TSDUDirIteratorLevel;
  SearchRec: TSearchRec;
  Status: integer;
begin
  dl := TSDUDirIteratorLevel.Create();

  dl.parentDir := theDir;

  Status := FindFirst(SlashSep(theDir, FDirMask), faDirectory, SearchRec);

  while (Status=0) do
    begin
    if ((SearchRec.Attr AND faDirectory) = faDirectory) then
      begin
      if (SearchRec.Name<>'.') AND (SearchRec.Name<>'..') then
        begin
        dl.items.add(SearchRec.Name);
        end;
      end;
    Status := FindNext(SearchRec);
    end;

  FindClose(SearchRec);

  searchLevel.AddObject('', dl);

end;

procedure TSDUDirIterator.KickEnterDir(theDir: string);
var
  dl: TSDUDirIteratorLevel;
begin
  if theDir<>'' then
    begin
    dl := TSDUDirIteratorLevel.Create();

    theDir := StripTrailSlash(theDir);

    dl.parentDir := theDir;
    dl.items.add('');

    searchLevel.AddObject('', dl);
    end;

end;

procedure TSDUDirIterator.ExitDir();
begin
  searchLevel.objects[(searchLevel.count-1)].Free();
  searchLevel.delete((searchLevel.count-1));

end;


function TSDUDirIterator.SlashSep(const Path, S: String): String;
begin
  if AnsiLastChar(Path)^ <> '\' then
    Result := Path + '\' + S
  else
    Result := Path + S;
end;

function TSDUDirIterator.StripTrailSlash(theDir: string): string;
begin

  if length(theDir)>2 then
    begin
    if (theDir[length(theDir)-1]<>':') AND
       (theDir[length(theDir)]='\') then
      begin
      delete(theDir, length(theDir), 1);
      end;
    end;

  Result := theDir;

end;

procedure TSDUDirIterator.SetRootDirectory(const theDir: string);
begin
  FRootDirectory := theDir;
  Reset();

end;

procedure TSDUDirIterator.SetDirMask(const theDirMask: string);
begin
  FDirMask := theDirMask;
  Reset();

end;


procedure TSDUDirIterator.ClearSearchLevel();
var
  i: integer;
begin
  for i:=0 to (searchLevel.count-1) do
    begin
    searchLevel.objects[i].Free();
    end;

  searchLevel.Clear();

end;



END.

