unit SDUFileIterator_U;
// Description: File Iterator
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SDUDirIterator_U;

type

  TSDUFileIterator = class(TComponent)
  private
    currDir: string;
    currDirsFiles: TStringList;
    currFileIdx: integer;
    dirIterator: TSDUDirIterator;
    procedure ReadFilenames(theDir: string);
  protected
    FRootDirectory: string;
    FFileMask: string;
    FSorted: boolean;
    FRecurseSubDirs: boolean;
    FOmitStartDirPrefix: boolean;
    FIncludeDirNames: boolean;
    function SlashSep(const Path, S: String): String;
    procedure SetRootDirectory(const theDir: string);
    procedure SetFileMask(const theFileMask: string);
    procedure SortedFilenames(sort: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure Reset();
    function  Next(): string;
    function  Count(): integer;
  published
    property Directory: string read FRootDirectory write SetRootDirectory;
    property FileMask: string read FFileMask write SetFileMask;
    property Sorted: boolean read FSorted write SortedFilenames;
    property RecurseSubDirs: boolean read FRecurseSubDirs write FRecurseSubDirs;
    property OmitStartDirPrefix: boolean read FOmitStartDirPrefix write FOmitStartDirPrefix;

    // If this is set, the directory names will be output, as well as filenames
    // Set to FALSE by default
    property IncludeDirNames: boolean read FIncludeDirNames write FIncludeDirNames;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SDeanUtils', [TSDUFileIterator]);
end;

constructor TSDUFileIterator.Create(AOwner: TComponent);
begin
  inherited;
  currDirsFiles := TStringList.Create();
  currFileIdx:=0;

  FRootDirectory := '.';
  FFileMask := '*.*';

  FOmitStartDirPrefix := FALSE;
  FIncludeDirNames := FALSE;

  dirIterator:= TSDUDirIterator.Create(nil);

end;

destructor TSDUFileIterator.Destroy();
begin
  dirIterator.Free();
  currDirsFiles.Free();
  inherited;
  
end;

procedure TSDUFileIterator.Reset();
begin
  dirIterator.Directory := FRootDirectory;
  dirIterator.Reset();
  ReadFilenames(dirIterator.Next());

end;

function TSDUFileIterator.Count(): integer;
var
  cnt: integer;
begin
  cnt := 0;

  Reset();
  while (Next() <> '') do
    begin
    inc(cnt);
    end;

  Reset();

  Result := cnt;

end;


function TSDUFileIterator.Next(): string;
var
  nextDir: string;
  returnFilename: string;
begin
  Result := '';

  while currFileIdx>=currDirsFiles.count do
    begin
    if not(RecurseSubDirs) then
      begin
      exit;
      end;

    nextDir := dirIterator.Next();
    if nextDir='' then
      begin
      // No more directories to process; exit while return value is still ''
      exit;
      end
    else
      begin
      ReadFilenames(nextDir);
      end;
    end;

  // At this point, we know that there's a file to return
  returnFilename := SlashSep(currDir, currDirsFiles[currFileIdx]);
  inc(currFileIdx);


//xxx -   if (returnFilename = SlashSep(Directory, '')) then
//xxx -     begin
//xxx -     Result := Next();
//xxx -     exit;
//xxx -     end;

  // If we are to omit the starting directory from items returned, remove it
  // now.
  if OmitStartDirPrefix then
    begin
    Delete(returnFilename, 1, Length(Directory));
    // Remove any starting slash
    if (returnFilename[1] = '\') then
      begin
      Delete(returnFilename, 1, 1);
      end;

    // If the result is an empty string, then
    if (returnFilename = '') then
      begin
      returnFilename := 'ZZZZ';
      end;
    end;

  Result := returnFilename;

end;


function TSDUFileIterator.SlashSep(const Path, S: String): String;
begin
  if AnsiLastChar(Path)^ <> '\' then
    Result := Path + '\' + S
  else
    Result := Path + S;
end;


procedure TSDUFileIterator.ReadFilenames(theDir: string);
var
  SearchRec: TSearchRec;
  Status: integer;
begin
  currDirsFiles.Clear();

  if theDir<>'' then
    begin
    Status := FindFirst(SlashSep(theDir, FFileMask), faAnyFile, SearchRec);

    while (Status=0) do
      begin
      if (FIncludeDirNames or ((SearchRec.Attr AND faDirectory) <> faDirectory)) then
        begin
        // Ignore "." and ".." entries
        if ((SearchRec.Name <> '.') and (SearchRec.Name <> '..')) then
          begin
          currDirsFiles.add(SearchRec.Name);
          end;

        end;
      Status := FindNext(SearchRec);
      end;

    FindClose(SearchRec);
    end;

  currDirsFiles.Sorted := FSorted;

  currFileIdx:=0;
  currDir := theDir;

end;


procedure TSDUFileIterator.SetRootDirectory(const theDir: string);
begin
  FRootDirectory := theDir;
  Reset();

end;

procedure TSDUFileIterator.SetFileMask(const theFileMask: string);
var
  tmpDir: string;
begin
  tmpDir := ExtractFilePath(theFileMask);
  if (tmpDir <> '') then
    begin
    Directory := tmpDir;
    end;

  FFileMask := ExtractFileName(theFileMask);
  Reset();

end;

procedure TSDUFileIterator.SortedFilenames(sort: boolean);
begin
  FSorted:= sort;
  currDirsFiles.Sorted := FSorted;

end;

END.

