unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, SDUClipbrd, SDUForms;

type
  TForm1 = class(TSDUForm)
    reReport: TRichEdit;
    SDUClipboardMonitor1: TSDUClipboardMonitor;
    pbClose: TButton;
    pbClear: TButton;
    Label1: TLabel;
    ckStayOnTop: TCheckBox;
    ckMonitorClipboard: TCheckBox;
    pbClearClipboard: TButton;
    procedure SDUClipboardMonitor1Changed(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbCloseClick(Sender: TObject);
    procedure pbClearClick(Sender: TObject);
    procedure ckStayOnTopClick(Sender: TObject);
    procedure ckMonitorClipboardClick(Sender: TObject);
    procedure pbClearClipboardClick(Sender: TObject);
  private
    procedure DumpClipboard();
    
    procedure ListFormatsOnClipboard();

    procedure ShowContent_HDROP();
    procedure ShowContent_PreferredDropEffect();
    procedure ShowContent_FileDescriptor();
    procedure ShowContent_FileContent();
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Clipbrd, ShlObj,
  SDUGeneral;

procedure TForm1.pbClearClipboardClick(Sender: TObject);
begin
  SDUClearClipboard();
end;

procedure TForm1.ckMonitorClipboardClick(Sender: TObject);
begin
  SDUClipboardMonitor1.Enabled := ckMonitorClipboard.Checked;
end;

procedure TForm1.ckStayOnTopClick(Sender: TObject);
begin
  if ckStayOnTop.Checked then
    begin
    self.FormStyle := fsStayOnTop;
    end
  else
    begin
    self.FormStyle := fsNormal;
    end;

end;

procedure TForm1.FormShow(Sender: TObject);
begin
  self.Caption := Application.Title;

  reReport.Plaintext := TRUE;
  reReport.lines.Clear();

  DumpClipboard();

  ckMonitorClipboard.Checked := TRUE;
end;

procedure TForm1.pbClearClick(Sender: TObject);
begin
  reReport.lines.clear();
end;

procedure TForm1.pbCloseClick(Sender: TObject);
begin
  Close();
end;

procedure TForm1.SDUClipboardMonitor1Changed(Sender: TObject);
begin
 reReport.lines.add('---------------------------');
 reReport.lines.add('Clipboard contents changed!');

 DumpClipboard();

end;


procedure TForm1.DumpClipboard();
begin
 ListFormatsOnClipboard();

 if Clipboard.HasFormat(CF_HDROP) then
   begin
   ShowContent_HDROP();
   end;

 if Clipboard.HasFormat(CF_PREFERREDDROPEFFECT) then
   begin
   ShowContent_PreferredDropEffect();
   end;

 if Clipboard.HasFormat(CF_FILEDESCRIPTOR) then
   begin
   ShowContent_FileDescriptor();
   end;

 if Clipboard.HasFormat(CF_FILECONTENTS) then
   begin
   ShowContent_FileContent();
   end;

end;

procedure TForm1.ShowContent_HDROP();
var
  selected: TStringList;
  dropPoint: TPoint;
begin
  selected:= TStringList.create();
  try
    if SDUGetDropFilesFromClipboard(selected, dropPoint) then
      begin
      reReport.lines.Add('HDROP:');
      reReport.lines.Add('X: '+inttostr(dropPoint.X));
      reReport.lines.Add('Y: '+inttostr(dropPoint.Y));
      reReport.lines.Add('Files:');
      reReport.lines.AddStrings(selected);
      end
    else
      begin
      reReport.lines.Add('Unable to get HDROP from clipboard');
      end;

    reReport.lines.Add('');

  finally
    selected.Free();
  end;

end;

procedure TForm1.ShowContent_PreferredDropEffect();
var
  dropEffect: DWORD;
begin
  if SDUGetPreferredDropEffectFromClipboard(dropEffect) then
    begin
    reReport.lines.Add('Preferred DropEffect:'+inttostr(dropEffect));
    end
  else
    begin
    reReport.lines.Add('Unable to get Preferred DropEffect from clipboard');
    end;

  reReport.lines.Add('');

end;

procedure TForm1.ShowContent_FileDescriptor();
var
  fileDescriptors: TFileDescriptorArray;
  i: integer;
begin
  SetLength(fileDescriptors, 0);
  if SDUGetFileDescriptorArrayFromClipboard(fileDescriptors) then
    begin
    reReport.lines.Add('FileDescriptors:');
    for i:=low(fileDescriptors) to high(fileDescriptors) do
      begin
      reReport.lines.Add('FILEDESCRIPTOR['+inttostr(i)+']:');
      reReport.lines.Add('dwFlags: '+inttostr(fileDescriptors[i].dwFlags));

      if ((fileDescriptors[i].dwFlags and FD_CLSID) > 0) then
        begin
        reReport.lines.Add('clsid: '+GUIDToString(fileDescriptors[i].clsid));
        end;
      if ((fileDescriptors[i].dwFlags and FD_SIZEPOINT) > 0) then
        begin
        reReport.lines.Add('sizel: ('+inttostr(fileDescriptors[i].sizel.cx)+', '+inttostr(fileDescriptors[i].sizel.cy)+')');
        reReport.lines.Add('pointl: ('+inttostr(fileDescriptors[i].pointl.X)+', '+inttostr(fileDescriptors[i].pointl.Y)+')');
        end;
      if ((fileDescriptors[i].dwFlags and FD_ATTRIBUTES) > 0) then
        begin
        reReport.lines.Add('dwFileAttributes: '+inttostr(fileDescriptors[i].dwFileAttributes));
        end;
      if ((fileDescriptors[i].dwFlags and FD_CREATETIME) > 0) then
        begin
        reReport.lines.Add('ftCreationTime: '+datetimetostr(SDUFileTimeToDateTime(fileDescriptors[i].ftCreationTime)));
        end;
      if ((fileDescriptors[i].dwFlags and FD_ACCESSTIME) > 0) then
        begin
        reReport.lines.Add('ftLastAccessTime: '+datetimetostr(SDUFileTimeToDateTime(fileDescriptors[i].ftLastAccessTime)));
        end;
      if ((fileDescriptors[i].dwFlags and FD_WRITESTIME) > 0) then
        begin
        reReport.lines.Add('ftLastWriteTime: '+datetimetostr(SDUFileTimeToDateTime(fileDescriptors[i].ftLastWriteTime)));
        end;
      if ((fileDescriptors[i].dwFlags and FD_FILESIZE) > 0) then
        begin
        reReport.lines.Add('nFileSizeHigh: '+inttostr(fileDescriptors[i].nFileSizeHigh));
        reReport.lines.Add('nFileSizeLow: '+inttostr(fileDescriptors[i].nFileSizeLow));
        end;
{
      if ((fileDescriptors[i].dwFlags and FD_PROGRESSUI) > 0) then
        begin
        reReport.lines.Add('FD_PROGRESSUI set');
        end;
}
{
      if ((fileDescriptors[i].dwFlags and FD_LINKUI) > 0) then
        begin
        reReport.lines.Add('FD_LINKUI set');
        end;
}
{
      if ((fileDescriptors[i].dwFlags and FD_UNICODE) > 0) then
        begin
        reReport.lines.Add('FD_UNICODE set');
        end;
}

      reReport.lines.Add('cFileName: '+fileDescriptors[i].cFileName);
      end;
    end
  else
    begin
    reReport.lines.Add('Unable to get FileDescriptors from clipboard');
    end;

  reReport.lines.Add('');

end;

procedure TForm1.ShowContent_FileContent();
var
  data: string;
  stlBinary: TStringList;
begin
  if SDUGetFileContentFromClipboard(data) then
    begin
    reReport.lines.Add('CF_FILECONTENT:');
    stlBinary:= TStringList.Create();
    try
      SDUPrettyPrintHex(data, 0, length(data), stlBinary);
      reReport.lines.AddStrings(stlBinary);
    finally
      stlBinary.Free();
    end;

    end
  else
    begin
    reReport.lines.Add('Unable to get Preferred DropEffect from clipboard');
    end;

  reReport.lines.Add('');

end;

procedure TForm1.ListFormatsOnClipboard();
const
  NAME_BUFFER_SIZE = 1024;
var
  i: integer;
  dispName: string;
  stlFormats: TStringList;
begin
  if (Clipboard.FormatCount <= 0) then
    begin
    reReport.lines.Add('Clipboard empty.');
    end
  else
    begin
    stlFormats:= TStringList.Create();
    try
      for i:=0 to (Clipboard.FormatCount - 1) do
        begin
        dispName := '0x'+inttohex(Clipboard.Formats[i], 8) + ' - ' + SDUClipboardFormatToStr(Clipboard.Formats[i]);
        stlFormats.add(dispName);
        end;

      stlFormats.Sort();
      reReport.lines.Add('Formats currently on clipboard:');
      reReport.lines.AddStrings(stlFormats);

    finally
      stlFormats.Free();
    end;

    end;

  reReport.lines.Add('');

end;


END.

