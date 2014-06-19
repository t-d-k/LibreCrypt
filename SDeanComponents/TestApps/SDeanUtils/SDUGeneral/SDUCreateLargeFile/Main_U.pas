unit Main_U;
// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin64;

type
  TMain_F = class(TForm)
    pbCreate: TButton;
    edFilename: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    se64FileSize: TSpinEdit64;
    cbUnits: TComboBox;
    pbClose: TButton;
    cbShowProgress: TCheckBox;
    SaveDialog1: TSDUSaveDialog;
    pbBrowse: TButton;
    procedure pbCreateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbCloseClick(Sender: TObject);
    procedure pbBrowseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Main_F: TMain_F;

implementation

{$R *.DFM}

uses
  SDUGeneral;

procedure TMain_F.pbCreateClick(Sender: TObject);
var
  size: int64;
  userCancelled: boolean;
begin
  // Determine the size of file to create...
  size := se64FileSize.Value;
  if (cbUnits.Items[cbUnits.ItemIndex] = 'KB') then
    begin
    size := size * int64(1024);
    end;

  if (cbUnits.Items[cbUnits.ItemIndex] = 'MB') then
    begin
    size := size * int64(1024) * int64(1024);
    end;

  if (cbUnits.Items[cbUnits.ItemIndex] = 'GB') then
    begin
    size := size * int64(1024) * int64(1024) * int64(1024);
    end;


  if SDUCreateLargeFile(
                        edFilename.text,
                        size,
                        cbShowProgress.checked,
                        userCancelled
                       ) then
    begin
    MessageDlg('File created successfully.', mtInformation, [mbOK], 0);
    end
  else if (userCancelled) then
    begin
    MessageDlg('User cancelled operation', mtInformation, [mbOK], 0);
    end
  else
    begin
    MessageDlg(
               'Unable to create file.'+SDUCRLF+
               SDUCRLF+
               'This is probably due to either the file already existing, or '+SDUCRLF+
               'there being insufficient free space on this drive.',
               mtError, [mbOK], 0
              );
    end;

end;

procedure TMain_F.FormCreate(Sender: TObject);
begin
  self.Caption := Application.Title;

  cbUnits.Items.Clear();
  cbUnits.Items.Add('Bytes');
  cbUnits.Items.Add('KB');
  cbUnits.Items.Add('MB');
  cbUnits.Items.Add('GB');

  edFilename.Text := 'C:\TestFile.dat';
  se64FileSize.Value := 10;
  cbUnits.ItemIndex := cbUnits.Items.IndexOf('MB');
  cbShowProgress.checked := TRUE;

end;

procedure TMain_F.pbCloseClick(Sender: TObject);
begin
  Close();
  
end;

procedure TMain_F.pbBrowseClick(Sender: TObject);
begin
  SDUOpenSaveDialogSetup(SaveDialog1, edFilename.Text);

  if (SaveDialog1.Execute) then
    begin
    edFilename.Text := SaveDialog1.Filename;
    end;

end;

END.


