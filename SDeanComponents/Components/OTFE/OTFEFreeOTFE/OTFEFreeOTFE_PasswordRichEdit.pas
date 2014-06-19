unit OTFEFreeOTFE_PasswordRichEdit;

interface

uses
  Menus, Classes,
  PasswordRichEdit;

type
  TOTFEFreeOTFE_PasswordRichEdit = class(TPasswordRichEdit)
  private
    FInternalPopupMenu: TPopupMenu;

  protected
    procedure SetPopupMenu(newMenu: TPopupMenu);
    function GetPopupMenuFilterInternal(): TPopupMenu; 

    procedure PasteClicked(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy(); override;

  published
    property PopupMenu: TPopupMenu read GetPopupMenuFilterInternal;// write SetPopupMenu;

  end;


procedure Register;

implementation

resourcestring
  RS_PASTE = 'Paste';

procedure Register;
begin
  RegisterComponents('FreeOTFE', [TOTFEFreeOTFE_PasswordRichEdit]);
end;

constructor TOTFEFreeOTFE_PasswordRichEdit.Create(AOwner: TComponent);
var
  pasteMenuItem: TMenuItem;
begin
  inherited;
  
  FInternalPopupMenu:= TPopupMenu.Create(nil);

  pasteMenuItem := TMenuItem.Create(FInternalPopupMenu);
  pasteMenuItem.OnClick := PasteClicked;
  pasteMenuItem.Caption := RS_PASTE;
  pasteMenuItem.ShortCut := TextToShortCut('CTRL+V');

  FInternalPopupMenu.Items.Add(pasteMenuItem);

  SetPopupMenu(FInternalPopupMenu);
end;

destructor TOTFEFreeOTFE_PasswordRichEdit.Destroy();
begin
  if (GetPopupMenu = FInternalPopupMenu) then
    begin
    SetPopupMenu(nil);
    end;

  FInternalPopupMenu.Free();

  inherited;
end;

procedure TOTFEFreeOTFE_PasswordRichEdit.SetPopupMenu(newMenu: TPopupMenu);
begin
// Don't set to FInternalPopupMenu; Destroy() calls this with nil, then free's
// off FInternalPopupMenu! 
//  if (newMenu = nil) then
//    begin
//    newMenu := FInternalPopupMenu;
//    end;

  inherited PopupMenu := newMenu;
end;

function TOTFEFreeOTFE_PasswordRichEdit.GetPopupMenuFilterInternal(): TPopupMenu;
var
  retval: TPopupMenu;
begin
  retval := inherited GetPopupMenu;
  if (retval = FInternalPopupMenu) then
    begin
    retval := nil;
    end;

  Result := retval
end;

procedure TOTFEFreeOTFE_PasswordRichEdit.PasteClicked(Sender: TObject);
begin
  self.PasteFromClipboard();

end;

END.

