unit SDUFrames;
// Description: Base frame
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs;

type
  TSDUFrame = class(TFrame)
  private
    FOnLoaded: TNotifyEvent;
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
  protected
    procedure Loaded(); override;

    procedure DoLoaded();
    procedure DoHide(); dynamic;
    procedure DoShow(); dynamic;

  published
    property OnLoaded: TNotifyEvent read FOnLoaded write FOnLoaded;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;

  end;

implementation

{$R *.dfm}

procedure TSDUFrame.Loaded();
begin
  inherited;

  if not(csDesigning in ComponentState) then
    begin
    DoLoaded();
    end;
end;

procedure TSDUFrame.CMShowingChanged(var Message: TMessage);
begin
  inherited;

  if not(csDesigning in ComponentState) then
    begin
    if Showing then
      begin
      try
        DoShow;
      except
        Application.HandleException(Self);
      end;
      end;
    end
  else
    begin
    try
      DoHide;
    except
      Application.HandleException(Self);
    end;
    end;

end;

procedure TSDUFrame.DoLoaded();
begin
  if Assigned(FOnLoaded) then FOnLoaded(Self);
end;

procedure TSDUFrame.DoHide();
begin
  if Assigned(FOnHide) then FOnHide(Self);
end;

procedure TSDUFrame.DoShow();
begin
  if Assigned(FOnShow) then FOnShow(Self);
end;

END.



