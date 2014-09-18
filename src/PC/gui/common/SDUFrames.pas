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
  Classes, Controls, Dialogs, Forms,
  Graphics, Messages, SysUtils, Variants, Windows;

type
  TSDUFrame = class (TFrame)
  PRIVATE
    FOnLoaded: TNotifyEvent;
    FOnHide:   TNotifyEvent;
    FOnShow:   TNotifyEvent;
    procedure CMShowingChanged(var Message: TMessage); MESSAGE CM_SHOWINGCHANGED;
  PROTECTED
    procedure Loaded(); OVERRIDE;

    procedure DoLoaded();
    procedure DoHide(); DYNAMIC;
    procedure DoShow(); DYNAMIC;

  PUBLISHED
    property OnLoaded: TNotifyEvent Read FOnLoaded Write FOnLoaded;
    property OnShow: TNotifyEvent Read FOnShow Write FOnShow;
    property OnHide: TNotifyEvent Read FOnHide Write FOnHide;

  end;

implementation

{$R *.dfm}

procedure TSDUFrame.Loaded();
begin
  inherited;

  if not (csDesigning in ComponentState) then begin
    DoLoaded();
  end;
end;

procedure TSDUFrame.CMShowingChanged(var Message: TMessage);
begin
  inherited;

  if not (csDesigning in ComponentState) then begin
    if Showing then begin
      try
        DoShow;
      except
        Application.HandleException(Self);
      end;
    end;
  end else begin
    try
      DoHide;
    except
      Application.HandleException(Self);
    end;
  end;

end;

procedure TSDUFrame.DoLoaded();
begin
  if Assigned(FOnLoaded) then
    FOnLoaded(Self);
end;

procedure TSDUFrame.DoHide();
begin
  if Assigned(FOnHide) then
    FOnHide(Self);
end;

procedure TSDUFrame.DoShow();
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

end.
