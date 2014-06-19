unit pkcs11_slot_event_thread;

interface

uses
  Classes,
  pkcs11t,
  pkcs11f,
  pkcs11_api;

type
  TPKCS11SlotEventThread = class(TThread)
  private
    FFunctionList: CK_FUNCTION_LIST;
    FLastEventSlotID: integer;

    procedure ThreadSyncEvent();
  public
    LastRV: CK_RV;
    LibraryCallback: TPKCS11SlotEvent;

    property LibraryFunctionList: CK_FUNCTION_LIST read FFunctionList write FFunctionList;
    procedure Execute(); override;
  end;

implementation

uses
  SysUtils;

{ Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TPKCS11SlotEventThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TPKCS11SlotEventThread }

procedure TPKCS11SlotEventThread.Execute();
const
  POLLING_DELAY = 5000;  // 5 seconds
var
  flags: CK_FLAGS;
  slotID: CK_SLOT_ID;
begin
  while not(Terminated) do
    begin
    flags := 0;
    // flags := CKF_DONT_BLOCK;
    LastRV := LibraryFunctionList.CK_C_WaitForSlotEvent(
                                                       flags,
                                                       @slotID,
                                                       nil
                                                      );

    if (LastRV = CKR_OK) then
      begin
      FLastEventSlotID := slotID;

      // Fire event
      Synchronize(ThreadSyncEvent);
      end
    else if (LastRV = CKR_NO_EVENT) then
      begin
      // We *should* ever get here, unless CKF_DONT_BLOCK flag is set
      sleep(POLLING_DELAY);  // 5 seconds
      end
    else if (LastRV = CKR_FUNCTION_NOT_SUPPORTED) then
      begin
      // Fair enough... Just exit.
      Terminate();
      end
    else
      begin
      // Unknown error - bail out!
      Terminate();
      end;

    end;

end;

procedure TPKCS11SlotEventThread.ThreadSyncEvent();
begin
  LibraryCallback(self, FLastEventSlotID);
end;

END.

