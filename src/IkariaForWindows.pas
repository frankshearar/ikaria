unit IkariaForWindows;

interface

uses
  Ikaria, Messages, Windows;

type
  // I translate messages sent to me into PostMessages to a Windows message
  // queue. In particular, I put a copy of the data of a received TActorMessage
  // in the WParam of a TMessage. You, as recipient of the TMessage, are
  // responsible for freeing the embedded TTuple.
  //
  // You pass me a target handle with the message
  // ("message-queue-handle {your-pid} (HWND)) and I pass on all other messages
  // to that handle.
  TWindowsMessageForwarder = class(TActor)
  private
    TargetHandle: HWND;

    function  MatchAny(Msg: TActorMessage): Boolean;
    function  MatchMessageQueueHandle(Msg: TActorMessage): Boolean;
    procedure ForwardToMessageQueue(Msg: TActorMessage);
    procedure SetMessageQueueHandle(Msg: TActorMessage);
  protected
    procedure RegisterActions(Table: TActorMessageTable); override;
  end;

const
  MessageQueueHandleName = 'message-queue-handle';
  WM_IKARIA_MSG = WM_USER + 10000;

implementation

//******************************************************************************
//* TWindowsMessageForwarder                                                   *
//******************************************************************************
//* TWindowsMessageForwarder Protected methods *********************************

procedure TWindowsMessageForwarder.RegisterActions(Table: TActorMessageTable);
begin
  Table.Add(Self.MatchMessageQueueHandle, Self.SetMessageQueueHandle);
  Table.Add(Self.MatchAny, Self.ForwardToMessageQueue);
end;

//* TWindowsMessageForwarder Private methods ***********************************

function TWindowsMessageForwarder.MatchAny(Msg: TActorMessage): Boolean;
begin
  Result := true;
end;

function TWindowsMessageForwarder.MatchMessageQueueHandle(Msg: TActorMessage): Boolean;
begin
  Result := Self.MatchMessageName(Msg, MessageQueueHandleName);
end;

procedure TWindowsMessageForwarder.ForwardToMessageQueue(Msg: TActorMessage);
begin
  // * Map the Msg to a (Windows) Message;
  // * dump the tuple into the WParam;
  // * send to a Windows message queue.

  PostMessage(Self.TargetHandle, WM_IKARIA_MSG, WPARAM(Msg.Data.Copy), 0);
end;

procedure TWindowsMessageForwarder.SetMessageQueueHandle(Msg: TActorMessage);
var
  O: TMessageTuple;
begin
  O := TMessageTuple.Overlay(Msg.Data);
  try
    Self.TargetHandle := (O.Parameters[0] as TIntegerElement).Value;
  finally
    O.Free;
  end;
end;

end.
