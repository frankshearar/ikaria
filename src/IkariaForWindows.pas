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
  // ("message-queue-handle" {your-pid} (HWND)) and I pass on all other messages
  // to that handle.
  TWindowsMessageForwarder = class(TActor)
  private
    TargetHandle: HWND;

    function  MatchMessageQueueHandle(Msg: TTuple): Boolean;
    procedure ForwardToMessageQueue(Msg: TTuple);
    procedure SetMessageQueueHandle(Msg: TTuple);
  protected
    procedure BeforeExit(Msg: TTuple); override;
    procedure RegisterActions(Table: TActorMessageTable); override;
  end;

  TMessageQueueHandleMsg = class(TMessageTuple)
  private
    function GetHandle: HWND;
  public
    constructor Create(ReplyTo: TProcessID; Handle: HWND);

    property Handle: HWND read GetHandle;
  end;

const
  MessageQueueHandleName = 'message-queue-handle';
  WM_IKARIA_MSG = WM_USER + 10000;

implementation

//******************************************************************************
//* TWindowsMessageForwarder                                                   *
//******************************************************************************
//* TWindowsMessageForwarder Protected methods *********************************

procedure TWindowsMessageForwarder.BeforeExit(Msg: TTuple);
begin
  inherited BeforeExit(Msg);

  // Notify the message queue of my exit.
  Self.ForwardToMessageQueue(Msg);
end;

procedure TWindowsMessageForwarder.RegisterActions(Table: TActorMessageTable);
begin
  Table.Add(Self.MatchMessageQueueHandle, Self.SetMessageQueueHandle);
  Table.Add(Self.MatchAny, Self.ForwardToMessageQueue);
end;

//* TWindowsMessageForwarder Private methods ***********************************

function TWindowsMessageForwarder.MatchMessageQueueHandle(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, MessageQueueHandleName);
end;

procedure TWindowsMessageForwarder.ForwardToMessageQueue(Msg: TTuple);
begin
  // * Map the Msg to a (Windows) Message;
  // * dump the tuple into the WParam;
  // * send to a Windows message queue.

  PostMessage(Self.TargetHandle, WM_IKARIA_MSG, WPARAM(Msg.Copy), 0);
end;

procedure TWindowsMessageForwarder.SetMessageQueueHandle(Msg: TTuple);
var
  O: TMessageQueueHandleMsg;
begin
  O := TMessageQueueHandleMsg.Overlay(Msg);
  try
    Self.TargetHandle := O.Handle;
  finally
    O.Free;
  end;
end;

//******************************************************************************
//* TMessageQueueHandleMsg                                                     *
//******************************************************************************
//* TMessageQueueHandleMsg Public methods **************************************

constructor TMessageQueueHandleMsg.Create(ReplyTo: TProcessID; Handle: HWND);
var
  Params: TTuple;
begin
  Params := TTuple.Create;
  try
    Params.AddInteger(Handle);
    
    inherited Create(MessageQueueHandleName, ReplyTo, Params);
  finally
    Params.Free;
  end;
end;

//* TMessageQueueHandleMsg Private methods *************************************

function TMessageQueueHandleMsg.GetHandle: HWND;
begin
  Result := (Self.Parameters[0] as TIntegerTerm).Value;
end;

end.
