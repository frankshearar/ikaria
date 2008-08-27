unit TestIkariaForWindows;

interface

uses
  Ikaria, IkariaForWindows, Messages, TestIkaria, TestFramework, Windows;

type
  TestTWindowsMessageForwarder = class(TActorTestCase)
  private
    Fwd:      TProcessID;
    Target:   HWND;
    Messages: TTuple;

    procedure MessageHandler(var Msg: TMessage);
    function  ProcessMessage(var M: TMsg): Boolean;
    procedure ProcessMessages;
    procedure SetHandler(Target: HWND);
    procedure WaitForMessage(Timeout: Cardinal; Msg: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSetMessageQueueHandler;
  end;

implementation

uses
  Classes, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('Ikaria for Windows unit tests');
  Result.AddSuite(TestTWindowsMessageForwarder.Suite);
end;

//******************************************************************************
//* TestTWindowsMessageForwarder                                               *
//******************************************************************************
//* TestTWindowsMessageForwarder Public methods ********************************

procedure TestTWindowsMessageForwarder.SetUp;
begin
  inherited SetUp;

  Self.Fwd      := Spawn(TWindowsMessageForwarder);
  Self.Messages := TTuple.Create;
  Self.Target   := AllocateHWnd(Self.MessageHandler);
end;

procedure TestTWindowsMessageForwarder.TearDown;
begin
  DeallocateHWnd(Self.Target);
  Self.Messages.Free;
  Kill(Self.Fwd);

  inherited TearDown;
end;

//* TestTWindowsMessageForwarder Private methods *******************************

procedure TestTWindowsMessageForwarder.MessageHandler(var Msg: TMessage);
var
  T: TTuple;
begin
  if (Msg.Msg = WM_IKARIA_MSG) then begin
    T := TTuple(Msg.WParam);
    Messages.Add(T);
    T.Free;
  end;
end;

function TestTWindowsMessageForwarder.ProcessMessage(var M: TMsg): Boolean;
begin
  Result := PeekMessage(M, Self.Target, WM_USER, WM_IKARIA_MSG, PM_REMOVE);

  if Result then
    DispatchMessage(M);
end;

procedure TestTWindowsMessageForwarder.ProcessMessages;
var
  M: TMsg;
begin
  while Self.ProcessMessage(M) do;
end;

procedure TestTWindowsMessageForwarder.SetHandler(Target: HWND);
var
  SetMH:  TMessageTuple;
  Handle: TTuple;
begin
  Handle := TTuple.Create;
  try
    Handle.AddInteger(Self.Target);

    SetMH := TMessageTuple.Create(MessageQueueHandleName, '', Handle);
    try
      SendActorMessage(Self.Fwd, SetMH);
    finally
      SetMH.Free;
    end;
  finally
    Handle.Free;
  end;
end;

procedure TestTWindowsMessageForwarder.WaitForMessage(Timeout: Cardinal; Msg: String);
var
  EndTime: TDateTime;
  M:       TMsg;
  Received: Boolean;
begin
  EndTime := Now + Timeout*OneMillisecond;

  repeat
    Received := PeekMessage(M, Self.Target, WM_USER, WM_IKARIA_MSG, PM_NOREMOVE)      
            and (M.message = WM_IKARIA_MSG);
    Sleep(50);
  until (Now > EndTime) or Received;

  if not Received then Fail(Msg);
end;

//* TestTWindowsMessageForwarder Published methods *****************************

procedure TestTWindowsMessageForwarder.TestSetMessageQueueHandler;
const
  TestMsg = 'test';
var
  Msg: TMessageTuple;
begin
  CheckEquals(0, Self.Messages.Count, 'Sanity check: message list should be empty');

  Self.SetHandler(Self.Target);

  Msg := TMessageTuple.Create(TestMsg, '');
  try
    SendActorMessage(Self.Fwd, Msg);

    Self.WaitForMessage(OneSecond, 'Timeout waiting for a message');
    Self.ProcessMessages;

    CheckEquals(1, Self.Messages.Count, 'Message not received');
    CheckEquals(TMessageTuple, Self.Messages[0].ClassType, 'Unexpected message type');
    CheckEquals(TestMsg, (Self.Messages[0] as TMessageTuple).MessageName, 'Unexpected message');
  finally
    Msg.Free;
  end;
end;

initialization;
  RegisterTest('Ikaria for Windows', Suite);
end.
