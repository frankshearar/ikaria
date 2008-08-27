{
  (c) 2008 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
program ActorFramework;

uses
  Forms,
  GuiTestRunner,
  ClientTcpConnectionActor in 'src\ClientTcpConnectionActor.pas',
  Ikaria in 'src\Ikaria.pas',
  PingPong in 'src\PingPong.pas' {PingPongDemo},
  TestIkaria in 'test\TestIkaria.pas',
  TestClientTcpConnectionActor in 'test\TestClientTcpConnectionActor.pas';

{$R *.res}

//{$DEFINE DEMO}

begin
  {$IFNDEF DEMO}
  GuiTestRunner.RunRegisteredTests;
  {$ELSE}
  Application.Initialize;
  Application.CreateForm(TPingPongDemo, PingPongDemo);
  Application.Run;
  {$ENDIF}
end.
