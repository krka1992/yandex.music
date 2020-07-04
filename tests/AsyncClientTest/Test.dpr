program Test;

uses
  Vcl.Forms,
  AsyncHTTPClientTest in 'AsyncHTTPClientTest.pas' {Form1},
  ymHTTPClient in '..\..\source\ymHTTPClient.pas',
  ymFace in '..\..\source\ymFace.pas',
  ymHTTPClientNotifies in '..\..\source\ymHTTPClientNotifies.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
