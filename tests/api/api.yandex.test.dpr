program api.yandex.test;

{$APPTYPE CONSOLE}
{$R *.res}
{.$DEFINE CHECK_LEACKS}

uses
  {$IFDEF CHECK_LEACKS}
    fastmm4,
  {$ENDIF }
  DUnitTestRunner,
  System.SysUtils,
  test in 'test.pas',
  ymApi in '..\..\source\api\ymApi.pas',
  ymFace in '..\..\source\ymFace.pas',
  test.obj in 'test.obj.pas',
  ymPlaylist in '..\..\source\api\ymPlaylist.pas',
  ymTrack in '..\..\source\api\ymTrack.pas',
  ymUser in '..\..\source\api\ymUser.pas',
  ymFeed in '..\..\source\api\ymFeed.pas',
  ymHTTPClient in '..\..\source\ymHTTPClient.pas',
  test.async.client.http in 'test.async.client.http.pas';

begin
  ReportMemoryLeaksOnShutdown := true;
  try
    RunRegisteredTests;
  except
    on E: Exception do Writeln(E.ClassName, ': ', E.Message);
  end;

  ReadLn;

end.
