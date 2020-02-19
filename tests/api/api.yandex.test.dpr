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
  ymFace in '..\..\source\api\ymFace.pas',
  test.obj in 'test.obj.pas',
  ymPlaylist in '..\..\source\api\ymPlaylist.pas',
  ymTrack in '..\..\source\api\ymTrack.pas';

begin
  ReportMemoryLeaksOnShutdown := true;
  try RunRegisteredTests;
  except
    on E: Exception do Writeln(E.ClassName, ': ', E.Message);
  end;

  ReadLn;

end.
