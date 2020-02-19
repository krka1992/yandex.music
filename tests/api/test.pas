unit test;

interface

uses sysutils, TestFramework, ymApi, ymPlaylist, ymTrack,
  JSON;

type

  TApiTest = class(TTestCase)
  private
    FApi: TymApi;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure Auth;
    procedure TestDownload;
    procedure TestGetMp3;
  published
    procedure TestGetPlaylist;
  end;

implementation

function GetAccountInfo(var login, pass: String): Boolean;
var
  FN: String;
  F: TextFile;
begin
  FN := ExtractFilePath(ParamStr(0)) + 'ym_account.env';
  Assign(F, FN);
  try
    Reset(F);
    ReadLn(F, login);
    ReadLn(F, pass);
  finally Close(F);
  end;
end;

procedure TApiTest.SetUp;
begin
  inherited;
  FApi := TymApi.Create;
  Auth;
end;

procedure TApiTest.TearDown;
begin
  inherited;
  FApi._Release;
end;

procedure TApiTest.Auth;
var
  l, p: String;
begin
  GetAccountInfo(l, p);
  CheckTrue(FApi.Auth(l, p));
end;

procedure TApiTest.TestDownload;
var
  Res: TJSONObject;
begin
  //Res := FApi._DownloadInfo('52799249');
end;

procedure TApiTest.TestGetMp3;
var
  Mp3: TymTrack;
begin
  Mp3 := FApi.GetTrack('52799249');
  try
    Mp3.Load();
    Mp3.Save('52799249.mp3');
  finally Mp3.Free;
  end;
end;

procedure TApiTest.TestGetPlaylist;
var
  Playlist: TymPlaylist;
  Track: TymTrack;
  TrackStr: String;
  i: Integer;
begin
  WriteLn;
  Playlist := FApi.GetUserPlaylist('57477732');
  WriteLn(Format('PlaylistID: %s. Track count: %d', ['57477732', Playlist.GetCount]));
  for i := 0 to Playlist.GetCount - 1 do
  begin
    Track := Playlist.CreateTrack(i);
    TrackStr := Format('Track: %s', [Track.Name]);
    Write(TrackStr.PadRight(80, '.'));
    Write('Load');
    Track.Load;
    WriteLn(' --> Success');
    Track.Save(Format('%s.mp3', [Track.Name]));
    Track._Release;
    if i > 0 then break;
  end;
  Playlist._Release;
end;

begin
  RegisterTest(TApiTest.Suite);

end.
