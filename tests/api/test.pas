unit test;

interface

uses sysutils, TestFramework, ymApi, ymPlaylist, ymTrack,
  JSON, ymFace;

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
    procedure TestGetPlaylistAsync;
  end;

implementation

type
  TTrackList = class(TymObjectList<TymTrack>)
  end;

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
    break;
  end;
  Playlist._Release;
end;

procedure TApiTest.TestGetPlaylistAsync;
var
  Playlist: TymPlaylist;
  Track: TymTrack;
  TrackStr: String;
  i: Integer;
  TrackList: TTrackList;
  State: TymAsyncLoadState;
begin
  WriteLn;
  FApi.AsyncMode := true;
  TrackList := TTrackList.Create;
  Playlist := FApi.GetUserPlaylist('57477732');
  try
    WriteLn(Format('PlaylistID: %s. Track count: %d', ['57477732', Playlist.GetCount]));

    for i := 0 to Playlist.GetCount - 1 do
    begin
      Track := Playlist.CreateTrack(i);
      Track.Load();
      TrackList.AddDirect(Track);
      break;
    end;

    while TrackList.Count > 0 do
    begin
      for i := TrackList.Count - 1 downto 0 do
      begin
        Track := TrackList[i];
        State := Track.LoadState;

        if State = alsCompleted then
        begin
          Track.Save(Format('%s.mp3', [Track.Name]));
          WriteLn(Format('Сохранен трэк: %s.mp3', [Track.Name]));
        end;

        if State in [alsCompleted, alsFail, alsNone] then
        begin
          _ReleaseNil(Track);
          TrackList.Delete(i);
        end;

      end;
    end;
  finally
    Playlist._Release;
    TrackList._Release;
  end;
end;

begin
  RegisterTest(TApiTest.Suite);

end.
