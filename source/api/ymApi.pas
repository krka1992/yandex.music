unit ymApi;

interface

uses SysUtils, idHTTP, System.classes, System.JSON, oxmlcdom, sblmd5, sblBuffer, ymFace;

type

  TymApi = class;

  TymToken = record
    TokenType: String;
    AccesToken: String;
    Expires: String;
    Uid: String;
  end;

  TymCodecType = (ctAac, ctMp3);

  PymDownloadInfoItem = ^TymDownloadInfoItem;
  PymDownloadInfo = ^TymDownloadInfo;

  TymDownloadInfoItem = record
    Codec: String;
    BitrateInKbps: Word;
    Gain: Boolean;
    Preview: Boolean;
    DownloadInfoUrl: String;
    Direct: Boolean;
    procedure Fill(Value: TJSONObject);
  end;

  TymDownloadInfo = record
  private
    function Add: PymDownloadInfoItem;
  public
    Hostname: String;
    ReqId: String;
    ExecDurationMillis: String;
    Data: array of TymDownloadInfoItem;
    Count: Integer;
    procedure Init;
    procedure Clear;
    function Fill(Value: TJSONObject): Boolean;
  end;

  TymTrack = class(TymObject)
  private
    FOwner: TymApi;
    FData: TMemoryStream;
    FInfo: TymDownloadInfo;
    FName: String;
    FTrackId: String;
    function FindHigherQualityMp3: PymDownloadInfoItem;
    function GetDownloadInfoItem(index: Integer): PymDownloadInfoItem;
  public
    constructor Create(Owner: TymApi; const TrackId: String);
    destructor Destroy; override;
    procedure Load(InfoIndex: Integer = -1);
    procedure Save(const Filename: String);
    property Data: TMemoryStream read FData;
    property Info: TymDownloadInfo read FInfo;
    property Name: String read FName;
  end;

  TTrackId = record
    Id: String;
    Name: String;
  end;

  TymPlaylist = class(TymObject)
  private
    FOwner: TymApi;
    FPlaylistId: String;
    FTrackIds: array of TTrackId;
    procedure Load;
  public
    constructor Create(Owner: TymApi; const PlaylistId: String);
    destructor Destroy; override;
    function CreateTrack(index: Integer): TymTrack;
    function GetCount: Integer;
  end;

  TymApi = class(TymObject)
  private
    FHTTP: TidHTTP;
    FAccessToken: TymToken;
    FAuthorized: Boolean;
    function POST(const Uri: String; Params: TStringList): String;
    function GET(const Uri: String): String;
    function SendMethod(const Uri: String): TJSONObject;
    function InternalAuth(const Username, Password: String): Boolean;
  protected
    procedure _DownloadInfo(const TrackId: String; DownloadInfo: PymDownloadInfo);
    function _FileDownloadInfo(const Uri: String): string;
    function GetDirectUrlMp3(const Uri: String): String;
    function _GetMp3(const DirectUrl: String): TMemoryStream;
    function _GetUserPlaylist(const PlaylistId: String): TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;
    function Auth(const Username, Password: String): Boolean;
    function GetTrack(const TrackId: String): TymTrack;
    function GetUserPlaylist(const PlaylistId: String): TymPlaylist;
  end;

implementation

function _JSONString(Node: TJSONObject; const Name: String): String;
var
  ErrMsg: String;
begin
  if not Assigned(Node) then raise Exception.Create('Error Message');
  if Node.Values[Name].TryGetValue(Result) then exit;
  ErrMsg := Format('Не удалось получить строковое значение пары с ключом: "%s"', [Name]);
  raise Exception.Create(ErrMsg);
end;

function _JSONBool(Node: TJSONObject; const Name: String): Boolean;
var
  ErrMsg: String;
begin
  if not Assigned(Node) then raise Exception.Create('Error Message');
  if Node.Values[Name].TryGetValue(Result) then exit;
  ErrMsg := Format('Не удалось получить булево значение пары с ключом: "%s"', [Name]);
  raise Exception.Create(ErrMsg);
end;

function _JSONInt32(Node: TJSONObject; const Name: String): Integer;
var
  ErrMsg: String;
begin
  if not Assigned(Node) then raise Exception.Create('Error Message');
  if Node.Values[Name].TryGetValue(Result) then exit;
  ErrMsg := Format('Не удалось получить числовое значение пары с ключом: "%s"', [Name]);
  raise Exception.Create(ErrMsg);
end;

constructor TymApi.Create;
begin
  inherited Create;
  FHTTP := TidHTTP.Create();
  FHTTP.HTTPOptions := FHTTP.HTTPOptions + [hoKeepOrigProtocol];
  FHTTP.Request.Accept := 'application/json';
  FHTTP.Request.Connection := 'Keep-Alive';
  FHTTP.Request.AcceptLanguage := 'ru';
  FHTTP.Request.CacheControl := 'no-cache';
  //FHTTP.Request.AcceptEncoding := 'gzip, deflate';
  FHTTP.Request.UserAgent := 'Windows 10';
end;

destructor TymApi.Destroy;
begin
  FreeAndNil(FHTTP);
  inherited Destroy;
end;

function TymApi.GET(const Uri: String): String;
begin
  Result := FHTTP.GET(Uri);
end;

function TymApi.InternalAuth(const Username, Password: String): Boolean;
var
  param: TStringList;
  Res: String;
  JSON: TJSONObject;
begin
  if FAuthorized then exit(true);
  Result := false;
  param := TStringList.Create;
  try
    param.Add('grant_type=password');
    param.Add('client_id=23cabbbdc6cd418abb4b39c32c41195d');
    param.Add('client_secret=53bc75238f0c4d08a118e51fe9203300');
    param.Add(Format('username=%s', [Username]));
    param.Add(Format('password=%s', [Password]));

    Res := POST('https://oauth.yandex.ru/token', param);
    JSON := TJSONObject.ParseJSONValue(Res, false, true) as TJSONObject;

    FAccessToken.TokenType := JSON.Values['token_type'].Value;
    FAccessToken.AccesToken := JSON.Values['access_token'].Value;
    FAccessToken.Expires := JSON.Values['expires_in'].Value;
    FAccessToken.Uid := JSON.Values['uid'].Value;

    FHTTP.Request.CustomHeaders.AddValue('Authorization', Format('OAuth %s', [FAccessToken.AccesToken]));

    FAuthorized := true;
    Result := true;
  finally param.Free;
  end;

end;

function TymApi.POST(const Uri: String; Params: TStringList): String;
begin
  FHTTP.Request.ContentType := 'application/x-www-form-urlencoded';
  Result := FHTTP.POST(Uri, Params);
end;

function TymApi.Auth(const Username, Password: String): Boolean;
begin
  try Result := InternalAuth(Username, Password);
  except
    on E: Exception do
    begin
      Result := false;
    end;
  end;
end;

function TymApi.GetDirectUrlMp3(const Uri: String): String;
var
  FileDownloadInfoXML: String;
  xml: TXMLDocument;
  Node: TXMLNode;
  Host, Path, TS, Region, S, sign, signHashed: String;

  function GetChildNodeText(Node: TXMLNode; const Name: String): String;
  var
    ChildNode: TXMLNode;
  begin
    if not Node.FindChild(Name, ChildNode) then raise Exception.Create('Error Message');
    Result := ChildNode.Text;
  end;

begin
  FileDownloadInfoXML := GET(Uri);

  xml := TXMLDocument.Create;
  try
    xml.LoadFromXML(FileDownloadInfoXML);
    if not xml.Node.FindChild('download-info', Node) then raise Exception.Create('Error Message');;
    Host := GetChildNodeText(Node, 'host');
    Path := GetChildNodeText(Node, 'path');
    TS := GetChildNodeText(Node, 'ts');
    Region := GetChildNodeText(Node, 'region');
    S := GetChildNodeText(Node, 's');

    sign := Format('XGRlBW9FXlekgbPrRHuSiA%s%s', [Copy(Path, 2, Length(Path) - 1), S]);
    signHashed := MD5DigestToStrA(MD5String(sign));

    Result := Format('https://%s/get-%s/%s/%s%s', [Host, 'mp3', signHashed, TS, Path]);
  finally xml.Free;
  end;
end;

function TymApi.GetUserPlaylist(const PlaylistId: String): TymPlaylist;
begin
  Result := TymPlaylist.Create(self, PlaylistId);
end;

function TymApi.GetTrack(const TrackId: String): TymTrack;
begin
  Result := TymTrack.Create(self, TrackId);
end;

function TymApi.SendMethod(const Uri: String): TJSONObject;
begin
  Result := TJSONObject.ParseJSONValue(GET(Uri)) as TJSONObject;
end;

procedure TymApi._DownloadInfo(const TrackId: String; DownloadInfo: PymDownloadInfo);
var
  JSON: TJSONObject;
begin
  JSON := SendMethod(Format('https://api.music.yandex.net/tracks/%s/download-info', [TrackId]));
  try
    DownloadInfo.Init;
    DownloadInfo.Fill(JSON);
  finally JSON.Free;
  end;
end;

function TymApi._FileDownloadInfo(const Uri: String): string;
begin
  Result := GET(Uri);
end;

function TymApi._GetMp3(const DirectUrl: String): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  FHTTP.GET(DirectUrl, Result);
end;

function TymApi._GetUserPlaylist(const PlaylistId: String): TJSONObject;
var
  Uri: String;
begin
  Uri := Format('https://api.music.yandex.net/users/692529388/playlists/%s', [PlaylistId]);
  Result := SendMethod(Uri);
end;

function TymDownloadInfo.Add: PymDownloadInfoItem;
begin
  if Length(Data) <= Count then SetLength(Data, Count * 2 + 2);
  Result := @Data[Count];
  Inc(Count);
end;

procedure TymDownloadInfo.Clear;
begin
  Init;
end;

function TymDownloadInfo.Fill(Value: TJSONObject): Boolean;
var
  i: Integer;
  invocationInfo: TJSONObject;
begin
  try
    Result := false;
    invocationInfo := Value.Values['invocationInfo'] as TJSONObject;
    Hostname := _JSONString(invocationInfo, 'hostname');
    ReqId := _JSONString(invocationInfo, 'req-id');
    ExecDurationMillis := _JSONString(invocationInfo, 'exec-duration-millis');

    with Value.Values['result'] as TJSONArray do
    begin
      for i := 0 to Count - 1 do
        {}self.Add.Fill(Items[i] as TJSONObject);
    end;
    Result := true;
  finally
  end;
end;

procedure TymDownloadInfo.Init;
begin
  Count := 0;
end;

procedure TymDownloadInfoItem.Fill(Value: TJSONObject);
begin
  Codec := _JSONString(Value, 'codec');
  BitrateInKbps := _JSONInt32(Value, 'bitrateInKbps');
  Gain := _JSONBool(Value, 'gain');
  Preview := _JSONBool(Value, 'preview');
  DownloadInfoUrl := _JSONString(Value, 'downloadInfoUrl');
  Direct := _JSONBool(Value, 'direct');
end;

constructor TymTrack.Create(Owner: TymApi; const TrackId: String);
begin
  inherited Create;
  FOwner := Owner;
  FTrackId := TrackId;
  FOwner._DownloadInfo(FTrackId, @FInfo);
end;

destructor TymTrack.Destroy;
begin
  if Assigned(FData) then FData.Free;
  inherited Destroy;
end;

function TymTrack.FindHigherQualityMp3: PymDownloadInfoItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FInfo.Count - 1 do
  begin
    if FInfo.Data[i].Codec = 'mp3' then
    begin
      if not Assigned(Result) then Result := @FInfo.Data[i];
      if Result.BitrateInKbps < FInfo.Data[i].BitrateInKbps then Result := @FInfo.Data[i];
    end;
  end;
end;

function TymTrack.GetDownloadInfoItem(index: Integer): PymDownloadInfoItem;
begin
  if index >= FInfo.Count then raise Exception.Create('Error Message');
  if index < 0 then Result := FindHigherQualityMp3
  else Result := @FInfo.Data[index];
end;

{TymTrack}

procedure TymTrack.Load(InfoIndex: Integer = -1);
var
  DownloadInfo: PymDownloadInfoItem;
  DirectLink: String;
  success: Boolean;
begin
  if Assigned(FData) then exit;
  success := false;
  try
    DownloadInfo := GetDownloadInfoItem(InfoIndex);
    if not Assigned(DownloadInfo) then raise Exception.Create('Error Message');

    DirectLink := FOwner.GetDirectUrlMp3(DownloadInfo.DownloadInfoUrl);
    FData := FOwner._GetMp3(DirectLink);
    success := true;
  finally
    if not success then
      if Assigned(FData) then FData.Free;
  end;
end;

procedure TymTrack.Save(const Filename: String);
begin
  if not Assigned(FData) then exit;
  FData.SaveToFile(Filename);
end;

constructor TymPlaylist.Create(Owner: TymApi; const PlaylistId: String);
begin
  inherited Create;
  FOwner := Owner;
  FPlaylistId := PlaylistId;
  Load;
end;

destructor TymPlaylist.Destroy;
begin
  inherited Destroy;
end;

function TymPlaylist.CreateTrack(index: Integer): TymTrack;
begin
  if (Index < 0) or (Index >= GetCount) then exit(nil);
  Result := TymTrack.Create(FOwner, FTrackIds[Index].Id);
  Result.FName := FTrackIds[Index].Name;
end;

function TymPlaylist.GetCount: Integer;
begin
  Result := Length(FTrackIds);
end;

procedure TymPlaylist.Load;
var
  JSON, Track, Item: TJSONObject;
  Tracks: TJSONArray;
  i: Integer;
begin
  JSON := FOwner._GetUserPlaylist(FPlaylistId);
  try
    with JSON.GetValue('result') as TJSONObject do
      {}Tracks := GetValue('tracks') as TJSONArray;
    SetLength(FTrackIds, Tracks.Count);
    for i := 0 to Tracks.Count - 1 do
    begin
      Item := Tracks.Items[i] as TJSONObject;
      FTrackIds[i].Id := _JSONString(Item, 'id');
      Track := Item.GetValue('track') as TJSONObject;
      FTrackIds[i].Name := _JSONString(Track, 'title');
    end;
  finally JSON.Free;
  end;
end;

end.
