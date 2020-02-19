unit ymTrack;

interface

uses System.Classes, SysUtils, System.JSON, ymFace, oxmlcdom;

type

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
    FOwner: TymObjectApi;
    FData: TMemoryStream;
    FInfo: TymDownloadInfo;
    FName: String;
    FTrackId: String;
    function FindHigherQualityMp3: PymDownloadInfoItem;
    function GetDownloadInfoItem(index: Integer): PymDownloadInfoItem;
  protected
    procedure _DownloadInfo(const TrackId: String; DownloadInfo: PymDownloadInfo);
    function _GetMp3(const DirectUrl: String): TMemoryStream;
  public
    constructor Create(Owner: TymObjectApi; const TrackId: String);
    destructor Destroy; override;
    function GetDirectUrlMp3(const Uri: String): String;
    procedure Load(InfoIndex: Integer = -1);
    procedure Save(const Filename: String);
    property Data: TMemoryStream read FData;
    property Info: TymDownloadInfo read FInfo;
    property Name: String read FName write FName;
  end;

implementation

constructor TymTrack.Create(Owner: TymObjectApi; const TrackId: String);
begin
  inherited Create;
  FOwner := Owner;
  FTrackId := TrackId;
  _DownloadInfo(FTrackId, @FInfo);
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

function TymTrack.GetDirectUrlMp3(const Uri: String): String;
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
  {TODO: Убрать отсюда OXML. Мне нужно несколько параметров из тела. Можно самому распарсить}
  FileDownloadInfoXML := FOwner.GET(Uri);

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
    //signHashed := MD5DigestToStrA(MD5String(sign));
    signHashed := '202cb962ac59075b964b07152d234b70';
    {TODO: Надо разобраться для чего эта строка ^}

    Result := Format('https://%s/get-%s/%s/%s%s', [Host, 'mp3', signHashed, TS, Path]);
  finally xml.Free;
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

    DirectLink := GetDirectUrlMp3(DownloadInfo.DownloadInfoUrl);
    FData := _GetMp3(DirectLink);
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

procedure TymTrack._DownloadInfo(const TrackId: String; DownloadInfo: PymDownloadInfo);
var
  JSON: TJSONObject;
begin
  JSON := FOwner.SendMethod(Format('https://api.music.yandex.net/tracks/%s/download-info', [TrackId]));
  try
    DownloadInfo.Init;
    DownloadInfo.Fill(JSON);
  finally JSON.Free;
  end;
end;

function TymTrack._GetMp3(const DirectUrl: String): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  with FOwner.CreateConnection do
  begin
    try HTTP.GET(DirectUrl, Result);
    finally _Release;
    end;
  end;
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

end.
