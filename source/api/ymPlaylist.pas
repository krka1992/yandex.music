unit ymPlaylist;

interface

uses ymApi, ymFace, System.JSON, SysUtils;

type
  TymPlaylist = class(TymObject)
  private
    FOwner: TymApi;
    FPlaylistId: String;
    FTrackIds: array of TTrackId;
    procedure Load;
  protected
    function _GetUserPlaylist(const PlaylistId: String): TJSONObject;
  public
    constructor Create(Owner: TymApi; const PlaylistId: String);
    destructor Destroy; override;
    function CreateTrack(index: Integer): TymTrack;
    function GetCount: Integer;
  end;

implementation

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
  Result.Name := FTrackIds[Index].Name;
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
  JSON := _GetUserPlaylist(FPlaylistId);
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

function TymPlaylist._GetUserPlaylist(const PlaylistId: String): TJSONObject;
var
  Uri: String;
begin
  Uri := Format('https://api.music.yandex.net/users/692529388/playlists/%s', [PlaylistId]);
  Result := FOwner.SendMethod(Uri);
end;

end.
