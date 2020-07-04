unit ymFeed;

interface

uses SysUtils, ymFace, System.JSON;

type
  TymBlock = class(TymObject)
  private
    FName: String;
  public
    property Name: String read FName;
  end;

  TymBlockList = class(TymObjectList<TymBlock>)

  end;

  TymFeed = class(TymObject)
  private
    FName: String;
    FOwner: TymObjectApi;
    FBlocks: TymBlockList;
    procedure InternalLoad;
  public
    constructor Create(AOwner: TymObjectApi);
    destructor Destroy; override;
    property Name: String read FName;
    property Blocks: TymBlockList read FBlocks;
  end;

implementation

constructor TymFeed.Create(AOwner: TymObjectApi);
begin
  inherited Create;
  FOwner := AOwner;
  InternalLoad;
end;

destructor TymFeed.Destroy;
begin
  inherited Destroy;
end;

procedure TymFeed.InternalLoad;
var
  JSON: TJSONObject;
begin
  //JSON := FOwner.SendMethod(Format('https://music.yandex.net/tracks/%s/download-info', [TrackId]));
end;

end.
