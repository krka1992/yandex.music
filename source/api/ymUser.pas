unit ymUser;

interface

uses SysUtils, ymFace, ymFeed;

type
  TymUser = class(TymObject)
  private
    FOwner: TymObjectApi;
    FID: Integer;
  public
    constructor Create(AOwner: TymObjectApi);
    destructor Destroy; override;
    function GetFeed: TymFeed;
    property ID: Integer Read FID;
  end;

implementation

constructor TymUser.Create(AOwner: TymObjectApi);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TymUser.Destroy;
begin
  inherited Destroy;
end;

function TymUser.GetFeed: TymFeed;
begin
  Result := TymFeed.Create(FOwner, FID);
end;

end.
