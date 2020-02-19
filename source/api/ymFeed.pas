unit ymFeed;

interface

uses SysUtils, ymFace;

type
  TymFeed = class(TymObject)
  private
    FOwner: TymObjectApi;
  public
    constructor Create(AOwner: TymObjectApi; UserID: Integer);
    destructor Destroy; override;
  end;

implementation

constructor TymFeed.Create(AOwner: TymObjectApi; UserID: Integer);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TymFeed.Destroy;
begin
  inherited Destroy;
end;

end.
