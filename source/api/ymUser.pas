unit ymUser;

interface

uses SysUtils, ymFace, ymFeed, ymApiCommon;

type
  TymUser = class(TymObject)
  private
    FOwner: TymObjectApi;
    FID: Integer;
  public
    constructor Create(AOwner: TymObjectApi);
    destructor Destroy; override;
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

end.
