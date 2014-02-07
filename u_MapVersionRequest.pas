unit u_MapVersionRequest;

interface

uses
  i_MapVersionInfo,
  i_MapVersionRequest,
  u_BaseInterfacedObject;

type
  TMapVersionRequest = class(TBaseInterfacedObject, IMapVersionRequest)
  private
    FBaseVersion: IMapVersionInfo;
    FShowPrevVersion: Boolean;
  private
    function GetBaseVersion: IMapVersionInfo;
    function GetShowPrevVersion: Boolean;
    function GetIsValidVersion(const AVersion: IMapVersionInfo): Boolean;
  public
    constructor Create(
      const ABaseVersion: IMapVersionInfo;
      const AShowPrevVersion: Boolean
    );
  end;
  
implementation

{ TMapVersionRequest }

constructor TMapVersionRequest.Create(
  const ABaseVersion: IMapVersionInfo;
  const AShowPrevVersion: Boolean
);
begin
  inherited Create;
  FBaseVersion := ABaseVersion;
  FShowPrevVersion := AShowPrevVersion;
end;

function TMapVersionRequest.GetBaseVersion: IMapVersionInfo;
begin
  Result := FBaseVersion;
end;

function TMapVersionRequest.GetIsValidVersion(
  const AVersion: IMapVersionInfo
): Boolean;
begin
  Result := FShowPrevVersion or FBaseVersion.IsSame(AVersion);
end;

function TMapVersionRequest.GetShowPrevVersion: Boolean;
begin
  Result := FShowPrevVersion;
end;

end.
