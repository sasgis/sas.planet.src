unit u_MapAbilitiesConfigStatic;

interface

uses
  i_MapAbilitiesConfig;

type
  TMapAbilitiesConfigStatic = class(TInterfacedObject, IMapAbilitiesConfigStatic)
  private
    FIsLayer: Boolean;
    FIsShowOnSmMap: Boolean;
    FIsUseStick: Boolean;
    FIsUseGenPrevious: Boolean;
    FUseDownload: Boolean;
  protected
    function GetIsLayer: Boolean;
    function GetIsShowOnSmMap: Boolean;
    function GetIsUseStick: Boolean;
    function GetIsUseGenPrevious: Boolean;
    function GetUseDownload: Boolean;
  public
    constructor Create(
      AIsLayer: Boolean;
      AIsShowOnSmMap: Boolean;
      AIsUseStick: Boolean;
      AIsUseGenPrevious: Boolean;
      AUseDownload: Boolean
    );
  end;

implementation

{ TMapAbilitiesConfigStatic }

constructor TMapAbilitiesConfigStatic.Create(
  AIsLayer,
  AIsShowOnSmMap,
  AIsUseStick,
  AIsUseGenPrevious,
  AUseDownload: boolean
);
begin
  FIsLayer := AIsLayer;
  FIsShowOnSmMap := AIsShowOnSmMap;
  FIsUseStick := AIsUseStick;
  FIsUseGenPrevious := AIsUseGenPrevious;
  FUseDownload := AUseDownload;
end;

function TMapAbilitiesConfigStatic.GetIsLayer: Boolean;
begin
  Result := FIsLayer;
end;

function TMapAbilitiesConfigStatic.GetIsShowOnSmMap: Boolean;
begin
  Result := FIsShowOnSmMap;
end;

function TMapAbilitiesConfigStatic.GetIsUseGenPrevious: Boolean;
begin
  Result := FIsUseGenPrevious;
end;

function TMapAbilitiesConfigStatic.GetIsUseStick: Boolean;
begin
  Result := FIsUseStick;
end;

function TMapAbilitiesConfigStatic.GetUseDownload: Boolean;
begin
  Result := FUseDownload;
end;

end.
