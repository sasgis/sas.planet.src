unit u_GPSSatellitesInView;

interface

uses
  ActiveX,
  Classes,
  SysUtils,
  i_GPS;

type
  TGPSSatellitesInView = class(TInterfacedObject, IGPSSatellitesInView)
  private
    FFixCount: Integer;
    FItems: IInterfaceList;
  protected
    function GetCount: Integer; stdcall;
    function GetFixCount: Integer; stdcall;
    function GetItem(AIndex: Integer): IGPSSatelliteInfo; stdcall;
  public
    constructor Create(
      AFixCount: Integer;
      AItemsCount: Integer;
      AItems: PUnknownList
    );
    destructor Destroy; override;
  end;

implementation

const
  CMaxSatellitesCount = 32;

{ TGPSSatellitesInView }

constructor TGPSSatellitesInView.Create(
  AFixCount: Integer;
  AItemsCount: Integer;
  AItems: PUnknownList
);
var
  i: Integer;
  VItemCount: Integer;
  VItem: IGPSSatelliteInfo;
begin
  if (AItemsCount > 0) and (AItems <> nil) then begin
    VItemCount := AItemsCount;
    if VItemCount > CMaxSatellitesCount then begin
      VItemCount := CMaxSatellitesCount;
    end;

    FItems := TInterfaceList.Create;
    FItems.Capacity := VItemCount;

    for i := 0 to VItemCount - 1 do begin
      VItem := IGPSSatelliteInfo(AItems^[i]);
      FItems.Add(VItem);
    end;
    FFixCount := AFixCount;
    if FItems.Count < FFixCount then begin
      FFixCount := FItems.Count;
    end;
  end else begin
    FItems := nil;
    FFixCount := 0;
  end;
end;

destructor TGPSSatellitesInView.Destroy;
begin
  FItems := nil;
  inherited;
end;

function TGPSSatellitesInView.GetCount: Integer;
begin
  if FItems <> nil then begin
    Result := FItems.Count;
  end else begin
    Result := 0;
  end;
end;

function TGPSSatellitesInView.GetFixCount: Integer;
begin
  Result := FFixCount;
end;

function TGPSSatellitesInView.GetItem(AIndex: Integer): IGPSSatelliteInfo;
begin
  if FItems <> nil then begin
    Result := IGPSSatelliteInfo(FItems[AIndex]);
  end else begin
    Result := nil;
  end;
end;

end.
