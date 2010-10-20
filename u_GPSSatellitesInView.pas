unit u_GPSSatellitesInView;

interface

uses
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
      AItems: IInterfaceList
    );
    destructor Destroy; override;
  end;

implementation
{ TGPSSatellitesInView }

constructor TGPSSatellitesInView.Create(
  AFixCount: Integer; AItems: IInterfaceList);
begin
  FFixCount := AFixCount;
  FItems := AItems;
  if FItems.Count < FFixCount then begin
    FFixCount := FItems.Count;
  end;
end;

destructor TGPSSatellitesInView.Destroy;
begin
  FItems := nil;
  inherited;
end;

function TGPSSatellitesInView.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TGPSSatellitesInView.GetFixCount: Integer;
begin
  Result := FFixCount;
end;

function TGPSSatellitesInView.GetItem(AIndex: Integer): IGPSSatelliteInfo;
begin
  Result := IGPSSatelliteInfo(FItems[AIndex]);
end;

end.
