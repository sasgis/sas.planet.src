unit u_ProjectionInfoFactory;

interface

uses
  SysUtils,
  i_IDList,
  i_CoordConverter,
  i_ProjectionInfo,
  i_CoordConverterFactory,
  u_BaseInterfacedObject;

type
  TProjectionInfoFactory = class(TBaseInterfacedObject, IProjectionInfoFactory)
  private
    FSync: IReadWriteSync;
    FProjectionsByConverter: IIDInterfaceList;
  private
    function GetByConverterAndZoom(
      const AGeoConverter: ICoordConverter;
      AZoom: Byte
    ): IProjectionInfo;
  public
    constructor Create(const ASync: IReadWriteSync);
  end;

implementation

uses
  u_IDInterfaceList,
  u_ProjectionInfo;

{ TProjectionInfoFactory }

constructor TProjectionInfoFactory.Create(const ASync: IReadWriteSync);
begin
  inherited Create;
  FSync := ASync;

  FProjectionsByConverter := TIDInterfaceList.Create(False);
end;

function TProjectionInfoFactory.GetByConverterAndZoom(
  const AGeoConverter: ICoordConverter;
  AZoom: Byte
): IProjectionInfo;
var
  VID: Integer;
  VZooms: IIDInterfaceList;
  i: Integer;
  VProjection: IProjectionInfo;
begin
  Result := nil;
  VID := Integer(AGeoConverter);
  FSync.BeginRead;
  try
    if not Supports(FProjectionsByConverter.GetByID(VID), IIDInterfaceList, VZooms) then begin
      VZooms := nil;
    end;
  finally
    FSync.EndRead;
  end;
  if VZooms <> nil then begin
    Result := IProjectionInfo(VZooms.GetByID(AZoom));
  end else begin
    VZooms := TIDInterfaceList.Create(False);
    for i := AGeoConverter.MinZoom to AGeoConverter.MaxZoom do begin
      VProjection := TProjectionInfo.Create(AGeoConverter, i);
      VZooms.Add(i, VProjection);
      if i =  AZoom then begin
        Result := VProjection;
      end;
    end;
    FSync.BeginWrite;
    try
      FProjectionsByConverter.Replace(VID, VZooms);
    finally
      FSync.EndWrite;
    end;
  end;
end;

end.
