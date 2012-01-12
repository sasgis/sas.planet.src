unit u_VectorItmesFactorySimple;

interface

uses
  t_GeoTypes,
  i_ProjectionInfo,
  i_LocalCoordConverter,
  i_EnumDoublePoint,
  i_DoublePointFilter,
  i_DoublePointsAggregator,
  i_VectorItemLonLat,
  i_VectorItemProjected,
  i_VectorItemLocal,
  i_VectorItmesFactory;

type
  TVectorItmesFactorySimple = class(TInterfacedObject, IVectorItmesFactory)
  private
    FEmptyLonLatPath: ILonLatPath;
    FEmptyLonLatPolygon: ILonLatPolygon;
    FEmptyProjectedPath: IProjectedPath;
    FEmptyProjectedPolygon: IProjectedPolygon;
    FEmptyLocalPath: ILocalPath;
    FEmptyLocalPolygon: ILocalPolygon;
  private
    function CreateLonLatPath(
      APoints: PDoublePointArray;
      ACount: Integer
    ): ILonLatPath;
    function CreateLonLatPolygon(
      APoints: PDoublePointArray;
      ACount: Integer
    ): ILonLatPolygon;
    function CreateProjectedPath(
      AProjection: IProjectionInfo;
      APoints: PDoublePointArray;
      ACount: Integer
    ): IProjectedPath;
    function CreateProjectedPolygon(
      AProjection: IProjectionInfo;
      APoints: PDoublePointArray;
      ACount: Integer
    ): IProjectedPolygon;
    function CreateLocalPath(
      ALocalConverter: ILocalCoordConverter;
      APoints: PDoublePointArray;
      ACount: Integer
    ): ILocalPath;
    function CreateLocalPolygon(
      ALocalConverter: ILocalCoordConverter;
      APoints: PDoublePointArray;
      ACount: Integer
    ): ILocalPolygon;

    function CreateLonLatPolygonLineByRect(
      ARect: TDoubleRect
    ): ILonLatPolygonLine;
    function CreateLonLatPolygonByRect(
      ARect: TDoubleRect
    ): ILonLatPolygon;
    function CreateProjectedPolygonLineByRect(
      AProjection: IProjectionInfo;
      ARect: TDoubleRect
    ): IProjectedPolygonLine;
    function CreateProjectedPolygonByRect(
      AProjection: IProjectionInfo;
      ARect: TDoubleRect
    ): IProjectedPolygon;

    function CreateProjectedPathByEnum(
      AProjection: IProjectionInfo;
      AEnum: IEnumProjectedPoint;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonByEnum(
      AProjection: IProjectionInfo;
      AEnum: IEnumProjectedPoint;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;
    function CreateLocalPathByEnum(
      ALocalConverter: ILocalCoordConverter;
      AEnum: IEnumLocalPoint;
      ATemp: IDoublePointsAggregator = nil
    ): ILocalPath;
    function CreateLocalPolygonByEnum(
      ALocalConverter: ILocalCoordConverter;
      AEnum: IEnumLocalPoint;
      ATemp: IDoublePointsAggregator = nil
    ): ILocalPolygon;

    function CreateProjectedPathByLonLatEnum(
      AProjection: IProjectionInfo;
      AEnum: IEnumLonLatPoint;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonByLonLatEnum(
      AProjection: IProjectionInfo;
      AEnum: IEnumLonLatPoint;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;

    function CreateProjectedPathByLonLatPath(
      AProjection: IProjectionInfo;
      ASource: ILonLatPath;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonByLonLatPolygon(
      AProjection: IProjectionInfo;
      ASource: ILonLatPolygon;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;

    function CreateProjectedPathWithClipByLonLatEnum(
      AProjection: IProjectionInfo;
      AEnum: IEnumLonLatPoint;
      AMapPixelsClipRect: TDoubleRect;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonWithClipByLonLatEnum(
      AProjection: IProjectionInfo;
      AEnum: IEnumLonLatPoint;
      AMapPixelsClipRect: TDoubleRect;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;

    function CreateProjectedPathWithClipByLonLatPath(
      AProjection: IProjectionInfo;
      ASource: ILonLatPath;
      AMapPixelsClipRect: TDoubleRect;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonWithClipByLonLatPolygon(
      AProjection: IProjectionInfo;
      ASource: ILonLatPolygon;
      AMapPixelsClipRect: TDoubleRect;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;

    function CreateProjectedPathByLonLatPathUseConverter(
      AProjection: IProjectionInfo;
      ASource: ILonLatPath;
      AConverter: ILonLatPointConverter;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonByLonLatPolygonUseConverter(
      AProjection: IProjectionInfo;
      ASource: ILonLatPolygon;
      AConverter: ILonLatPointConverter;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;
  public
    constructor Create();
  end;

implementation

uses
  Classes,
  u_GeoFun,
  u_DoublePointsAggregator,
  u_LonLatSingleLine,
  u_ProjectedSingleLine,
  u_LocalSingleLine,
  u_EnumDoublePointLonLatToMapPixel,
  u_EnumDoublePointMapPixelToLocalPixel,
  u_EnumDoublePointWithClip,
  u_EnumDoublePointFilterFirstSegment,
  u_EnumDoublePointFilterEqual,
  u_VectorItemLonLat,
  u_VectorItemProjected,
  u_VectorItemLocal;

{ TEnumDoublePointEmpty }

type
  TEnumDoublePointEmpty = class(TInterfacedObject, IEnumDoublePoint, IEnumLonLatPoint, IEnumProjectedPoint, IEnumLocalPoint)
  private
    function Next(out APoint: TDoublePoint): Boolean;
  end;

function TEnumDoublePointEmpty.Next(out APoint: TDoublePoint): Boolean;
begin
  APoint := CEmptyDoublePoint;
  Result := False;
end;

{ TLineSetEmpty }
type
  TLineSetEmpty = class(TInterfacedObject, ILonLatPath, ILonLatPolygon, IProjectedPath, IProjectedPolygon, ILocalPath, ILocalPolygon)
  private
    FEnumLonLat: IEnumLonLatPoint;
    FEnumProjected: IEnumProjectedPoint;
    FEnumLocal: IEnumLocalPoint;
  private
    function GetItemLonLatPathLine(AIndex: Integer): ILonLatPathLine;
    function GetItemLonLatPolygonLine(AIndex: Integer): ILonLatPolygonLine;
    function GetItemProjectedPathLine(AIndex: Integer): IProjectedPathLine;
    function GetItemProjectedPolygonLine(AIndex: Integer): IProjectedPolygonLine;
    function GetItemLocalPathLine(AIndex: Integer): ILocalPathLine;
    function GetItemLocalPolygonLine(AIndex: Integer): ILocalPolygonLine;

    function GetEnumLonLat: IEnumLonLatPoint;
    function GetEnumProjected: IEnumProjectedPoint;
    function GetEnumLocal: IEnumLocalPoint;
  private
    function GetProjection: IProjectionInfo;
    function GetLocalConverter: ILocalCoordConverter;
    function GetCount: Integer;

    function ILonLatPath.GetEnum = GetEnumLonLat;
    function ILonLatPolygon.GetEnum = GetEnumLonLat;
    function IProjectedPath.GetEnum = GetEnumProjected;
    function IProjectedPolygon.GetEnum = GetEnumProjected;
    function ILocalPath.GetEnum = GetEnumLocal;
    function ILocalPolygon.GetEnum = GetEnumLocal;

    function ILonLatPath.GetItem = GetItemLonLatPathLine;
    function ILonLatPolygon.GetItem = GetItemLonLatPolygonLine;
    function IProjectedPath.GetItem = GetItemProjectedPathLine;
    function IProjectedPolygon.GetItem = GetItemProjectedPolygonLine;
    function ILocalPath.GetItem = GetItemLocalPathLine;
    function ILocalPolygon.GetItem = GetItemLocalPolygonLine;
  public
    constructor Create();
  end;

constructor TLineSetEmpty.Create;
var
  VEnum: TEnumDoublePointEmpty;
begin
  VEnum := TEnumDoublePointEmpty.Create;
  FEnumLonLat := VEnum;
  FEnumProjected := VEnum;
  FEnumLocal := VEnum;
end;

function TLineSetEmpty.GetCount: Integer;
begin
  Result := 0;
end;

function TLineSetEmpty.GetEnumLocal: IEnumLocalPoint;
begin
  Result := FEnumLocal;
end;

function TLineSetEmpty.GetEnumLonLat: IEnumLonLatPoint;
begin
  Result := FEnumLonLat;
end;

function TLineSetEmpty.GetEnumProjected: IEnumProjectedPoint;
begin
  Result := FEnumProjected;
end;

function TLineSetEmpty.GetItemLocalPathLine(AIndex: Integer): ILocalPathLine;
begin
  Result := nil;
end;

function TLineSetEmpty.GetItemLocalPolygonLine(
  AIndex: Integer): ILocalPolygonLine;
begin
  Result := nil;
end;

function TLineSetEmpty.GetItemLonLatPathLine(AIndex: Integer): ILonLatPathLine;
begin
  Result := nil;
end;

function TLineSetEmpty.GetItemLonLatPolygonLine(
  AIndex: Integer): ILonLatPolygonLine;
begin
  Result := nil;
end;

function TLineSetEmpty.GetItemProjectedPathLine(
  AIndex: Integer): IProjectedPathLine;
begin
  Result := nil;
end;

function TLineSetEmpty.GetItemProjectedPolygonLine(
  AIndex: Integer): IProjectedPolygonLine;
begin
  Result := nil;
end;

function TLineSetEmpty.GetLocalConverter: ILocalCoordConverter;
begin
  Result := nil;
end;

function TLineSetEmpty.GetProjection: IProjectionInfo;
begin
  Result := nil;
end;

{ TVectorItmesFactorySimple }

constructor TVectorItmesFactorySimple.Create;
var
VEmpty: TLineSetEmpty;
begin
  VEmpty := TLineSetEmpty.Create;
  FEmptyLonLatPath := VEmpty;
  FEmptyLonLatPolygon := VEmpty;
  FEmptyProjectedPath := VEmpty;
  FEmptyProjectedPolygon := VEmpty;
  FEmptyLocalPath := VEmpty;
  FEmptyLocalPolygon := VEmpty;
end;

function TVectorItmesFactorySimple.CreateLocalPath(
  ALocalConverter: ILocalCoordConverter; APoints: PDoublePointArray;
  ACount: Integer): ILocalPath;
var
  VLine: ILocalPathLine;
  i: Integer;
  VStart: PDoublePointArray;
  VLineLen: Integer;
  VLineCount: Integer;
  VList: IInterfaceList;
  VPoint: TDoublePoint;
begin
  VLineCount := 0;
  VStart := APoints;
  VLineLen := 0;
  for i := 0 to ACount - 1 do begin
    VPoint := APoints[i];
    if PointIsEmpty(VPoint) then begin
      if VLineLen > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceList.Create;
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := TLocalPathLine.Create(ALocalConverter, VStart, VLineLen);
        Inc(VLineCount);
        VLineLen := 0;
      end;
    end else begin
      if VLineLen = 0 then begin
        VStart := @APoints[i];
      end;
      Inc(VLineLen);
    end;
  end;
  if VLineLen > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceList.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TLocalPathLine.Create(ALocalConverter, VStart, VLineLen);
    Inc(VLineCount);
  end;
  if VLineCount = 0 then begin
    Result := FEmptyLocalPath;
  end else if VLineCount = 1 then begin
    Result := TLocalPathOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TLocalPath.Create(ALocalConverter, VList);
  end;
end;

function TVectorItmesFactorySimple.CreateLocalPathByEnum(
  ALocalConverter: ILocalCoordConverter; AEnum: IEnumLocalPoint;
  ATemp: IDoublePointsAggregator): ILocalPath;
var
  VPoint: TDoublePoint;
  VLine: ILocalPathLine;
  VList: IInterfaceList;
  VLineCount: Integer;
  VTemp: IDoublePointsAggregator;
begin
  VTemp := ATemp;
  if VTemp = nil then begin
    VTemp := TDoublePointsAggregator.Create;
  end;
  VTemp.Clear;
  VLineCount := 0;
  while AEnum.Next(VPoint) do begin
    if PointIsEmpty(VPoint) then begin
      if VTemp.Count > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceList.Create;
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := TLocalPathLine.Create(ALocalConverter, VTemp.Points, VTemp.Count);
        Inc(VLineCount);
        VTemp.Clear
      end;
    end else begin
      VTemp.Add(VPoint);
    end;
  end;
  if VTemp.Count > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceList.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TLocalPathLine.Create(ALocalConverter, VTemp.Points, VTemp.Count);
    Inc(VLineCount);
    VTemp.Clear
  end;
  if VLineCount = 0 then begin
    Result := FEmptyLocalPath;
  end else if VLineCount = 1 then begin
    Result := TLocalPathOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TLocalPath.Create(ALocalConverter, VList);
  end;
end;

function TVectorItmesFactorySimple.CreateLocalPolygon(
  ALocalConverter: ILocalCoordConverter; APoints: PDoublePointArray;
  ACount: Integer): ILocalPolygon;
var
  VLine: ILocalPolygonLine;
  i: Integer;
  VStart: PDoublePointArray;
  VLineLen: Integer;
  VLineCount: Integer;
  VList: IInterfaceList;
  VPoint: TDoublePoint;
begin
  VLineCount := 0;
  VStart := APoints;
  VLineLen := 0;
  for i := 0 to ACount - 1 do begin
    VPoint := APoints[i];
    if PointIsEmpty(VPoint) then begin
      if VLineLen > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceList.Create;
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := TLocalPolygonLine.Create(ALocalConverter, VStart, VLineLen);
        Inc(VLineCount);
        VLineLen := 0;
      end;
    end else begin
      if VLineLen = 0 then begin
        VStart := @APoints[i];
      end;
      Inc(VLineLen);
    end;
  end;
  if VLineLen > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceList.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TLocalPolygonLine.Create(ALocalConverter, VStart, VLineLen);
    Inc(VLineCount);
  end;
  if VLineCount = 0 then begin
    Result := FEmptyLocalPolygon;
  end else if VLineCount = 1 then begin
    Result := TLocalPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TLocalPolygon.Create(ALocalConverter, VList);
  end;
end;

function TVectorItmesFactorySimple.CreateLocalPolygonByEnum(
  ALocalConverter: ILocalCoordConverter; AEnum: IEnumLocalPoint;
  ATemp: IDoublePointsAggregator): ILocalPolygon;
var
  VPoint: TDoublePoint;
  VLine: ILocalPolygonLine;
  VList: IInterfaceList;
  VLineCount: Integer;
  VTemp: IDoublePointsAggregator;
begin
  VTemp := ATemp;
  if VTemp = nil then begin
    VTemp := TDoublePointsAggregator.Create;
  end;
  VTemp.Clear;
  VLineCount := 0;
  while AEnum.Next(VPoint) do begin
    if PointIsEmpty(VPoint) then begin
      if VTemp.Count > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceList.Create;
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := TLocalPolygonLine.Create(ALocalConverter, VTemp.Points, VTemp.Count);
        Inc(VLineCount);
        VTemp.Clear
      end;
    end else begin
      VTemp.Add(VPoint);
    end;
  end;
  if VTemp.Count > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceList.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TLocalPolygonLine.Create(ALocalConverter, VTemp.Points, VTemp.Count);
    Inc(VLineCount);
    VTemp.Clear
  end;
  if VLineCount = 0 then begin
    Result := FEmptyLocalPolygon;
  end else if VLineCount = 1 then begin
    Result := TLocalPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TLocalPolygon.Create(ALocalConverter, VList);
  end;
end;

function TVectorItmesFactorySimple.CreateLonLatPath(APoints: PDoublePointArray;
  ACount: Integer): ILonLatPath;
var
  VLine: ILonLatPathLine;
  i: Integer;
  VStart: PDoublePointArray;
  VLineLen: Integer;
  VLineCount: Integer;
  VList: IInterfaceList;
  VPoint: TDoublePoint;
begin
  VLineCount := 0;
  VStart := APoints;
  VLineLen := 0;
  for i := 0 to ACount - 1 do begin
    VPoint := APoints[i];
    if PointIsEmpty(VPoint) then begin
      if VLineLen > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceList.Create;
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := TLonLatPathLine.Create(VStart, VLineLen);
        Inc(VLineCount);
        VLineLen := 0;
      end;
    end else begin
      if VLineLen = 0 then begin
        VStart := @APoints[i];
      end;
      Inc(VLineLen);
    end;
  end;
  if VLineLen > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceList.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TLonLatPathLine.Create(VStart, VLineLen);
    Inc(VLineCount);
  end;
  if VLineCount = 0 then begin
    Result := FEmptyLonLatPath;
  end else if VLineCount = 1 then begin
    Result := TLonLatPathOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TLonLatPath.Create(VList);
  end;
end;

function TVectorItmesFactorySimple.CreateLonLatPolygon(
  APoints: PDoublePointArray; ACount: Integer): ILonLatPolygon;
var
  VLine: ILonLatPolygonLine;
  i: Integer;
  VStart: PDoublePointArray;
  VLineLen: Integer;
  VLineCount: Integer;
  VList: IInterfaceList;
  VPoint: TDoublePoint;
begin
  VLineCount := 0;
  VStart := APoints;
  VLineLen := 0;
  for i := 0 to ACount - 1 do begin
    VPoint := APoints[i];
    if PointIsEmpty(VPoint) then begin
      if VLineLen > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceList.Create;
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := TLonLatPolygonLine.Create(VStart, VLineLen);
        Inc(VLineCount);
        VLineLen := 0;
      end;
    end else begin
      if VLineLen = 0 then begin
        VStart := @APoints[i];
      end;
      Inc(VLineLen);
    end;
  end;
  if VLineLen > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceList.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TLonLatPolygonLine.Create(VStart, VLineLen);
    Inc(VLineCount);
  end;
  if VLineCount = 0 then begin
    Result := FEmptyLonLatPolygon;
  end else if VLineCount = 1 then begin
    Result := TLonLatPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TLonLatPolygon.Create(VList);
  end;
end;

function TVectorItmesFactorySimple.CreateLonLatPolygonByRect(
  ARect: TDoubleRect): ILonLatPolygon;
begin
  Result := TLonLatPolygonOneLine.Create(CreateLonLatPolygonLineByRect(ARect));
end;

function TVectorItmesFactorySimple.CreateLonLatPolygonLineByRect(
  ARect: TDoubleRect): ILonLatPolygonLine;
var
  VPoints: array [0..4] of TDoublePoint;
begin
  VPoints[0] := ARect.TopLeft;
  VPoints[1].X := ARect.Right;
  VPoints[1].Y := ARect.Top;
  VPoints[2] := ARect.BottomRight;
  VPoints[3].X := ARect.Left;
  VPoints[3].Y := ARect.Bottom;
  Result := TLonLatPolygonLine.Create(@VPoints[0], 4);
end;

function TVectorItmesFactorySimple.CreateProjectedPolygonByLonLatPolygonUseConverter(
  AProjection: IProjectionInfo;
  ASource: ILonLatPolygon;
  AConverter: ILonLatPointConverter;
  ATemp: IDoublePointsAggregator
): IProjectedPolygon;
var
  VPoint: TDoublePoint;
  VLine: IProjectedPolygonLine;
  VList: IInterfaceList;
  VLineCount: Integer;
  VTemp: IDoublePointsAggregator;
  i: Integer;
  VSourceLine: ILonLatPolygonLine;
  VEnumLonLat: IEnumLonLatPoint;
  VEnumProjected: IEnumProjectedPoint;
begin
  VTemp := ATemp;
  if VTemp = nil then begin
    VTemp := TDoublePointsAggregator.Create;
  end;
  VTemp.Clear;
  VLineCount := 0;
  for i := 0 to ASource.Count - 1 do begin
    VSourceLine := ASource.Item[i];
    VEnumLonLat := VSourceLine.GetEnum;
    VEnumProjected := AConverter.CreateFilteredEnum(VEnumLonLat);
    while VEnumLonLat.Next(VPoint) do begin
      if PointIsEmpty(VPoint) then begin
        Break;
      end;
      VTemp.Add(VPoint);
    end;
    if VTemp.Count > 0 then begin
      if VLineCount > 0 then begin
        if VLineCount = 1 then begin
          VList := TInterfaceList.Create;
        end;
        VList.Add(VLine);
        VLine := nil;
      end;
      VLine := TProjectedPolygonLine.Create(AProjection, VTemp.Points, VTemp.Count);
      Inc(VLineCount);
      VTemp.Clear
    end;
  end;
  if VTemp.Count > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceList.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TProjectedPolygonLine.Create(AProjection, VTemp.Points, VTemp.Count);
    Inc(VLineCount);
    VTemp.Clear
  end;
  if VLineCount = 0 then begin
    Result := FEmptyProjectedPolygon;
  end else if VLineCount = 1 then begin
    Result := TProjectedPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TProjectedPolygon.Create(AProjection, VList);
  end;
end;

function TVectorItmesFactorySimple.CreateProjectedPath(
  AProjection: IProjectionInfo; APoints: PDoublePointArray;
  ACount: Integer): IProjectedPath;
var
  VLine: IProjectedPathLine;
  i: Integer;
  VStart: PDoublePointArray;
  VLineLen: Integer;
  VLineCount: Integer;
  VList: IInterfaceList;
  VPoint: TDoublePoint;
begin
  VLineCount := 0;
  VStart := APoints;
  VLineLen := 0;
  for i := 0 to ACount - 1 do begin
    VPoint := APoints[i];
    if PointIsEmpty(VPoint) then begin
      if VLineLen > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceList.Create;
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := TProjectedPathLine.Create(AProjection, VStart, VLineLen);
        Inc(VLineCount);
        VLineLen := 0;
      end;
    end else begin
      if VLineLen = 0 then begin
        VStart := @APoints[i];
      end;
      Inc(VLineLen);
    end;
  end;
  if VLineLen > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceList.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TProjectedPathLine.Create(AProjection, VStart, VLineLen);
    Inc(VLineCount);
  end;
  if VLineCount = 0 then begin
    Result := FEmptyProjectedPath;
  end else if VLineCount = 1 then begin
    Result := TProjectedPathOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TProjectedPath.Create(AProjection, VList);
  end;
end;

function TVectorItmesFactorySimple.CreateProjectedPathByEnum(
  AProjection: IProjectionInfo;
  AEnum: IEnumProjectedPoint;
  ATemp: IDoublePointsAggregator
): IProjectedPath;
var
  VPoint: TDoublePoint;
  VLine: IProjectedPathLine;
  VList: IInterfaceList;
  VLineCount: Integer;
  VTemp: IDoublePointsAggregator;
begin
  VTemp := ATemp;
  if VTemp = nil then begin
    VTemp := TDoublePointsAggregator.Create;
  end;
  VTemp.Clear;
  VLineCount := 0;
  while AEnum.Next(VPoint) do begin
    if PointIsEmpty(VPoint) then begin
      if VTemp.Count > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceList.Create;
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := TProjectedPathLine.Create(AProjection, VTemp.Points, VTemp.Count);
        Inc(VLineCount);
        VTemp.Clear
      end;
    end else begin
      VTemp.Add(VPoint);
    end;
  end;
  if VTemp.Count > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceList.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TProjectedPathLine.Create(AProjection, VTemp.Points, VTemp.Count);
    Inc(VLineCount);
    VTemp.Clear
  end;
  if VLineCount = 0 then begin
    Result := FEmptyProjectedPath;
  end else if VLineCount = 1 then begin
    Result := TProjectedPathOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TProjectedPath.Create(AProjection, VList);
  end;
end;

function TVectorItmesFactorySimple.CreateProjectedPathByLonLatEnum(
  AProjection: IProjectionInfo;
  AEnum: IEnumLonLatPoint;
  ATemp: IDoublePointsAggregator
): IProjectedPath;
begin
  Result :=
    CreateProjectedPathByEnum(
      AProjection,
      TEnumProjectedPointFilterEqual.Create(
        TEnumDoublePointLonLatToMapPixel.Create(
          AProjection.Zoom,
          AProjection.GeoConverter,
          AEnum
        )
      ),
      ATemp
    );
end;

function TVectorItmesFactorySimple.CreateProjectedPathByLonLatPath(
  AProjection: IProjectionInfo; ASource: ILonLatPath;
  ATemp: IDoublePointsAggregator): IProjectedPath;
begin
  Result :=
    CreateProjectedPathByLonLatEnum(
      AProjection,
      ASource.GetEnum,
      ATemp
    );
end;

function TVectorItmesFactorySimple.CreateProjectedPathByLonLatPathUseConverter(
  AProjection: IProjectionInfo; ASource: ILonLatPath;
  AConverter: ILonLatPointConverter;
  ATemp: IDoublePointsAggregator): IProjectedPath;
var
  VPoint: TDoublePoint;
  VLine: IProjectedPathLine;
  VList: IInterfaceList;
  VLineCount: Integer;
  VTemp: IDoublePointsAggregator;
  i: Integer;
  VSourceLine: ILonLatPathLine;
  VEnumLonLat: IEnumLonLatPoint;
  VEnumProjected: IEnumProjectedPoint;
begin
  VTemp := ATemp;
  if VTemp = nil then begin
    VTemp := TDoublePointsAggregator.Create;
  end;
  VTemp.Clear;
  VLineCount := 0;
  for i := 0 to ASource.Count - 1 do begin
    VSourceLine := ASource.Item[i];
    VEnumLonLat := VSourceLine.GetEnum;
    VEnumProjected := AConverter.CreateFilteredEnum(VEnumLonLat);
    while VEnumLonLat.Next(VPoint) do begin
      if PointIsEmpty(VPoint) then begin
        Break;
      end;
      VTemp.Add(VPoint);
    end;
    if VTemp.Count > 0 then begin
      if VLineCount > 0 then begin
        if VLineCount = 1 then begin
          VList := TInterfaceList.Create;
        end;
        VList.Add(VLine);
        VLine := nil;
      end;
      VLine := TProjectedPathLine.Create(AProjection, VTemp.Points, VTemp.Count);
      Inc(VLineCount);
      VTemp.Clear
    end;
  end;
  if VTemp.Count > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceList.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TProjectedPathLine.Create(AProjection, VTemp.Points, VTemp.Count);
    Inc(VLineCount);
    VTemp.Clear
  end;
  if VLineCount = 0 then begin
    Result := FEmptyProjectedPath;
  end else if VLineCount = 1 then begin
    Result := TProjectedPathOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TProjectedPath.Create(AProjection, VList);
  end;
end;

function TVectorItmesFactorySimple.CreateProjectedPathWithClipByLonLatEnum(
  AProjection: IProjectionInfo;
  AEnum: IEnumLonLatPoint;
  AMapPixelsClipRect: TDoubleRect;
  ATemp: IDoublePointsAggregator
): IProjectedPath;
begin
  Result :=
    CreateProjectedPathByEnum(
      AProjection,
      TEnumProjectedPointClipByRect.Create(
        False,
        AMapPixelsClipRect,
        TEnumProjectedPointFilterEqual.Create(
          TEnumDoublePointLonLatToMapPixel.Create(
            AProjection.Zoom,
            AProjection.GeoConverter,
            AEnum
          )
        )
      ),
      ATemp
    );
end;

function TVectorItmesFactorySimple.CreateProjectedPathWithClipByLonLatPath(
  AProjection: IProjectionInfo; ASource: ILonLatPath;
  AMapPixelsClipRect: TDoubleRect;
  ATemp: IDoublePointsAggregator): IProjectedPath;
begin
  Result :=
    CreateProjectedPathWithClipByLonLatEnum(
      AProjection,
      ASource.GetEnum,
      AMapPixelsClipRect,
      ATemp
    );
end;

function TVectorItmesFactorySimple.CreateProjectedPolygon(
  AProjection: IProjectionInfo; APoints: PDoublePointArray;
  ACount: Integer): IProjectedPolygon;
var
  VLine: IProjectedPolygonLine;
  i: Integer;
  VStart: PDoublePointArray;
  VLineLen: Integer;
  VLineCount: Integer;
  VList: IInterfaceList;
  VPoint: TDoublePoint;
begin
  VLineCount := 0;
  VStart := APoints;
  VLineLen := 0;
  for i := 0 to ACount - 1 do begin
    VPoint := APoints[i];
    if PointIsEmpty(VPoint) then begin
      if VLineLen > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceList.Create;
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := TProjectedPolygonLine.Create(AProjection, VStart, VLineLen);
        Inc(VLineCount);
        VLineLen := 0;
      end;
    end else begin
      if VLineLen = 0 then begin
        VStart := @APoints[i];
      end;
      Inc(VLineLen);
    end;
  end;
  if VLineLen > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceList.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TProjectedPolygonLine.Create(AProjection, VStart, VLineLen);
    Inc(VLineCount);
  end;
  if VLineCount = 0 then begin
    Result := FEmptyProjectedPolygon;
  end else if VLineCount = 1 then begin
    Result := TProjectedPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TProjectedPolygon.Create(AProjection, VList);
  end;
end;

function TVectorItmesFactorySimple.CreateProjectedPolygonByEnum(
  AProjection: IProjectionInfo;
  AEnum: IEnumProjectedPoint;
  ATemp: IDoublePointsAggregator
): IProjectedPolygon;
var
  VPoint: TDoublePoint;
  VLine: IProjectedPolygonLine;
  VList: IInterfaceList;
  VLineCount: Integer;
  VTemp: IDoublePointsAggregator;
begin
  VTemp := ATemp;
  if VTemp = nil then begin
    VTemp := TDoublePointsAggregator.Create;
  end;
  VTemp.Clear;
  VLineCount := 0;
  while AEnum.Next(VPoint) do begin
    if PointIsEmpty(VPoint) then begin
      if VTemp.Count > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceList.Create;
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := TProjectedPolygonLine.Create(AProjection, VTemp.Points, VTemp.Count);
        Inc(VLineCount);
        VTemp.Clear
      end;
    end else begin
      VTemp.Add(VPoint);
    end;
  end;
  if VTemp.Count > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceList.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TProjectedPolygonLine.Create(AProjection, VTemp.Points, VTemp.Count);
    Inc(VLineCount);
    VTemp.Clear
  end;
  if VLineCount = 0 then begin
    Result := FEmptyProjectedPolygon;
  end else if VLineCount = 1 then begin
    Result := TProjectedPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TProjectedPolygon.Create(AProjection, VList);
  end;
end;

function TVectorItmesFactorySimple.CreateProjectedPolygonByLonLatEnum(
  AProjection: IProjectionInfo;
  AEnum: IEnumLonLatPoint;
  ATemp: IDoublePointsAggregator
): IProjectedPolygon;
begin
  Result :=
    CreateProjectedPolygonByEnum(
      AProjection,
      TEnumProjectedPointFilterEqual.Create(
        TEnumDoublePointLonLatToMapPixel.Create(
          AProjection.Zoom,
          AProjection.GeoConverter,
          TEnumLonLatPointFilterFirstSegment.Create(
            AEnum
          )
        )
      ),
      ATemp
    );
end;

function TVectorItmesFactorySimple.CreateProjectedPolygonByLonLatPolygon(
  AProjection: IProjectionInfo;
  ASource: ILonLatPolygon;
  ATemp: IDoublePointsAggregator
): IProjectedPolygon;
begin
  Result :=
    CreateProjectedPolygonByLonLatEnum(
      AProjection,
      ASource.GetEnum,
      ATemp
    );
end;

function TVectorItmesFactorySimple.CreateProjectedPolygonByRect(
  AProjection: IProjectionInfo; ARect: TDoubleRect): IProjectedPolygon;
begin
  Result := TProjectedPolygonOneLine.Create(CreateProjectedPolygonLineByRect(AProjection, ARect));
end;

function TVectorItmesFactorySimple.CreateProjectedPolygonLineByRect(
  AProjection: IProjectionInfo; ARect: TDoubleRect): IProjectedPolygonLine;
var
  VPoints: array [0..4] of TDoublePoint;
begin
  VPoints[0] := ARect.TopLeft;
  VPoints[1].X := ARect.Right;
  VPoints[1].Y := ARect.Top;
  VPoints[2] := ARect.BottomRight;
  VPoints[3].X := ARect.Left;
  VPoints[3].Y := ARect.Bottom;
  Result := TProjectedPolygonLine.Create(AProjection, @VPoints[0], 4);
end;

function TVectorItmesFactorySimple.CreateProjectedPolygonWithClipByLonLatEnum(
  AProjection: IProjectionInfo;
  AEnum: IEnumLonLatPoint;
  AMapPixelsClipRect: TDoubleRect;
  ATemp: IDoublePointsAggregator
): IProjectedPolygon;
begin
  Result :=
    CreateProjectedPolygonByEnum(
      AProjection,
      TEnumProjectedPointClipByRect.Create(
        True,
        AMapPixelsClipRect,
        TEnumProjectedPointFilterEqual.Create(
          TEnumDoublePointLonLatToMapPixel.Create(
            AProjection.Zoom,
            AProjection.GeoConverter,
            TEnumLonLatPointFilterFirstSegment.Create(
              AEnum
            )
          )
        )
      ),
      ATemp
    );
end;

function TVectorItmesFactorySimple.CreateProjectedPolygonWithClipByLonLatPolygon(
  AProjection: IProjectionInfo; ASource: ILonLatPolygon;
  AMapPixelsClipRect: TDoubleRect;
  ATemp: IDoublePointsAggregator): IProjectedPolygon;
begin
  Result :=
    CreateProjectedPolygonWithClipByLonLatEnum(
      AProjection,
      ASource.GetEnum,
      AMapPixelsClipRect,
      ATemp
    );
end;

end.
