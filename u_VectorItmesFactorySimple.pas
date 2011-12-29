unit u_VectorItmesFactorySimple;

interface

uses
  t_GeoTypes,
  i_VectorItemLonLat,
  i_ProjectionInfo,
  i_VectorItemProjected,
  i_VectorItmesFactory;

type
  TVectorItmesFactorySimple = class(TInterfacedObject, IVectorItmesFactory)
  private
    FEmptyLonLatPath: ILonLatPath;
    FEmptyLonLatPolygon: ILonLatPolygon;
    FEmptyProjectedPath: IProjectedPath;
    FEmptyProjectedPolygon: IProjectedPolygon;
  private
    function CreateLonLatPath(APoints: PDoublePointArray; ACount: Integer): ILonLatPath;
    function CreateLonLatPolygon(APoints: PDoublePointArray; ACount: Integer): ILonLatPolygon;
    function CreateProjectedPath(AProjection: IProjectionInfo; APoints: PDoublePointArray; ACount: Integer): IProjectedPath;
    function CreateProjectedPolygon(AProjection: IProjectionInfo; APoints: PDoublePointArray; ACount: Integer): IProjectedPolygon;
    function CreateLonLatPolygonLineByRect(ARect: TDoubleRect): ILonLatPolygonLine;
    function CreateProjectedPolygonLineByRect(AProjection: IProjectionInfo; ARect: TDoubleRect): IProjectedPolygonLine;
  public
    constructor Create();
  end;

implementation

uses
  Classes,
  i_EnumDoublePoint,
  u_GeoFun,
  u_LonLatSingleLine,
  u_ProjectedSingleLine,
  u_VectorItemLonLat,
  u_VectorItemProjected;

//{ TLineSetEmpty }
//type
//  TLineSetEmpty = class(TInterfacedObject, ILonLatPath, ILonLatPolygon, IProjectedPath, IProjectedPolygon)
//  private
//    function GetProjection: IProjectionInfo;
//    function GetCount: Integer;
//    function GetEnum: IEnumDoublePoint;
//    function GetItemLonLatPathLine(AIndex: Integer): ILonLatPathLine;
//    function GetItemLonLatPolygonLine(AIndex: Integer): ILonLatPolygonLine;
//    function GetItemProjectedPathLine(AIndex: Integer): IProjectedPathLine;
//    function GetItemProjectedPolygonLine(AIndex: Integer): IProjectedPolygonLine;
//    procedure ILonLatPath.GetItem = GetItemLonLatPathLine;
//    procedure ILonLatPolygon.GetItem = GetItemLonLatPolygonLine;
//    procedure IProjectedPath.GetItem = GetItemProjectedPathLine;
//    procedure IProjectedPolygon.GetItem = GetItemProjectedPolygonLine;
//  end;
//
//function TLineSetEmpty.GetCount: Integer;
//begin
//  Result := 0;
//end;
//
//function TLineSetEmpty.GetEnum: IEnumDoublePoint;
//begin
//  Result := nil;
//end;
//
//function TLineSetEmpty.GetProjection: IProjectionInfo;
//begin
//  Result := nil;
//end;

{ TVectorItmesFactorySimple }

constructor TVectorItmesFactorySimple.Create;
//var
//VEmpty: TLineSetEmpty;
begin
//  VEmpty := TLineSetEmpty.Create;
//  FEmptyLonLatPath := VEmpty;
//  FEmptyLonLatPolygon := VEmpty;
//  FEmptyProjectedPath := VEmpty;
//  FEmptyProjectedPolygon := VEmpty;
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
    Result := TLonLatPath.Create(VList);
  end;
end;

function TVectorItmesFactorySimple.CreateLonLatPolygon(
  APoints: PDoublePointArray; ACount: Integer): ILonLatPolygon;
begin

end;

function TVectorItmesFactorySimple.CreateLonLatPolygonLineByRect(
  ARect: TDoubleRect): ILonLatPolygonLine;
begin

end;

function TVectorItmesFactorySimple.CreateProjectedPath(
  AProjection: IProjectionInfo; APoints: PDoublePointArray;
  ACount: Integer): IProjectedPath;
begin

end;

function TVectorItmesFactorySimple.CreateProjectedPolygon(
  AProjection: IProjectionInfo; APoints: PDoublePointArray;
  ACount: Integer): IProjectedPolygon;
begin

end;

function TVectorItmesFactorySimple.CreateProjectedPolygonLineByRect(
  AProjection: IProjectionInfo; ARect: TDoubleRect): IProjectedPolygonLine;
begin

end;

end.
