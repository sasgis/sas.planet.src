unit u_VectorItemProjected;

interface

uses
  Classes,
  t_GeoTypes,
  i_EnumDoublePoint,
  i_ProjectionInfo,
  i_VectorItemProjected;

type
  TProjectedLineSet = class(TInterfacedObject)
  private
    FList: IInterfaceList;
    FProjection: IProjectionInfo;
  private
    function GetCount: Integer;
    function GetProjection: IProjectionInfo;
  public
    constructor Create(
      AProjection: IProjectionInfo;
      AList: IInterfaceList
    );
  end;

  TProjectedPath = class(TProjectedLineSet, IProjectedPath)
  private
    function GetEnum: IEnumDoublePoint;
    function GetItem(AIndex: Integer): IProjectedPathLine;
  end;

  TProjectedPolygon = class(TProjectedLineSet, IProjectedPolygon)
  private
    function GetEnum: IEnumDoublePoint;
    function GetItem(AIndex: Integer): IProjectedPolygonLine;
  end;

  TProjectedPathOneLine = class(TInterfacedObject, IProjectedPath)
  private
    FLine: IProjectedPathLine;
  private
    function GetProjection: IProjectionInfo;
    function GetCount: Integer;
    function GetEnum: IEnumDoublePoint;
    function GetItem(AIndex: Integer): IProjectedPathLine;
  public
    constructor Create(
      ALine: IProjectedPathLine
    );
  end;

  TProjectedPolygonOneLine = class(TInterfacedObject, IProjectedPolygon)
  private
    FLine: IProjectedPolygonLine;
  private
    function GetProjection: IProjectionInfo;
    function GetCount: Integer;
    function GetEnum: IEnumDoublePoint;
    function GetItem(AIndex: Integer): IProjectedPolygonLine;
  public
    constructor Create(
      ALine: IProjectedPolygonLine
    );
  end;

implementation

uses
  SysUtils,
  u_EnumDoublePointByLineSet;

{ TProjectedLineSet }

constructor TProjectedLineSet.Create(AProjection: IProjectionInfo;
  AList: IInterfaceList);
begin
  FList := AList;
  FProjection := AProjection;
end;

function TProjectedLineSet.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TProjectedLineSet.GetProjection: IProjectionInfo;
begin
  Result := FProjection;
end;

{ TProjectedPath }

function TProjectedPath.GetEnum: IEnumDoublePoint;
begin
  Result := TEnumDoublePointByLineSet.Create(Self);
end;

function TProjectedPath.GetItem(AIndex: Integer): IProjectedPathLine;
begin
  if not Supports(FList[AIndex], IProjectedPathLine, Result) then begin
    Result := nil;
  end;
end;

{ TProjectedPolygon }

function TProjectedPolygon.GetEnum: IEnumDoublePoint;
begin
  Result := TEnumDoublePointByLineSet.Create(Self);
end;

function TProjectedPolygon.GetItem(AIndex: Integer): IProjectedPolygonLine;
begin
  if not Supports(FList[AIndex], IProjectedPolygonLine, Result) then begin
    Result := nil;
  end;
end;

{ TProjectedPathOneLine }

constructor TProjectedPathOneLine.Create(ALine: IProjectedPathLine);
begin
  FLine := ALine;
end;

function TProjectedPathOneLine.GetCount: Integer;
begin
  Result := 1;
end;

function TProjectedPathOneLine.GetEnum: IEnumDoublePoint;
begin
  Result := FLine.GetEnum;
end;

function TProjectedPathOneLine.GetItem(AIndex: Integer): IProjectedPathLine;
begin
  if AIndex = 0 then begin
    Result := FLine;
  end else begin
    Result := nil;
  end;
end;

function TProjectedPathOneLine.GetProjection: IProjectionInfo;
begin
  Result := FLine.Projection;
end;

{ TProjectedPolygonOneLine }

constructor TProjectedPolygonOneLine.Create(ALine: IProjectedPolygonLine);
begin
  FLine := ALine;
end;

function TProjectedPolygonOneLine.GetCount: Integer;
begin
  Result := 1;
end;

function TProjectedPolygonOneLine.GetEnum: IEnumDoublePoint;
begin
  Result := FLine.GetEnum;
end;

function TProjectedPolygonOneLine.GetItem(
  AIndex: Integer): IProjectedPolygonLine;
begin
  if AIndex = 0 then begin
    Result := FLine;
  end else begin
    Result := nil;
  end;
end;

function TProjectedPolygonOneLine.GetProjection: IProjectionInfo;
begin
  Result := FLine.Projection;
end;

end.
