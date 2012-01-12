unit u_VectorItemLocal;

interface

uses
  Classes,
  t_GeoTypes,
  i_EnumDoublePoint,
  i_LocalCoordConverter,
  i_VectorItemLocal;

type
  TLocalLineSet = class(TInterfacedObject)
  private
    FList: IInterfaceList;
    FLocalConverter: ILocalCoordConverter;
  private
    function GetCount: Integer;
    function GetLocalConverter: ILocalCoordConverter;
  public
    constructor Create(
      ALocalConverter: ILocalCoordConverter;
      AList: IInterfaceList
    );
  end;

  TLocalPath = class(TLocalLineSet, ILocalPath)
  private
    function GetEnum: IEnumLocalPoint;
    function GetItem(AIndex: Integer): ILocalPathLine;
  end;

  TLocalPolygon = class(TLocalLineSet, ILocalPolygon)
  private
    function GetEnum: IEnumLocalPoint;
    function GetItem(AIndex: Integer): ILocalPolygonLine;
  end;

  TLocalPathOneLine = class(TInterfacedObject, ILocalPath)
  private
    FLine: ILocalPathLine;
  private
    function GetLocalConverter: ILocalCoordConverter;
    function GetCount: Integer;
    function GetEnum: IEnumLocalPoint;
    function GetItem(AIndex: Integer): ILocalPathLine;
  public
    constructor Create(
      ALine: ILocalPathLine
    );
  end;

  TLocalPolygonOneLine = class(TInterfacedObject, ILocalPolygon)
  private
    FLine: ILocalPolygonLine;
  private
    function GetLocalConverter: ILocalCoordConverter;
    function GetCount: Integer;
    function GetEnum: IEnumLocalPoint;
    function GetItem(AIndex: Integer): ILocalPolygonLine;
  public
    constructor Create(
      ALine: ILocalPolygonLine
    );
  end;

implementation

uses
  SysUtils,
  u_EnumDoublePointByLineSet;

{ TLocalLineSet }

constructor TLocalLineSet.Create(ALocalConverter: ILocalCoordConverter;
  AList: IInterfaceList);
begin
  FList := AList;
  FLocalConverter := ALocalConverter;
end;

function TLocalLineSet.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TLocalLineSet.GetLocalConverter: ILocalCoordConverter;
begin
  Result := FLocalConverter;
end;

{ TLocalPath }

function TLocalPath.GetEnum: IEnumLocalPoint;
begin
  Result := TEnumLocalPointByPath.Create(Self);
end;

function TLocalPath.GetItem(AIndex: Integer): ILocalPathLine;
begin
  if not Supports(FList[AIndex], ILocalPathLine, Result) then begin
    Result := nil;
  end;
end;

{ TLocalPolygon }

function TLocalPolygon.GetEnum: IEnumLocalPoint;
begin
  Result := TEnumLocalPointByPolygon.Create(Self);
end;

function TLocalPolygon.GetItem(AIndex: Integer): ILocalPolygonLine;
begin
  if not Supports(FList[AIndex], ILocalPolygonLine, Result) then begin
    Result := nil;
  end;
end;

{ TLocalPathOneLine }

constructor TLocalPathOneLine.Create(ALine: ILocalPathLine);
begin
  FLine := ALine;
end;

function TLocalPathOneLine.GetCount: Integer;
begin
  Result := 1;
end;

function TLocalPathOneLine.GetEnum: IEnumLocalPoint;
begin
  Result := FLine.GetEnum;
end;

function TLocalPathOneLine.GetItem(AIndex: Integer): ILocalPathLine;
begin
  if AIndex = 0 then begin
    Result := FLine;
  end else begin
    Result := nil;
  end;
end;

function TLocalPathOneLine.GetLocalConverter: ILocalCoordConverter;
begin
  Result := FLine.LocalConverter;
end;

{ TLocalPolygonOneLine }

constructor TLocalPolygonOneLine.Create(ALine: ILocalPolygonLine);
begin
  FLine := ALine;
end;

function TLocalPolygonOneLine.GetCount: Integer;
begin
  Result := 1;
end;

function TLocalPolygonOneLine.GetEnum: IEnumLocalPoint;
begin
  Result := FLine.GetEnum;
end;

function TLocalPolygonOneLine.GetItem(
  AIndex: Integer): ILocalPolygonLine;
begin
  if AIndex = 0 then begin
    Result := FLine;
  end else begin
    Result := nil;
  end;
end;

function TLocalPolygonOneLine.GetLocalConverter: ILocalCoordConverter;
begin
  Result := FLine.LocalConverter;
end;

end.
