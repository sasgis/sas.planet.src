unit u_VectorItemLonLat;

interface

uses
  Classes,
  t_GeoTypes,
  i_EnumDoublePoint,
  i_VectorItemLonLat;

type
  TLonLatLineSet = class(TInterfacedObject)
  private
    FList: IInterfaceList;
    FBounds: TDoubleRect;
  private
    function GetCount: Integer;
    function GetBounds: TDoubleRect;
  public
    constructor Create(
      const ABounds: TDoubleRect;
      AList: IInterfaceList
    );
  end;

  TLonLatPath = class(TLonLatLineSet, ILonLatPath)
  private
    function GetEnum: IEnumLonLatPoint;
    function GetItem(AIndex: Integer): ILonLatPathLine;
  end;

  TLonLatPolygon = class(TLonLatLineSet, ILonLatPolygon)
  private
    function GetEnum: IEnumLonLatPoint;
    function GetItem(AIndex: Integer): ILonLatPolygonLine;
  end;

  TLonLatPathOneLine = class(TInterfacedObject, ILonLatPath)
  private
    FLine: ILonLatPathLine;
  private
    function GetCount: Integer;
    function GetEnum: IEnumLonLatPoint;
    function GetBounds: TDoubleRect;
    function GetItem(AIndex: Integer): ILonLatPathLine;
  public
    constructor Create(
      ALine: ILonLatPathLine
    );
  end;

  TLonLatPolygonOneLine = class(TInterfacedObject, ILonLatPolygon)
  private
    FLine: ILonLatPolygonLine;
  private
    function GetCount: Integer;
    function GetEnum: IEnumLonLatPoint;
    function GetBounds: TDoubleRect;
    function GetItem(AIndex: Integer): ILonLatPolygonLine;
  public
    constructor Create(
      ALine: ILonLatPolygonLine
    );
  end;

implementation

uses
  SysUtils,
  u_EnumDoublePointByLineSet;

{ TLonLatLineSet }

constructor TLonLatLineSet.Create(
  const ABounds: TDoubleRect;
  AList: IInterfaceList
);
begin
  FBounds := ABounds;
  FList := AList;
end;

function TLonLatLineSet.GetBounds: TDoubleRect;
begin
  Result := FBounds;
end;

function TLonLatLineSet.GetCount: Integer;
begin
  Result := FList.Count;
end;

{ TLonLatPath }

function TLonLatPath.GetEnum: IEnumLonLatPoint;
begin
  Result := TEnumLonLatPointByPath.Create(Self);
end;

function TLonLatPath.GetItem(AIndex: Integer): ILonLatPathLine;
begin
  if not Supports(FList[AIndex], ILonLatPathLine, Result) then begin
    Result := nil;
  end;
end;

{ TLonLatPolygon }

function TLonLatPolygon.GetEnum: IEnumLonLatPoint;
begin
  Result := TEnumLonLatPointByPolygon.Create(Self);
end;

function TLonLatPolygon.GetItem(AIndex: Integer): ILonLatPolygonLine;
begin
  if not Supports(FList[AIndex], ILonLatPolygonLine, Result) then begin
    Result := nil;
  end;
end;

{ TLonLatPathOneLine }

constructor TLonLatPathOneLine.Create(ALine: ILonLatPathLine);
begin
  FLine := ALine;
end;

function TLonLatPathOneLine.GetBounds: TDoubleRect;
begin
  Result := FLine.Bounds;
end;

function TLonLatPathOneLine.GetCount: Integer;
begin
  Result := 1;
end;

function TLonLatPathOneLine.GetEnum: IEnumLonLatPoint;
begin
  Result := FLine.GetEnum;
end;

function TLonLatPathOneLine.GetItem(AIndex: Integer): ILonLatPathLine;
begin
  if AIndex = 0 then begin
    Result := FLine;
  end else begin
    Result := nil;
  end;
end;

{ TLonLatPolygonOneLine }

constructor TLonLatPolygonOneLine.Create(ALine: ILonLatPolygonLine);
begin
  FLine := ALine;
end;

function TLonLatPolygonOneLine.GetBounds: TDoubleRect;
begin
  Result := FLine.Bounds;
end;

function TLonLatPolygonOneLine.GetCount: Integer;
begin
  Result := 1;
end;

function TLonLatPolygonOneLine.GetEnum: IEnumLonLatPoint;
begin
  Result := FLine.GetEnum;
end;

function TLonLatPolygonOneLine.GetItem(AIndex: Integer): ILonLatPolygonLine;
begin
  if AIndex = 0 then begin
    Result := FLine;
  end else begin
    Result := nil;
  end;
end;

end.
