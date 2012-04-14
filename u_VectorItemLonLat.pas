unit u_VectorItemLonLat;

interface

uses
  Classes,
  t_GeoTypes,
  i_EnumDoublePoint,
  i_Datum,
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
      const AList: IInterfaceList
    );
  end;

  TLonLatPath = class(TLonLatLineSet, ILonLatPath)
  private
    function GetEnum: IEnumLonLatPoint;
    function CalcLength(const ADatum: IDatum): Double;
    function GetItem(AIndex: Integer): ILonLatPathLine;
  end;

  TLonLatPolygon = class(TLonLatLineSet, ILonLatPolygon)
  private
    function GetEnum: IEnumLonLatPoint;
    function CalcPerimeter(const ADatum: IDatum): Double;
    function CalcArea(const ADatum: IDatum): Double;
    function GetItem(AIndex: Integer): ILonLatPolygonLine;
  end;

  TLonLatPathOneLine = class(TInterfacedObject, ILonLatPath)
  private
    FLine: ILonLatPathLine;
  private
    function GetCount: Integer;
    function GetEnum: IEnumLonLatPoint;
    function CalcLength(const ADatum: IDatum): Double;
    function GetBounds: TDoubleRect;
    function GetItem(AIndex: Integer): ILonLatPathLine;
  public
    constructor Create(
      const ALine: ILonLatPathLine
    );
  end;

  TLonLatPolygonOneLine = class(TInterfacedObject, ILonLatPolygon)
  private
    FLine: ILonLatPolygonLine;
  private
    function GetCount: Integer;
    function GetEnum: IEnumLonLatPoint;
    function CalcPerimeter(const ADatum: IDatum): Double;
    function CalcArea(const ADatum: IDatum): Double;
    function GetBounds: TDoubleRect;
    function GetItem(AIndex: Integer): ILonLatPolygonLine;
  public
    constructor Create(
      const ALine: ILonLatPolygonLine
    );
  end;

implementation

uses
  SysUtils,
  u_EnumDoublePointByLineSet;

{ TLonLatLineSet }

constructor TLonLatLineSet.Create(
  const ABounds: TDoubleRect;
  const AList: IInterfaceList
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

function TLonLatPath.CalcLength(const ADatum: IDatum): Double;
var
  i: Integer;
begin
  Result := 0;
  for i:= 0 to  FList.Count - 1 do begin
    Result := Result + GetItem(i).CalcLength(ADatum);
  end;
end;

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

function TLonLatPolygon.CalcArea(const ADatum: IDatum): Double;
var
  i: Integer;
begin
  Result := 0;
  for i:= 0 to  FList.Count - 1 do begin
    Result := Result + GetItem(i).CalcArea(ADatum);
  end;
end;

function TLonLatPolygon.CalcPerimeter(const ADatum: IDatum): Double;
var
  i: Integer;
begin
  Result := 0;
  for i:= 0 to  FList.Count - 1 do begin
    Result := Result + GetItem(i).CalcPerimeter(ADatum);
  end;
end;

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

function TLonLatPathOneLine.CalcLength(const ADatum: IDatum): Double;
begin
  Result := FLine.CalcLength(ADatum);
end;

constructor TLonLatPathOneLine.Create(const ALine: ILonLatPathLine);
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

function TLonLatPolygonOneLine.CalcArea(const ADatum: IDatum): Double;
begin
  Result := FLine.CalcArea(ADatum);
end;

function TLonLatPolygonOneLine.CalcPerimeter(const ADatum: IDatum): Double;
begin
  Result := FLine.CalcPerimeter(ADatum);
end;

constructor TLonLatPolygonOneLine.Create(const ALine: ILonLatPolygonLine);
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
