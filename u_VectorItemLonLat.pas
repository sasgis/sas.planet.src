unit u_VectorItemLonLat;

interface

uses
  i_NotifierOperation,
  i_LonLatRect,
  i_EnumDoublePoint,
  i_Datum,
  i_InterfaceListStatic,
  i_VectorItemLonLat,
  u_BaseInterfacedObject;

type
  TLonLatLineSet = class(TBaseInterfacedObject)
  private
    FList: IInterfaceListStatic;
    FBounds: ILonLatRect;
  private
    function GetCount: Integer;
    function GetBounds: ILonLatRect;
  public
    constructor Create(
      const ABounds: ILonLatRect;
      const AList: IInterfaceListStatic
    );
  end;

  TLonLatPath = class(TLonLatLineSet, ILonLatPath)
  private
    function GetEnum: IEnumLonLatPoint;
    function IsSame(const APath: ILonLatPath): Boolean;
    function CalcLength(const ADatum: IDatum): Double;
    function GetItem(AIndex: Integer): ILonLatPathLine;
  end;

  TLonLatPolygon = class(TLonLatLineSet, ILonLatPolygon)
  private
    function GetEnum: IEnumLonLatPoint;
    function IsSame(const APolygon: ILonLatPolygon): Boolean;
    function CalcPerimeter(const ADatum: IDatum): Double;
    function CalcArea(
      const ADatum: IDatum;
      const ANotifier: INotifierOperation = nil;
      const AOperationID: Integer = 0
    ): Double;
    function GetItem(AIndex: Integer): ILonLatPolygonLine;
  end;

  TLonLatPathOneLine = class(TBaseInterfacedObject, ILonLatPath)
  private
    FLine: ILonLatPathLine;
  private
    function GetCount: Integer;
    function GetEnum: IEnumLonLatPoint;
    function IsSame(const APath: ILonLatPath): Boolean;
    function CalcLength(const ADatum: IDatum): Double;
    function GetBounds: ILonLatRect;
    function GetItem(AIndex: Integer): ILonLatPathLine;
  public
    constructor Create(
      const ALine: ILonLatPathLine
    );
  end;

  TLonLatPolygonOneLine = class(TBaseInterfacedObject, ILonLatPolygon)
  private
    FLine: ILonLatPolygonLine;
  private
    function GetCount: Integer;
    function GetEnum: IEnumLonLatPoint;
    function IsSame(const APolygon: ILonLatPolygon): Boolean;
    function CalcPerimeter(const ADatum: IDatum): Double;
    function CalcArea(
      const ADatum: IDatum;
      const ANotifier: INotifierOperation = nil;
      const AOperationID: Integer = 0
    ): Double;
    function GetBounds: ILonLatRect;
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
  const ABounds: ILonLatRect;
  const AList: IInterfaceListStatic
);
begin
  Assert(AList <> nil);
  Assert(ABounds <> nil);
  inherited Create;
  FBounds := ABounds;
  FList := AList;
end;

function TLonLatLineSet.GetBounds: ILonLatRect;
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
  for i := 0 to FList.Count - 1 do begin
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

function TLonLatPath.IsSame(const APath: ILonLatPath): Boolean;
var
  i: Integer;
  VLine: ILonLatPathLine;
begin
  if APath = ILonLatPath(Self) then begin
    Result := True;
    Exit;
  end;

  if FList.Count <> APath.Count then begin
    Result := False;
    Exit;
  end;

  for i := 0 to FList.Count - 1 do begin
    VLine := GetItem(i);
    if VLine = nil then begin
      Result := False;
      Exit;
    end;
    if not VLine.IsSame(APath.Item[i]) then begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

{ TLonLatPolygon }

function TLonLatPolygon.CalcArea(
  const ADatum: IDatum;
  const ANotifier: INotifierOperation = nil;
  const AOperationID: Integer = 0
): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FList.Count - 1 do begin
    Result := Result + GetItem(i).CalcArea(ADatum, ANotifier, AOperationID);
  end;
end;

function TLonLatPolygon.CalcPerimeter(const ADatum: IDatum): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FList.Count - 1 do begin
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

function TLonLatPolygon.IsSame(const APolygon: ILonLatPolygon): Boolean;
var
  i: Integer;
  VLine: ILonLatPolygonLine;
begin
  if APolygon = ILonLatPolygon(Self) then begin
    Result := True;
    Exit;
  end;

  if FList.Count <> APolygon.Count then begin
    Result := False;
    Exit;
  end;

  for i := 0 to FList.Count - 1 do begin
    VLine := GetItem(i);
    if VLine = nil then begin
      Result := False;
      Exit;
    end;
    if not VLine.IsSame(APolygon.Item[i]) then begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

{ TLonLatPathOneLine }

function TLonLatPathOneLine.CalcLength(const ADatum: IDatum): Double;
begin
  Result := FLine.CalcLength(ADatum);
end;

constructor TLonLatPathOneLine.Create(const ALine: ILonLatPathLine);
begin
  inherited Create;
  FLine := ALine;
end;

function TLonLatPathOneLine.GetBounds: ILonLatRect;
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

function TLonLatPathOneLine.IsSame(const APath: ILonLatPath): Boolean;
begin
  if APath.Count <> 1 then begin
    Result := False;
    Exit;
  end;
  Result := FLine.IsSame(APath.Item[0]);
end;

{ TLonLatPolygonOneLine }

function TLonLatPolygonOneLine.CalcArea(
  const ADatum: IDatum;
  const ANotifier: INotifierOperation = nil;
  const AOperationID: Integer = 0
): Double;
begin
  Result := FLine.CalcArea(ADatum, ANotifier, AOperationID);
end;

function TLonLatPolygonOneLine.CalcPerimeter(const ADatum: IDatum): Double;
begin
  Result := FLine.CalcPerimeter(ADatum);
end;

constructor TLonLatPolygonOneLine.Create(const ALine: ILonLatPolygonLine);
begin
  inherited Create;
  FLine := ALine;
end;

function TLonLatPolygonOneLine.GetBounds: ILonLatRect;
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

function TLonLatPolygonOneLine.IsSame(const APolygon: ILonLatPolygon): Boolean;
begin
  if APolygon.Count <> 1 then begin
    Result := False;
    Exit;
  end;
  Result := FLine.IsSame(APolygon.Item[0]);
end;

end.
