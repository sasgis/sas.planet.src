unit u_ImportMpSimple;

interface

uses
  t_GeoTypes,
  i_ImportFile,
  i_ImportConfig;

type
  TImportMpSimple = class(TInterfacedObject, IImportFile)
  private
    function ParseCoordinates(AData: string): TArrayOfDoublePoint;
  protected
    function ProcessImport(AFileName: string; AConfig: IImportConfig): Boolean;
  end;

implementation

uses
  Classes,
  Math,
  SysUtils,
  StrUtils,
  i_MarksSimple,
  u_GeoFun;

const
  CPoligonHeader = '[POLYGON]';
  CDataHeader = 'Data0=';

{ TImportMpSimple }

function TImportMpSimple.ParseCoordinates(AData: string): TArrayOfDoublePoint;
var
  VCoordList: TStringList;
  VString: string;
  i: Integer;
  VPos: Integer;
  VXStr: string;
  VYStr: string;
  VPoint: TDoublePoint;
  VFormatSettings : TFormatSettings;
  VPointsCount: Integer;
begin
  VPointsCount := 0;
  VCoordList := TStringList.Create;
  try
    VFormatSettings.DecimalSeparator := '.';
    VCoordList.Delimiter := '(';
    VCoordList.DelimitedText := AData;
    for i := 0 to VCoordList.Count - 1 do begin
      VString := VCoordList[i];
      if VString <> '' then begin
        VPoint := DoublePoint(NAN, NaN);
        VPos := Pos(',', VString);
        if VPos > 0 then begin
          VYStr := LeftStr(VString, VPos - 1);
          VXStr := MidStr(VString, VPos + 1, Length(VString));
          VPos := Pos(')', VXStr);
          if VPos > 0 then begin
            VXStr := LeftStr(VXStr, VPos - 1);
          end;
          VPoint.X := StrToFloatDef(VXStr, VPoint.X, VFormatSettings);
          VPoint.Y := StrToFloatDef(VYStr, VPoint.Y, VFormatSettings);
        end;
        if not PointIsEmpty(VPoint) then begin
          SetLength(Result, VPointsCount + 1);
          Result[VPointsCount] := VPoint;
          Inc(VPointsCount);
        end;
      end;
    end;
  finally
    VCoordList.Free;
  end;
end;

function TImportMpSimple.ProcessImport(AFileName: string;
  AConfig: IImportConfig): Boolean;
var
  VFile: TStringList;
  i:integer;
  VPolygon: TArrayOfDoublePoint;
  VMark: IMarkFull;
  VString: string;
  VPoligonLine: Integer;
  VDataLine: Integer;
begin
  Result := False;
  VPolygon := nil;
  if AConfig.TemplateNewPoly <> nil then begin
    VFile:=TStringList.Create;
    try
      VFile.LoadFromFile(AFileName);
      VPoligonLine := -1;
      for i := 0 to VFile.Count - 1 do begin
        VString := VFile[i];
        if LeftStr(VString, Length(CPoligonHeader)) = CPoligonHeader then begin
          VPoligonLine := i;
          Break;
        end;
      end;
      if VPoligonLine >= 0 then begin
        VDataLine := -1;
        for i := VPoligonLine + 1 to VFile.Count - 1 do begin
          VString := VFile[i];
          if LeftStr(VString, Length(CDataHeader)) = CDataHeader then begin
            VDataLine := i;
            Break;
          end;
        end;
        if VDataLine >= 0 then begin
          VString := MidStr(VString, Length(CDataHeader) + 1, Length(VString));
          if VString <> '' then begin
            VPolygon := ParseCoordinates(VString);
          end;
        end;
      end;
    finally
      FreeAndNil(VFile);
    end;
    if Length(VPolygon) > 2 then begin
      VMark := AConfig.MarkDB.Factory.CreateNewPoly(
        VPolygon,
        ExtractFileName(AFileName),
        '',
        AConfig.TemplateNewPoly
      );
      if VMark <> nil then begin
        AConfig.MarkDB.WriteMark(VMark);
        Result := True;
      end;
    end;
  end;
end;

end.
