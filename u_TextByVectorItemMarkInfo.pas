unit u_TextByVectorItemMarkInfo;

interface

uses
  i_VectorDataItemSimple,
  i_MarksSimple,
  i_ValueToStringConverter,
  i_Datum,
  i_TextByVectorItem;

type
  TTextByVectorItemMarkInfo = class(TInterfacedObject, ITextByVectorItem)
  private
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    FDatum: IDatum;
    function GetTextForPoint(const AMark: IMarkPoint): string;
    function GetTextForPath(const AMark: IMarkLine): string;
    function GetTextForPoly(const AMark: IMarkPoly): string;
  private
    function GetText(const AItem: IVectorDataItemSimple): string;
  public
    constructor Create(
      const AValueToStringConverterConfig: IValueToStringConverterConfig;
      const ADatum: IDatum
    );
  end;


implementation

uses
  SysUtils,
  gnugettext;

{ TTextByVectorItemMarkInfo }

constructor TTextByVectorItemMarkInfo.Create(
  const AValueToStringConverterConfig: IValueToStringConverterConfig;
  const ADatum: IDatum);
begin
  Assert(AValueToStringConverterConfig <> nil);
  Assert(ADatum <> nil);
  inherited Create;
  FValueToStringConverterConfig := AValueToStringConverterConfig;
  FDatum := ADatum;
end;

function TTextByVectorItemMarkInfo.GetText(
  const AItem: IVectorDataItemSimple): string;
var
  VMarkPoint: IMarkPoint;
  VMarkLine: IMarkLine;
  VMarkPoly: IMarkPoly;
begin
  if Supports(AItem, IMarkPoint, VMarkPoint) then begin
    Result := GetTextForPoint(VMarkPoint);
  end else if Supports(AItem, IMarkLine, VMarkLine) then begin
    Result := GetTextForPath(VMarkLine);
  end else if Supports(AItem, IMarkPoly, VMarkPoly) then begin
    Result := GetTextForPoly(VMarkPoly);
  end else begin
    Result := 'Unknown mark type';
  end;
  if Result <> '' then begin
    Result :=
      '<html>'#13#10 +
        '<head>'#13#10 +
          '<title>' + AItem.GetInfoCaption + '</title>'#13#10 +
        '</head>'#13#10 +
        '<body>'#13#10 +
          Result + #13#10 +
        '</body>'#13#10 +
        '</html>';
  end;
end;

function TTextByVectorItemMarkInfo.GetTextForPath(const AMark: IMarkLine): string;
var
  VLength: Double;
  VPartsCount: Integer;
  VPointsCount: Integer;
  i: Integer;
  VConverter: IValueToStringConverter;
  VCategoryName: string;
begin
  VPartsCount := AMark.Line.Count;
  VPointsCount := 0;
  for i := 0 to VPartsCount - 1 do begin
    Inc(VPointsCount, AMark.Line.Item[i].Count);
  end;
  VLength := AMark.Line.CalcLength(FDatum);
  VConverter := FValueToStringConverterConfig.GetStatic;
  Result := '';
  VCategoryName := '';
  if AMark.Category <> nil then begin
    VCategoryName := AMark.Category.Name;
  end;
  Result := Result + Format(_('Category: %s'), [VCategoryName]) + '<br>'#13#10;
  Result := Result + Format(_('Name: %s'), [AMark.Name]) + '<br>'#13#10;
  Result := Result + Format(_('Parts count: %d'), [VPartsCount]) + '<br>'#13#10;
  Result := Result + Format(_('Points count: %d'), [VPointsCount]) + '<br>'#13#10;
  Result := Result + Format(_('Length: %s'), [VConverter.DistConvert(VLength)]) + '<br>'#13#10;
  Result := Result + Format(_('Description:<br>'#13#10'%s'), [AMark.Desc]) + '<br>'#13#10;
end;

function TTextByVectorItemMarkInfo.GetTextForPoint(const AMark: IMarkPoint): string;
var
  VConverter: IValueToStringConverter;
  VCategoryName: string;
begin
  VConverter := FValueToStringConverterConfig.GetStatic;
  Result := '';
  VCategoryName := '';
  if AMark.Category <> nil then begin
    VCategoryName := AMark.Category.Name;
  end;
  Result := Result + Format(_('Category: %s'), [VCategoryName]) + '<br>'#13#10;
  Result := Result + Format(_('Name: %s'), [AMark.Name]) + '<br>'#13#10;
  Result := Result + Format(_('Coordinates: %s'), [VConverter.LonLatConvert(AMark.Point)]) + '<br>'#13#10;
  Result := Result + Format(_('Description:<br>'#13#10'%s'), [AMark.Desc]) + '<br>'#13#10;
end;

function TTextByVectorItemMarkInfo.GetTextForPoly(const AMark: IMarkPoly): string;
var
  VLength: Double;
  VArea: Double;
  VPartsCount: Integer;
  VPointsCount: Integer;
  i: Integer;
  VConverter: IValueToStringConverter;
  VCategoryName: string;
begin
  VPartsCount := AMark.Line.Count;
  VPointsCount := 0;
  for i := 0 to VPartsCount - 1 do begin
    Inc(VPointsCount, AMark.Line.Item[i].Count);
  end;
  VLength := AMark.Line.CalcPerimeter(FDatum);
  VArea := AMark.Line.CalcArea(FDatum);
  VConverter := FValueToStringConverterConfig.GetStatic;
  Result := '';
  VCategoryName := '';
  if AMark.Category <> nil then begin
    VCategoryName := AMark.Category.Name;
  end;
  Result := Result + Format(_('Category: %s'), [VCategoryName]) + '<br>'#13#10;
  Result := Result + Format(_('Name: %s'), [AMark.Name]) + '<br>'#13#10;
  Result := Result + Format(_('Parts count: %d'), [VPartsCount]) + '<br>'#13#10;
  Result := Result + Format(_('Points count: %d'), [VPointsCount]) + '<br>'#13#10;
  Result := Result + Format(_('Perimeter: %s'), [VConverter.DistConvert(VLength)]) + '<br>'#13#10;
  Result := Result + Format(_('Area: %s'), [VConverter.AreaConvert(VArea)]) + '<br>'#13#10;
  Result := Result + Format(_('Description:<br>'#13#10'%s'), [AMark.Desc]) + '<br>'#13#10;
end;

end.
