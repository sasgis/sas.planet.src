unit u_TextByVectorItemMarkInfo;

interface

uses
  i_VectorDataItemSimple,
  i_ValueToStringConverter,
  i_Datum,
  i_TextByVectorItem,
  u_BaseInterfacedObject;

type
  TTextByVectorItemMarkInfo = class(TBaseInterfacedObject, ITextByVectorItem)
  private
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    FDatum: IDatum;
    function GetTextForPoint(const AMark: IVectorDataItemPoint): string;
    function GetTextForPath(const AMark: IVectorDataItemLine): string;
    function GetTextForPoly(const AMark: IVectorDataItemPoly): string;
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
  VMarkPoint: IVectorDataItemPoint;
  VMarkLine: IVectorDataItemLine;
  VMarkPoly: IVectorDataItemPoly;
begin
  if Supports(AItem, IVectorDataItemPoint, VMarkPoint) then begin
    Result := GetTextForPoint(VMarkPoint);
  end else if Supports(AItem, IVectorDataItemLine, VMarkLine) then begin
    Result := GetTextForPath(VMarkLine);
  end else if Supports(AItem, IVectorDataItemPoly, VMarkPoly) then begin
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

function TTextByVectorItemMarkInfo.GetTextForPath(const AMark: IVectorDataItemLine): string;
var
  VLength: Double;
  VPartsCount: Integer;
  VPointsCount: Integer;
  i: Integer;
  VItemWithCategory: IVectorDataItemWithCategory;
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
  if Supports(AMark.MainInfo, IVectorDataItemWithCategory, VItemWithCategory) then begin
    if VItemWithCategory.Category <> nil then begin
      VCategoryName := VItemWithCategory.Category.Name;
    end;
  end;
  Result := Result + Format(_('Category: %s'), [VCategoryName]) + '<br>'#13#10;
  Result := Result + Format(_('Name: %s'), [AMark.Name]) + '<br>'#13#10;
  Result := Result + Format(_('Parts count: %d'), [VPartsCount]) + '<br>'#13#10;
  Result := Result + Format(_('Points count: %d'), [VPointsCount]) + '<br>'#13#10;
  Result := Result + Format(_('Length: %s'), [VConverter.DistConvert(VLength)]) + '<br>'#13#10;
  Result := Result + Format(_('Description:<br>'#13#10'%s'), [AMark.Desc]) + '<br>'#13#10;
end;

function TTextByVectorItemMarkInfo.GetTextForPoint(const AMark: IVectorDataItemPoint): string;
var
  VItemWithCategory: IVectorDataItemWithCategory;
  VConverter: IValueToStringConverter;
  VCategoryName: string;
begin
  VConverter := FValueToStringConverterConfig.GetStatic;
  Result := '';
  VCategoryName := '';
  if Supports(AMark.MainInfo, IVectorDataItemWithCategory, VItemWithCategory) then begin
    if VItemWithCategory.Category <> nil then begin
      VCategoryName := VItemWithCategory.Category.Name;
    end;
  end;
  Result := Result + Format(_('Category: %s'), [VCategoryName]) + '<br>'#13#10;
  Result := Result + Format(_('Name: %s'), [AMark.Name]) + '<br>'#13#10;
  Result := Result + Format(_('Coordinates: %s'), [VConverter.LonLatConvert(AMark.Point.Point)]) + '<br>'#13#10;
  Result := Result + Format(_('Description:<br>'#13#10'%s'), [AMark.Desc]) + '<br>'#13#10;
end;

function TTextByVectorItemMarkInfo.GetTextForPoly(const AMark: IVectorDataItemPoly): string;
var
  VLength: Double;
  VArea: Double;
  VPartsCount: Integer;
  VPointsCount: Integer;
  i: Integer;
  VItemWithCategory: IVectorDataItemWithCategory;
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
  if Supports(AMark.MainInfo, IVectorDataItemWithCategory, VItemWithCategory) then begin
    if VItemWithCategory.Category <> nil then begin
      VCategoryName := VItemWithCategory.Category.Name;
    end;
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
