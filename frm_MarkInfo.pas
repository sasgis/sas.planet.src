unit frm_MarkInfo;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  i_LanguageManager,
  i_Datum,
  i_ValueToStringConverter,
  i_MarksSimple,
  u_CommonFormAndFrameParents;

type
  TfrmMarkInfo = class(TFormWitghLanguageManager)
    mmoInfo: TMemo;
    pnlBottom: TPanel;
    btnClose: TButton;
  private
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    FDatum: IDatum;
    function GetTextForPoint(AMark: IMarkPoint): string;
    function GetTextForPath(AMark: IMarkLine): string;
    function GetTextForPoly(AMark: IMarkPoly): string;
  public
    procedure ShowInfoModal(AMark: IMark);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AValueToStringConverterConfig: IValueToStringConverterConfig;
      const ADatum: IDatum
    ); reintroduce;
  end;

implementation

uses
  gnugettext;

{$R *.dfm}

{ TfrmMarkInfo }

constructor TfrmMarkInfo.Create(
  const ALanguageManager: ILanguageManager;
  const AValueToStringConverterConfig: IValueToStringConverterConfig;
  const ADatum: IDatum
);
begin
  Assert(AValueToStringConverterConfig <> nil);
  Assert(ADatum <> nil);
  inherited Create(ALanguageManager);
  FValueToStringConverterConfig := AValueToStringConverterConfig;
  FDatum := ADatum;
end;

function TfrmMarkInfo.GetTextForPath(AMark: IMarkLine): string;
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
  Result := Result + Format(_('Category: %s'), [VCategoryName]) + #13#10;
  Result := Result + Format(_('Name: %s'), [AMark.Name]) + #13#10;
  Result := Result + Format(_('Parts count: %d'), [VPartsCount]) + #13#10;
  Result := Result + Format(_('Points count: %d'), [VPointsCount]) + #13#10;
  Result := Result + Format(_('Length: %s'), [VConverter.DistConvert(VLength)]) + #13#10;
  Result := Result + Format(_('Description:'#13#10'%s'), [AMark.Desc]) + #13#10;
end;

function TfrmMarkInfo.GetTextForPoint(AMark: IMarkPoint): string;
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
  Result := Result + Format(_('Category: %s'), [VCategoryName]) + #13#10;
  Result := Result + Format(_('Name: %s'), [AMark.Name]) + #13#10;
  Result := Result + Format(_('Coordinates: %s'), [VConverter.LonLatConvert(AMark.Point)]) + #13#10;
  Result := Result + Format(_('Description:'#13#10'%s'), [AMark.Desc]) + #13#10;
end;

function TfrmMarkInfo.GetTextForPoly(AMark: IMarkPoly): string;
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
  Result := Result + Format(_('Category: %s'), [VCategoryName]) + #13#10;
  Result := Result + Format(_('Name: %s'), [AMark.Name]) + #13#10;
  Result := Result + Format(_('Parts count: %d'), [VPartsCount]) + #13#10;
  Result := Result + Format(_('Points count: %d'), [VPointsCount]) + #13#10;
  Result := Result + Format(_('Perimeter: %s'), [VConverter.DistConvert(VLength)]) + #13#10;
  Result := Result + Format(_('Area: %s'), [VConverter.AreaConvert(VArea)]) + #13#10;
  Result := Result + Format(_('Description:'#13#10'%s'), [AMark.Desc]) + #13#10;
end;

procedure TfrmMarkInfo.ShowInfoModal(AMark: IMark);
var
  VMarkPoint: IMarkPoint;
  VMarkLine: IMarkLine;
  VMarkPoly: IMarkPoly;
  VText: string;
begin
  if Supports(AMark, IMarkPoint, VMarkPoint) then begin
    VText := GetTextForPoint(VMarkPoint);
  end else if Supports(AMark, IMarkLine, VMarkLine) then begin
    VText := GetTextForPath(VMarkLine);
  end else if Supports(AMark, IMarkPoly, VMarkPoly) then begin
    VText := GetTextForPoly(VMarkPoly);
  end else begin
    VText := 'Unknown mark type';
  end;
  mmoInfo.Lines.Text := VText;
  Self.ShowModal;
end;

end.
