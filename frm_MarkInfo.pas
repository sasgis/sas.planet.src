{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit frm_MarkInfo;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  OleCtrls,
  SHDocVw_EWB,
  EwbCore,
  EmbeddedWB,
  i_NotifierOperation,
  i_LanguageManager,
  i_GeoCalc,
  i_ValueToStringConverter,
  i_VectorDataItemSimple,
  u_CommonFormAndFrameParents;

type
  TfrmMarkInfo = class(TFormWitghLanguageManager)
    mmoInfo: TMemo;
    embdwbDesc: TEmbeddedWB;
    splDesc: TSplitter;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FCancelNotifier: INotifierOperationInternal;
    FValueToStringConverter: IValueToStringConverterChangeable;
    FGeoCalc: IGeoCalc;
    procedure OnAreaCalc(const APoly: IVectorDataItemPoly; const AArea: Double);
    function GetTextForPoint(const AMark: IVectorDataItemPoint): string;
    function GetTextForPath(const AMark: IVectorDataItemLine): string;
    function GetTextForPoly(const AMark: IVectorDataItemPoly; const AArea: Double = -1): string;
  public
    procedure ShowInfoModal(const AMark: IVectorDataItemSimple);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AValueToStringConverter: IValueToStringConverterChangeable;
      const AGeoCalc: IGeoCalc
    ); reintroduce;
  end;

implementation

uses
  gnugettext,
  c_InternalBrowser,
  u_Notifier,
  u_NotifierOperation,
  u_ReadableThreadNames;

{$R *.dfm}

type
  TOnAreaCalc = procedure(const APoly: IVectorDataItemPoly; const AArea: Double) of object;

  TCalcAreaThread = class(TThread)
  private
    FPoly: IVectorDataItemPoly;
    FGeoCalc: IGeoCalc;
    FArea: Double;
    FOnFinish: TOnAreaCalc;
    FOperationID: Cardinal;
    FCancelNotifier: INotifierOperation;
    procedure OnFinishSync;
  protected
    procedure Execute; override;
  public
    constructor Create(
      const APoly: IVectorDataItemPoly;
      const AGeoCalc: IGeoCalc;
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Cardinal;
      const AOnFinish: TOnAreaCalc
    );
  end;

{ TCalcAreaThread }

constructor TCalcAreaThread.Create(
  const APoly: IVectorDataItemPoly;
  const AGeoCalc: IGeoCalc;
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Cardinal;
  const AOnFinish: TOnAreaCalc
);
begin
  FPoly := APoly;
  FGeoCalc := AGeoCalc;
  FArea := 0;
  FOperationID := AOperationID;
  FCancelNotifier := ACancelNotifier;
  FOnFinish := AOnFinish;
  FreeOnTerminate := True;
  inherited Create(False);
end;

procedure TCalcAreaThread.OnFinishSync;
begin
  if not FCancelNotifier.IsOperationCanceled(FOperationID) then begin
    FOnFinish(FPoly, FArea);
  end;
end;

procedure TCalcAreaThread.Execute;
begin
  SetCurrentThreadName(Self.ClassName);
  FArea := FGeoCalc.CalcMultiPolygonArea(FPoly.Line, FCancelNotifier, FOperationID);
  if FCancelNotifier.IsOperationCanceled(FOperationID) then begin
    Terminate;
  end else begin
    Synchronize(OnFinishSync);
  end;
end;

{ TfrmMarkInfo }

constructor TfrmMarkInfo.Create(
  const ALanguageManager: ILanguageManager;
  const AValueToStringConverter: IValueToStringConverterChangeable;
  const AGeoCalc: IGeoCalc
);
begin
  TP_GlobalIgnoreClassProperty(TEmbeddedWB, 'StatusText');
  Assert(AValueToStringConverter <> nil);
  Assert(Assigned(AGeoCalc));
  inherited Create(ALanguageManager);
  FValueToStringConverter := AValueToStringConverter;
  FGeoCalc := AGeoCalc;
  FCancelNotifier := TNotifierOperation.Create(TNotifierBase.Create);
end;

procedure TfrmMarkInfo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FCancelNotifier.NextOperation;
end;

function TfrmMarkInfo.GetTextForPath(const AMark: IVectorDataItemLine): string;
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
  VLength := FGeoCalc.CalcMultiLineLength(AMark.Line);
  VConverter := FValueToStringConverter.GetStatic;
  Result := '';
  VCategoryName := '';
  if Supports(AMark.MainInfo, IVectorDataItemWithCategory, VItemWithCategory) then begin
    if VItemWithCategory.Category <> nil then begin
      VCategoryName := VItemWithCategory.Category.Name;
    end;
  end;
  Result := Result + Format(_('Category: %s'), [VCategoryName]) + #13#10;
  Result := Result + Format(_('Name: %s'), [AMark.Name]) + #13#10;
  Result := Result + Format(_('Parts count: %d'), [VPartsCount]) + #13#10;
  Result := Result + Format(_('Points count: %d'), [VPointsCount]) + #13#10;
  Result := Result + Format(_('Length: %s'), [VConverter.DistConvert(VLength)]) + #13#10;
end;

function TfrmMarkInfo.GetTextForPoint(const AMark: IVectorDataItemPoint): string;
var
  VConverter: IValueToStringConverter;
  VItemWithCategory: IVectorDataItemWithCategory;
  VCategoryName: string;
begin
  VConverter := FValueToStringConverter.GetStatic;
  Result := '';
  VCategoryName := '';
  if Supports(AMark.MainInfo, IVectorDataItemWithCategory, VItemWithCategory) then begin
    if VItemWithCategory.Category <> nil then begin
      VCategoryName := VItemWithCategory.Category.Name;
    end;
  end;
  Result := Result + Format(_('Category: %s'), [VCategoryName]) + #13#10;
  Result := Result + Format(_('Name: %s'), [AMark.Name]) + #13#10;
  Result := Result + Format(_('Coordinates: %s'), [VConverter.LonLatConvert(AMark.Point.Point)]) + #13#10;
end;

function TfrmMarkInfo.GetTextForPoly(const AMark: IVectorDataItemPoly; const AArea: Double = -1): string;
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
  VLength := FGeoCalc.CalcMultiPolygonPerimeter(AMark.Line);
  VConverter := FValueToStringConverter.GetStatic;
  Result := '';
  VCategoryName := '';
  if Supports(AMark.MainInfo, IVectorDataItemWithCategory, VItemWithCategory) then begin
    if VItemWithCategory.Category <> nil then begin
      VCategoryName := VItemWithCategory.Category.Name;
    end;
  end;
  Result := Result + Format(_('Category: %s'), [VCategoryName]) + #13#10;
  Result := Result + Format(_('Name: %s'), [AMark.Name]) + #13#10;
  Result := Result + Format(_('Parts count: %d'), [VPartsCount]) + #13#10;
  Result := Result + Format(_('Points count: %d'), [VPointsCount]) + #13#10;
  Result := Result + Format(_('Perimeter: %s'), [VConverter.DistConvert(VLength)]) + #13#10;
  if AArea <> -1 then begin
    Result := Result + Format(_('Area: %s'), [VConverter.AreaConvert(AArea)]) + #13#10;
  end else begin
    Result := Result + Format(_('Area: %s'), [_('calc...')]) + #13#10;
  end;
end;

procedure TfrmMarkInfo.OnAreaCalc(const APoly: IVectorDataItemPoly; const AArea: Double);
begin
  mmoInfo.Lines.Text := GetTextForPoly(APoly, AArea);
end;

procedure TfrmMarkInfo.ShowInfoModal(const AMark: IVectorDataItemSimple);
var
  VMarkPoint: IVectorDataItemPoint;
  VMarkLine: IVectorDataItemLine;
  VMarkPoly: IVectorDataItemPoly;
  VText: string;
begin
  if Supports(AMark, IVectorDataItemPoint, VMarkPoint) then begin
    VText := GetTextForPoint(VMarkPoint);
  end else if Supports(AMark, IVectorDataItemLine, VMarkLine) then begin
    VText := GetTextForPath(VMarkLine);
  end else if Supports(AMark, IVectorDataItemPoly, VMarkPoly) then begin
    VText := GetTextForPoly(VMarkPoly);
    TCalcAreaThread.Create(
      VMarkPoly,
      FGeoCalc,
      FCancelNotifier,
      FCancelNotifier.CurrentOperation,
      Self.OnAreaCalc
    );
  end else begin
    VText := 'Unknown mark type';
  end;
  mmoInfo.Lines.Text := VText;
  if (AMark.GetInfoUrl <> '') and (AMark.Desc <> '') then begin
    embdwbDesc.NavigateWait(AMark.GetInfoUrl + CVectorItemInfoSuffix);
  end else begin
    embdwbDesc.AssignEmptyDocument;
  end;
  Self.ShowModal;
end;

end.
