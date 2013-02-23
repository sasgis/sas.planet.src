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
  OleCtrls,
  SHDocVw_EWB,
  EwbCore,
  EmbeddedWB,
  i_NotifierOperation,
  i_LanguageManager,
  i_Datum,
  i_ValueToStringConverter,
  i_MarksSimple,
  u_CommonFormAndFrameParents;

type
  TfrmMarkInfo = class(TFormWitghLanguageManager)
    mmoInfo: TMemo;
    embdwbDesc: TEmbeddedWB;
    splDesc: TSplitter;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FCancelNotifier: INotifierOperationInternal;
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    FDatum: IDatum;
    procedure OnAreaCalc(const APoly: IMarkPoly; const AArea: Double);
    function GetTextForPoint(const AMark: IMarkPoint): string;
    function GetTextForPath(const AMark: IMarkLine): string;
    function GetTextForPoly(const AMark: IMarkPoly; const AArea: Double = -1): string;
  public
    procedure ShowInfoModal(const AMark: IMark);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AValueToStringConverterConfig: IValueToStringConverterConfig;
      const ADatum: IDatum
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
  TOnAreaCalc = procedure(const APoly: IMarkPoly; const AArea: Double) of object;

  TCalcAreaThread = class(TThread)
  private
    FPoly: IMarkPoly;
    FDatum: IDatum;
    FArea: Double;
    FOnFinish: TOnAreaCalc;
    FOperationID: Cardinal;
    FCancelNotifier: INotifierOperation;
    procedure OnFinishSync;
  protected
    procedure Execute; override;
  public
    constructor Create(
      const APoly: IMarkPoly;
      const ADatum: IDatum;
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Cardinal;
      const AOnFinish: TOnAreaCalc
    );
  end;

{ TCalcAreaThread }

constructor TCalcAreaThread.Create(
  const APoly: IMarkPoly;
  const ADatum: IDatum;
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Cardinal;
  const AOnFinish: TOnAreaCalc
);
begin
  FPoly := APoly;
  FDatum := ADatum;
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
  FArea := FPoly.Line.CalcArea(FDatum, FCancelNotifier, FOperationID);
  Synchronize(OnFinishSync);
end;

{ TfrmMarkInfo }

constructor TfrmMarkInfo.Create(
  const ALanguageManager: ILanguageManager;
  const AValueToStringConverterConfig: IValueToStringConverterConfig;
  const ADatum: IDatum
);
begin
  TP_GlobalIgnoreClassProperty(TEmbeddedWB, 'StatusText');
  Assert(AValueToStringConverterConfig <> nil);
  Assert(ADatum <> nil);
  inherited Create(ALanguageManager);
  FValueToStringConverterConfig := AValueToStringConverterConfig;
  FDatum := ADatum;
  FCancelNotifier := TNotifierOperation.Create(TNotifierBase.Create);
end;

procedure TfrmMarkInfo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FCancelNotifier.NextOperation;
end;

function TfrmMarkInfo.GetTextForPath(const AMark: IMarkLine): string;
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
end;

function TfrmMarkInfo.GetTextForPoint(const AMark: IMarkPoint): string;
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
end;

function TfrmMarkInfo.GetTextForPoly(const AMark: IMarkPoly; const AArea: Double = -1): string;
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
  VLength := AMark.Line.CalcPerimeter(FDatum);
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
  if AArea <> -1 then begin
    Result := Result + Format(_('Area: %s'), [VConverter.AreaConvert(AArea)]) + #13#10;
  end else begin
    Result := Result + Format(_('Area: %s'), ['calc...']) + #13#10;
  end;
end;

procedure TfrmMarkInfo.OnAreaCalc(const APoly: IMarkPoly; const AArea: Double);
begin
  mmoInfo.Lines.Text := GetTextForPoly(APoly, AArea);
end;

procedure TfrmMarkInfo.ShowInfoModal(const AMark: IMark);
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
    TCalcAreaThread.Create(
      VMarkPoly,
      FDatum,
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
  end;
  Self.ShowModal;
end;

end.
