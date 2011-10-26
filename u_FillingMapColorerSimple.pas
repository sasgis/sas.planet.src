unit u_FillingMapColorerSimple;

interface

uses
  GR32,
  t_FillingMapModes,
  i_TileInfoBasic,
  i_FillingMapColorer;

type
  TFillingMapColorerSimple = class(TInterfacedObject, IFillingMapColorer)
  private
    FNoTileColor: TColor32;
    FShowTNE: Boolean;
    FTNEColor: TColor32;
    FFillMode: TFillMode;
    FFilterMode: Boolean;
    FFillFirstDay: TDateTime;
    FFillLastDay: TDateTime;
    FGradientDays: integer;
  protected
    function GetColor(ATileInfo: ITileInfoBasic): TColor32;
  public
    constructor Create(
      ANoTileColor: TColor32;
      AShowTNE: Boolean;
      ATNEColor: TColor32;
      AFillMode: TFillMode;
      AFilterMode: Boolean;
      AFillFirstDay: TDateTime;
      AFillLastDay: TDateTime
    );
  end;

implementation

uses
  Types,
  SysUtils,
  DateUtils;

{ TFillingMapColorerSimple }

constructor TFillingMapColorerSimple.Create(
  ANoTileColor: TColor32;
  AShowTNE: Boolean;
  ATNEColor: TColor32;
  AFillMode: TFillMode;
  AFilterMode: Boolean;
  AFillFirstDay, AFillLastDay: TDateTime
);
begin
  FNoTileColor := ANoTileColor;
  FShowTNE := AShowTNE;
  FTNEColor := ATNEColor;
  FFillMode := AFillMode;
  FFilterMode := AFilterMode;
  if FFilterMode then begin
    FFillFirstDay := AFillFirstDay;
    FFillLastDay := AFillLastDay;
  end else begin
    FFillFirstDay := EncodeDate(2000,1,1);
    FFillLastDay := DateOf(Now);
  end;
  FGradientDays:=Trunc(FFillLastDay + 1.0 - FFillFirstDay);
end;

function TFillingMapColorerSimple.GetColor(ATileInfo: ITileInfoBasic): TColor32;
var
  VFileExists: Boolean;
  VFileDate: TDateTime;
  VDateCompare: TValueRelationship;
  VC1,VC2: Double;
begin
  Result := 0;
  VFileExists := ATileInfo.IsExists;
  if VFileExists then begin
    if FFillMode=fmExisting then begin
      if FFilterMode then begin
        VFileDate := ATileInfo.LoadDate;
        VDateCompare :=CompareDate(VFileDate, FFillLastDay);
        if(VDateCompare<GreaterThanValue) then begin
          VDateCompare :=CompareDate(VFileDate, FFillFirstDay);
          if(VDateCompare>LessThanValue) then Result := FNoTileColor;
        end;
      end else begin
        Result := FNoTileColor;
      end;
    end else if FFillMode=fmGradient then begin
      VFileDate := ATileInfo.LoadDate;
      VDateCompare :=CompareDate(VFileDate,FFillLastDay);
      if(VDateCompare<>GreaterThanValue) then begin
        VDateCompare :=CompareDate(VFileDate,FFillFirstDay);
        if(VDateCompare<>LessThanValue) then begin
          VFileDate := FFillLastDay+1.0-VFileDate;
          VC1 := 255.0*(-1.0+2.0*VFileDate/FGradientDays);
          if(VC1>255.0) then VC1 := 255.0;
          if(VC1<0.0) then VC1 := 0.0;
          VC2 := 255.0*2.0*VFileDate/FGradientDays;
          if(VC2>255.0) then VC2 := 255.0;
          if(VC2<0.0) then VC2 := 0.0;
          Result := Color32(Trunc(VC1), Trunc(255.0-VC2), Trunc(VC2-VC1), AlphaComponent(FNoTileColor));
        end;
      end;
    end;
  end else begin
    if FFillMode = fmUnexisting then Result := FNoTileColor;
    if FShowTNE then begin
      if ATileInfo.IsExistsTNE then begin
        Result := FTNEColor;
      end;
    end;
  end;
end;

end.
