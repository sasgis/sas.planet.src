unit u_FillingMapColorerSimple;

interface

uses
  GR32,
  t_FillingMapModes,
  t_RangeFillingMap,
  i_TileInfoBasic,
  i_FillingMapColorer;

type
  PTileInfo = ^TTileInfo;

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
  private
    function InternalGetColor(
      const ATileInfoOptional: PTileInfo;
      const ARangeItemPtr: Pointer;
      const ARangeItemLen: SmallInt
    ): TColor32;
  protected
    function GetColor(const ATileInfo: ITileInfoBasic): TColor32; overload;
    function GetColor(const ATileInfo: TTileInfo): TColor32; overload;
    function GetRangeColor(
      const ARangeItemPtr: Pointer;
      const ARangeItemLen: SmallInt
    ): TColor32;
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
  inherited Create;
  FNoTileColor := ANoTileColor;
  FShowTNE := AShowTNE;
  FTNEColor := ATNEColor;
  FFillMode := AFillMode;
  FFilterMode := AFilterMode;
  if FFilterMode then begin
    FFillFirstDay := AFillFirstDay;
    FFillLastDay := AFillLastDay;
  end else begin
    FFillFirstDay := EncodeDate(2000, 1, 1);
    FFillLastDay := DateOf(Now);
  end;
  FGradientDays := Trunc(FFillLastDay + 1.0 - FFillFirstDay);
end;

function TFillingMapColorerSimple.GetColor(const ATileInfo: ITileInfoBasic): TColor32;
var
  VTileInfo: TTileInfo;
begin
  try
    VTileInfo.FLoadDate := ATileInfo.LoadDate;
    VTileInfo.FSize := ATileInfo.Size;
    VTileInfo.FVersionInfo := ATileInfo.VersionInfo;
    VTileInfo.FContentType := ATileInfo.ContentType;
    VTileInfo.FData := ATileInfo.TileData;
    if ATileInfo.IsExists then begin
      VTileInfo.FInfoType := titExists;
    end else if ATileInfo.IsExistsTNE then begin
      VTileInfo.FInfoType := titTneExists;
    end else begin
      VTileInfo.FInfoType := titNotExists;
    end;
    Result := GetColor(VTileInfo);
  finally
    VTileInfo.FVersionInfo := nil;
    VTileInfo.FContentType := nil;
    VTileInfo.FData := nil;
  end;
end;

function TFillingMapColorerSimple.GetColor(
  const ATileInfo: TTileInfo
): TColor32;
begin
  Result := InternalGetColor(@ATileInfo, nil, 0);
end;

function TFillingMapColorerSimple.GetRangeColor(
  const ARangeItemPtr: Pointer;
  const ARangeItemLen: SmallInt
): TColor32;
begin
  Result := InternalGetColor(nil, ARangeItemPtr, ARangeItemLen);
end;

function TFillingMapColorerSimple.InternalGetColor(
  const ATileInfoOptional: PTileInfo;
  const ARangeItemPtr: Pointer;
  const ARangeItemLen: SmallInt
): TColor32;

  function _GetIsExists: Boolean;
  begin
    if Assigned(ATileInfoOptional) then begin
      Result := ATileInfoOptional.FInfoType = titExists;
    end else begin
      case ARangeItemLen of
        SizeOf(TRangeFillingItem1): begin
          Result := PRangeFillingItem1(ARangeItemPtr)^.IsTileExists;
        end;
        SizeOf(TRangeFillingItem4): begin
          Result := PRangeFillingItem4(ARangeItemPtr)^.IsTileExists;
        end;
        SizeOf(TRangeFillingItem8): begin
          Result := PRangeFillingItem8(ARangeItemPtr)^.IsTileExists;
        end;
      else begin
        Result := FALSE;
      end;
      end;
    end;
  end;

  function _GetIsExistsTNE: Boolean;
  begin
    if Assigned(ATileInfoOptional) then begin
      Result := ATileInfoOptional.FInfoType = titTneExists;
    end else begin
      case ARangeItemLen of
        SizeOf(TRangeFillingItem1): begin
          Result := PRangeFillingItem1(ARangeItemPtr)^.IsTNEExists;
        end;
        SizeOf(TRangeFillingItem4): begin
          Result := PRangeFillingItem4(ARangeItemPtr)^.IsTNEExists;
        end;
        SizeOf(TRangeFillingItem8): begin
          Result := PRangeFillingItem8(ARangeItemPtr)^.IsTNEExists;
        end;
      else begin
        Result := FALSE;
      end;
      end;
    end;
  end;

  function _GetLoadDate: TDateTime;
  begin
    if Assigned(ATileInfoOptional) then begin
      Result := ATileInfoOptional.FLoadDate;
    end else begin
      case ARangeItemLen of
        SizeOf(TRangeFillingItem1): begin
          Result := 0; // no datetime
        end;
        SizeOf(TRangeFillingItem4): begin
          Result := PRangeFillingItem4(ARangeItemPtr)^.GetAsDateTime;
        end;
        SizeOf(TRangeFillingItem8): begin
          Result := PRangeFillingItem8(ARangeItemPtr)^.GetAsDateTime;
        end;
      else begin
        Result := 0;
      end;
      end;
    end;
  end;

var
  VFileExists: Boolean;
  VFileDate: TDateTime;
  VDateCompare: TValueRelationship;
  VC1, VC2: Double;
begin
  Result := 0;
  VFileExists := _GetIsExists;
  if VFileExists then begin
    if FFillMode = fmExisting then begin
      if FFilterMode then begin
        VFileDate := _GetLoadDate;
        VDateCompare := CompareDate(VFileDate, FFillLastDay);
        if (VDateCompare < GreaterThanValue) then begin
          VDateCompare := CompareDate(VFileDate, FFillFirstDay);
          if (VDateCompare > LessThanValue) then begin
            Result := FNoTileColor;
          end;
        end;
      end else begin
        Result := FNoTileColor;
      end;
    end else if FFillMode = fmGradient then begin
      VFileDate := _GetLoadDate;
      VDateCompare := CompareDate(VFileDate, FFillLastDay);
      if (VDateCompare <> GreaterThanValue) then begin
        VDateCompare := CompareDate(VFileDate, FFillFirstDay);
        if (VDateCompare <> LessThanValue) then begin
          VFileDate := FFillLastDay + 1.0 - VFileDate;
          VC1 := 255.0 * (-1.0 + 2.0 * VFileDate / FGradientDays);
          if (VC1 > 255.0) then begin
            VC1 := 255.0;
          end;
          if (VC1 < 0.0) then begin
            VC1 := 0.0;
          end;
          VC2 := 255.0 * 2.0 * VFileDate / FGradientDays;
          if (VC2 > 255.0) then begin
            VC2 := 255.0;
          end;
          if (VC2 < 0.0) then begin
            VC2 := 0.0;
          end;
          Result := Color32(Trunc(VC1), Trunc(255.0 - VC2), Trunc(VC2 - VC1), AlphaComponent(FNoTileColor));
        end;
      end;
    end;
  end else begin
    if FFillMode = fmUnexisting then begin
      Result := FNoTileColor;
    end;
    if FShowTNE then begin
      if _GetIsExistsTNE then begin
        Result := FTNEColor;
      end;
    end;
  end;
end;

end.
