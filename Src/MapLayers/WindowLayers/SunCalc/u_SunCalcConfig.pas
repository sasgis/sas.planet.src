{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit u_SunCalcConfig;

interface

uses  
  t_SunCalcConfig,
  i_SunCalcConfig,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataElementBase;

type
  TSunCalcConfig = class(TConfigDataElementBase, ISunCalcConfig)
  private
    FVisible: Boolean;
    FCircleRadius: Integer;
    FYearTimeLineHight: Integer;
    FDayTimeLineHight: Integer;
    FDetailsPanelRowHight: Integer;
    FDetailsPanelColsWidth: TSunCalcDetailsPanelColsWidth;
    FIsDetailedView: Boolean;
    FColorSchemaList: ISunCalcColorSchemaList;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    { ISunCalcConfig }
    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);

    function GetCircleRadius: Integer;
    procedure SetCircleRadius(const AValue: Integer);

    function GetYearTimeLineHight: Integer;
    procedure SetYearTimeLineHight(const AValue: Integer);

    function GetDayTimeLineHight: Integer;
    procedure SetDayTimeLineHight(const AValue: Integer);

    function GetDetailsPanelRowHight: Integer;
    procedure SetDetailsPanelRowHight(const AValue: Integer);

    function GetDetailsPanelColsWidth: TSunCalcDetailsPanelColsWidth;
    procedure SetDetailsPanelColsWidth(const AValue: TSunCalcDetailsPanelColsWidth);

    function GetIsDetailedView: Boolean;
    procedure SetIsDetailedView(const AValue: Boolean);

    function GetColorSchemaList: ISunCalcColorSchemaList;
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  GR32,
  i_InterfaceListSimple,
  u_InterfaceListSimple,
  u_ConfigProviderHelpers,
  u_BaseInterfacedObject;

resourcestring
  rsDefaultColorSchemaFmt = 'Color schema %d';

type
  TSunCalcColorSchemaStatic = class(TBaseInterfacedObject, ISunCalcColorSchemaStatic)
  private
    FSchemaName: string;
    FTimeLineColors: TSunCalcTimeLineColors;
    FTimeLineFont: TSunCalcFontInfo;
    FTimeLineHintFont: TSunCalcFontInfo;
    FDetailsPanelFont: TSunCalcFontInfo;
    FDetailsPanelColors: TSunCalcDetailsPanelColors;
    FShapesColors: TSunCalcShapesColors;
  private
    function GetSchemaName: string;
    function GetTimeLineColors: TSunCalcTimeLineColors;
    function GetTimeLineFont: TSunCalcFontInfo;
    function GetTimeLineHintFont: TSunCalcFontInfo;
    function GetDetailsPanelFont: TSunCalcFontInfo;
    function GetDetailsPanelColors: TSunCalcDetailsPanelColors;
    function GetShapesColors: TSunCalcShapesColors;
  public
    constructor Create(
      const ASchemaName: string;
      const ATimeLineColors: TSunCalcTimeLineColors;
      const ATimeLineFont: TSunCalcFontInfo;
      const ATimeLineHintFont: TSunCalcFontInfo;
      const ADetailsPanelFont: TSunCalcFontInfo;
      const ADetailsPanelColors: TSunCalcDetailsPanelColors;
      const AShapesColors: TSunCalcShapesColors
    );
  end;

  TSunCalcColorSchema = class(TConfigDataElementBase, ISunCalcColorSchema)
  private
    FSchemaName: string;
    FTimeLineColors: TSunCalcTimeLineColors;
    FTimeLineFont: TSunCalcFontInfo;
    FTimeLineHintFont: TSunCalcFontInfo;
    FDetailsPanelFont: TSunCalcFontInfo;
    FDetailsPanelColors: TSunCalcDetailsPanelColors;
    FShapesColors: TSunCalcShapesColors;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    { ISunCalcColorSchema }
    function GetSchemaName: string;
    procedure SetSchemaName(const AValue: string);

    function GetTimeLineColors: TSunCalcTimeLineColors;
    procedure SetTimeLineColors(const AValue: TSunCalcTimeLineColors);

    function GetTimeLineFont: TSunCalcFontInfo;
    procedure SetTimeLineFont(const AValue: TSunCalcFontInfo);

    function GetTimeLineHintFont: TSunCalcFontInfo;
    procedure SetTimeLineHintFont(const AValue: TSunCalcFontInfo);

    function GetDetailsPanelFont: TSunCalcFontInfo;
    procedure SetDetailsPanelFont(const AValue: TSunCalcFontInfo);

    function GetDetailsPanelColors: TSunCalcDetailsPanelColors;
    procedure SetDetailsPanelColors(const AValue: TSunCalcDetailsPanelColors);

    function GetShapesColors: TSunCalcShapesColors;
    procedure SetShapesColors(const AValue: TSunCalcShapesColors);

    function GetStatic: ISunCalcColorSchemaStatic;
  public
    constructor Create(const APresetIndex: Integer = 0);
  end;

  TSunCalcColorSchemaList = class(TConfigDataElementBase, ISunCalcColorSchemaList)
  private
    FList: IInterfaceListSimple;
    FActiveSchemaIndex: Integer;
    function CheckListIndex(const AIndex: Integer; const ASilent: Boolean): Boolean; inline;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    { ISunCalcColorSchemaList }
    function GetCount: Integer;

    function GetActiveSchemaIndex: Integer;
    procedure SetActiveSchemaIndex(const AValue: Integer);

    function GetColorSchemaByIndex(const AIndex: Integer): ISunCalcColorSchema;

    function GetActiveColorSchema: ISunCalcColorSchema;
  public
    constructor Create;
  end;

{$REGION 'TSunCalcConfig'}

{ TSunCalcConfig }

constructor TSunCalcConfig.Create;
begin
  inherited Create;

  FVisible := False;

  FCircleRadius := 270;

  FYearTimeLineHight := 28;
  FDayTimeLineHight := 42;

  FDetailsPanelRowHight := 23;

  FDetailsPanelColsWidth[0] := 50; // Time
  FDetailsPanelColsWidth[1] := 50; // Az.
  FDetailsPanelColsWidth[2] := 50; // Alt.
  FDetailsPanelColsWidth[3] := 200; // Event

  FIsDetailedView := False;

  FColorSchemaList := TSunCalcColorSchemaList.Create;
end;

procedure TSunCalcConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
var
  I: Integer;
begin
  inherited;
  if AConfigData <> nil then begin
    FCircleRadius := AConfigData.ReadInteger('CircleRadius', FCircleRadius);
    FYearTimeLineHight := AConfigData.ReadInteger('YearTimeLineHight', FYearTimeLineHight);
    FDayTimeLineHight := AConfigData.ReadInteger('DayTimeLineHight', FDayTimeLineHight);

    FDetailsPanelRowHight := AConfigData.ReadInteger('DetailsPanelRowHight', FDetailsPanelRowHight);
    for I := Low(FDetailsPanelColsWidth) to High(FDetailsPanelColsWidth) do begin
      FDetailsPanelColsWidth[I] := AConfigData.ReadInteger('DetailsPanelColsWidth' + IntToStr(I), FDetailsPanelColsWidth[I]);
    end;

    FIsDetailedView := AConfigData.ReadBool('IsDetailedView', FIsDetailedView);
    FColorSchemaList.ReadConfig(AConfigData.GetSubItem('ColorSchemaList'));
    SetChanged;
  end;
end;

procedure TSunCalcConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
var
  I: Integer;
begin
  inherited;
  if AConfigData <> nil then begin
    AConfigData.WriteInteger('CircleRadius', FCircleRadius);
    AConfigData.WriteInteger('YearTimeLineHight', FYearTimeLineHight);
    AConfigData.WriteInteger('DayTimeLineHight', FDayTimeLineHight);

    AConfigData.WriteInteger('DetailsPanelRowHight', FDetailsPanelRowHight);
    for I := Low(FDetailsPanelColsWidth) to High(FDetailsPanelColsWidth) do begin
      AConfigData.WriteInteger('DetailsPanelColsWidth' + IntToStr(I), FDetailsPanelColsWidth[I]);
    end;

    AConfigData.WriteBool('IsDetailedView', FIsDetailedView);
    FColorSchemaList.WriteConfig(AConfigData.GetOrCreateSubItem('ColorSchemaList'));
  end;
end;

function TSunCalcConfig.GetCircleRadius: Integer;
begin
  LockRead;
  try
    Result := FCircleRadius;
  finally
    UnlockRead;
  end;
end;

function TSunCalcConfig.GetColorSchemaList: ISunCalcColorSchemaList;
begin
  LockRead;
  try
    Result := FColorSchemaList;
  finally
    UnlockRead;
  end;
end;

function TSunCalcConfig.GetDayTimeLineHight: Integer;
begin
  LockRead;
  try
    Result := FDayTimeLineHight;
  finally
    UnlockRead;
  end;
end;

function TSunCalcConfig.GetDetailsPanelColsWidth: TSunCalcDetailsPanelColsWidth;
begin
  LockRead;
  try
    Result := FDetailsPanelColsWidth;
  finally
    UnlockRead;
  end;
end;

function TSunCalcConfig.GetDetailsPanelRowHight: Integer;
begin
  LockRead;
  try
    Result := FDetailsPanelRowHight;
  finally
    UnlockRead;
  end;
end;

function TSunCalcConfig.GetIsDetailedView: Boolean;
begin
  LockRead;
  try
    Result := FIsDetailedView;
  finally
    UnlockRead;
  end;
end;

function TSunCalcConfig.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

function TSunCalcConfig.GetYearTimeLineHight: Integer;
begin
  LockRead;
  try
    Result := FYearTimeLineHight;
  finally
    UnlockRead;
  end;
end;

procedure TSunCalcConfig.SetCircleRadius(const AValue: Integer);
begin
  LockWrite;
  try
    if FCircleRadius <> AValue then begin
      FCircleRadius := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSunCalcConfig.SetDayTimeLineHight(const AValue: Integer);
begin
  LockWrite;
  try
    if FDayTimeLineHight <> AValue then begin
      FDayTimeLineHight := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSunCalcConfig.SetDetailsPanelColsWidth(
  const AValue: TSunCalcDetailsPanelColsWidth
);
var
  I: Integer;
begin
  LockWrite;
  try
    for I := Low(FDetailsPanelColsWidth) to High(FDetailsPanelColsWidth) do begin
      if FDetailsPanelColsWidth[I] <> AValue[I] then begin
        FDetailsPanelColsWidth := AValue;
        SetChanged;
        Break;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSunCalcConfig.SetDetailsPanelRowHight(const AValue: Integer);
begin
  LockWrite;
  try
    if FDetailsPanelRowHight <> AValue then begin
      FDetailsPanelRowHight := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSunCalcConfig.SetIsDetailedView(const AValue: Boolean);
begin
  LockWrite;
  try
    if FIsDetailedView <> AValue then begin
      FIsDetailedView := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSunCalcConfig.SetVisible(const AValue: Boolean);
begin
  LockWrite;
  try
    if FVisible <> AValue then begin
      FVisible := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSunCalcConfig.SetYearTimeLineHight(const AValue: Integer);
begin
  LockWrite;
  try
    if FYearTimeLineHight <> AValue then begin
      FYearTimeLineHight := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

{$ENDREGION 'TSunCalcConfig'}

{$REGION 'TSunCalcColorSchemaList'}

{ TSunCalcColorSchemaList }

constructor TSunCalcColorSchemaList.Create;
begin
  inherited Create;
  FList := TInterfaceListSimple.Create;
  FActiveSchemaIndex := -1;
end;

procedure TSunCalcColorSchemaList.DoReadConfig(const AConfigData: IConfigDataProvider);
var
  I: Integer;
  VCount: Integer;
  VItem: ISunCalcColorSchema;
  VConfigData: IConfigDataProvider;
begin
  inherited;
  if AConfigData <> nil then begin
    VCount := AConfigData.ReadInteger('Count', 0);

    FActiveSchemaIndex := AConfigData.ReadInteger('ActiveSchemaIndex', FActiveSchemaIndex);
    if FActiveSchemaIndex <> -1 then begin
      Dec(FActiveSchemaIndex);
    end;

    if VCount > 0 then begin
      for I := 0 to VCount - 1 do begin
        VConfigData := AConfigData.GetSubItem('Item' + IntToStr(I+1));
        if Assigned(VConfigData) then begin
          VItem := TSunCalcColorSchema.Create;
          VItem.ReadConfig(VConfigData);
          FList.Add(VItem);
        end;
      end;
      if not CheckListIndex(FActiveSchemaIndex, True) then begin
        FActiveSchemaIndex := 0;
      end;
    end;
    if FList.Count = 0 then begin
      VItem := TSunCalcColorSchema.Create(0);
      FActiveSchemaIndex := FList.Add(VItem);

      VItem := TSunCalcColorSchema.Create(1);
      FList.Add(VItem);
    end;
    SetChanged;
  end;
end;

procedure TSunCalcColorSchemaList.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
var
  I: Integer;
  VCount: Integer;
  VItem: ISunCalcColorSchema;
begin
  inherited;
  if AConfigData <> nil then begin
    VCount := FList.Count;

    AConfigData.WriteInteger('Count', VCount);

    I := FActiveSchemaIndex;
    if I <> -1 then begin
      Inc(I);
    end;
    AConfigData.WriteInteger('ActiveSchemaIndex', I);

    for I := 0 to VCount - 1 do begin
      VItem := FList.Items[I] as ISunCalcColorSchema;
      VItem.WriteConfig(AConfigData.GetOrCreateSubItem('Item' + IntToStr(I+1)));
    end;
  end;
end;

function TSunCalcColorSchemaList.CheckListIndex(
  const AIndex: Integer;
  const ASilent: Boolean
): Boolean;
const
  cErrMsg = 'TSunCalcColorSchemaList: Index out of bounds: %d (Count = %d)';
begin
  Result := (AIndex >= 0) and (AIndex < FList.Count);
  if not Result and not ASilent then begin
    raise Exception.CreateFmt(cErrMsg, [AIndex, FList.Count]);
  end;
end;

function TSunCalcColorSchemaList.GetActiveColorSchema: ISunCalcColorSchema;
begin
  LockRead;
  try
    if CheckListIndex(FActiveSchemaIndex, False) then begin
      Result := FList.Items[FActiveSchemaIndex] as ISunCalcColorSchema;
    end else begin
      Result := nil; // never should happen
    end;
  finally
    UnlockRead;
  end;
end;

function TSunCalcColorSchemaList.GetActiveSchemaIndex: Integer;
begin
  LockRead;
  try
    Result := FActiveSchemaIndex;
  finally
    UnlockRead;
  end;
end;

function TSunCalcColorSchemaList.GetColorSchemaByIndex(
  const AIndex: Integer
): ISunCalcColorSchema;
begin
  LockRead;
  try
    if CheckListIndex(AIndex, False) then begin
      Result := FList.Items[AIndex] as ISunCalcColorSchema;
    end else begin
      Result := nil; // never should happen
    end;
  finally
    UnlockRead;
  end;
end;

function TSunCalcColorSchemaList.GetCount: Integer;
begin
  LockRead;
  try
    Result := FList.Count;
  finally
    UnlockRead;
  end;
end;

procedure TSunCalcColorSchemaList.SetActiveSchemaIndex(const AValue: Integer);
begin
  LockWrite;
  try
    if FActiveSchemaIndex <> AValue then begin
      if CheckListIndex(AValue, False) then begin
        FActiveSchemaIndex := AValue;
        SetChanged;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

{$ENDREGION 'TSunCalcColorSchemaList'}

{$REGION 'TSunCalcColorSchemaStatic'}

{ TSunCalcColorSchemaStatic }

constructor TSunCalcColorSchemaStatic.Create(
  const ASchemaName: string;
  const ATimeLineColors: TSunCalcTimeLineColors;
  const ATimeLineFont: TSunCalcFontInfo;
  const ATimeLineHintFont: TSunCalcFontInfo;
  const ADetailsPanelFont: TSunCalcFontInfo;
  const ADetailsPanelColors: TSunCalcDetailsPanelColors;
  const AShapesColors: TSunCalcShapesColors
);
begin
  inherited Create;
  FSchemaName := ASchemaName;
  FTimeLineColors := ATimeLineColors;
  FTimeLineFont := ATimeLineFont;
  FTimeLineHintFont := ATimeLineHintFont;
  FDetailsPanelFont := ADetailsPanelFont;
  FDetailsPanelColors := ADetailsPanelColors;
  FShapesColors := AShapesColors;
end;

function TSunCalcColorSchemaStatic.GetSchemaName: string;
begin
  Result := FSchemaName;
end;

function TSunCalcColorSchemaStatic.GetShapesColors: TSunCalcShapesColors;
begin
  Result := FShapesColors;
end;

function TSunCalcColorSchemaStatic.GetTimeLineColors: TSunCalcTimeLineColors;
begin
  Result := FTimeLineColors;
end;

function TSunCalcColorSchemaStatic.GetTimeLineFont: TSunCalcFontInfo;
begin
  Result := FTimeLineFont;
end;

function TSunCalcColorSchemaStatic.GetTimeLineHintFont: TSunCalcFontInfo;
begin
  Result := FTimeLineHintFont;
end;

function TSunCalcColorSchemaStatic.GetDetailsPanelColors: TSunCalcDetailsPanelColors;
begin
  Result := FDetailsPanelColors;
end;

function TSunCalcColorSchemaStatic.GetDetailsPanelFont: TSunCalcFontInfo;
begin
  Result := FDetailsPanelFont;
end;

{$ENDREGION 'TSunCalcColorSchemaStatic'}

{$REGION 'TSunCalcColorSchema'}

{ TSunCalcColorSchema }

constructor TSunCalcColorSchema.Create(const APresetIndex: Integer);

  function OpacityToInt(const AValue: Double): Integer;
  begin
    Result := Round(AValue * 255);
  end;

begin
  inherited Create;

  FSchemaName := Format(rsDefaultColorSchemaFmt, [1]);

  FTimeLineColors.BgColor := SetAlpha(clBlack32, $50);
  FTimeLineColors.VertLinesColor := SetAlpha(clSilver32, $AA);
  FTimeLineColors.YearLineColor := SetAlpha(clSilver32, $AA);

  Assert(Length(FTimeLineColors.DayLineColors) = 6);
  FTimeLineColors.DayLineColors[0] := $ff596f98; // night
  FTimeLineColors.DayLineColors[1] := $ff41aaef; // astronomical twilight
  FTimeLineColors.DayLineColors[2] := $ff7cc1ef; // nautical twilight
  FTimeLineColors.DayLineColors[3] := $ffc0ddf0; // civil twilight
  FTimeLineColors.DayLineColors[4] := $ffffc3ad; // golden hour
  FTimeLineColors.DayLineColors[5] := $ffffed9e; // day

  FTimeLineFont.FontName := ''; // use Default font
  FTimeLineFont.FontSize := 0;  // use Default size
  FTimeLineFont.TextColor := SetAlpha(clWhite32, $FF);
  FTimeLineFont.BgColor := 0; // not used

  FTimeLineHintFont.FontName := '';
  FTimeLineHintFont.FontSize := 0;
  FTimeLineHintFont.TextColor := clWhite32;
  FTimeLineHintFont.BgColor := SetAlpha(clSilver32, 230);

  FDetailsPanelFont.FontName := 'Arial';
  FDetailsPanelFont.FontSize := 10;
  FDetailsPanelFont.TextColor := clWhite32;
  FDetailsPanelFont.BgColor := 0; // not used

  FDetailsPanelColors.BgColor := SetAlpha(clBlack32, $80);
  FDetailsPanelColors.GridLinesColor := SetAlpha(clBlack32, $30);

  // suncalc.net colors - good for light background (i.e. for all kind of maps)
  FShapesColors.DayLineColor := SetAlpha($ffffa500, OpacityToInt(0.9));
  FShapesColors.DayPolyLineColor := SetAlpha($ffffa500, OpacityToInt(0.7));

  FShapesColors.DaySunriseLineColor := SetAlpha($ffffd700, OpacityToInt(0.9));
  FShapesColors.DaySunsetLineColor := SetAlpha($ffff4500, OpacityToInt(0.6));

  FShapesColors.YearCircleColor := SetAlpha($ff000000, OpacityToInt(0.5));
  FShapesColors.YearPolyLinesColor := SetAlpha($ff000000, OpacityToInt(0.4));
  FShapesColors.YearPolygonFillColor := SetAlpha($ffffd700, OpacityToInt(0.2));

  // custom colors for dark background (i.e. for all kind of satellite images)
  if APresetIndex > 0 then begin
    FSchemaName := Format(rsDefaultColorSchemaFmt, [2]);
    FShapesColors.YearCircleColor := clSilver32;
    FShapesColors.YearPolyLinesColor := clSilver32;
  end;
end;

function TSunCalcColorSchema.GetStatic: ISunCalcColorSchemaStatic;
begin
  LockRead;
  try
    Result :=
      TSunCalcColorSchemaStatic.Create(
        FSchemaName,
        FTimeLineColors,
        FTimeLineFont,
        FTimeLineHintFont,
        FDetailsPanelFont,
        FDetailsPanelColors,
        FShapesColors
      );
  finally
    UnlockRead;
  end;
end;

procedure TSunCalcColorSchema.DoReadConfig(const AConfigData: IConfigDataProvider);

  procedure ReadFontRec(var ARec: TSunCalcFontInfo; const APrefix: string);
  begin
    ARec.FontName := AConfigData.ReadString(APrefix + '_FontName', ARec.FontName);
    ARec.FontSize := AConfigData.ReadInteger(APrefix + '_FontSize', ARec.FontSize);
    ARec.TextColor := ReadColor32(AConfigData, APrefix + '_TextColor', ARec.TextColor);
    ARec.BgColor := ReadColor32(AConfigData, APrefix + '_BgColor', ARec.BgColor);
  end;

var
  I: Integer;
begin
  inherited;
  if AConfigData <> nil then begin
    FSchemaName := AConfigData.ReadString('SchemaName', FSchemaName);

    FTimeLineColors.BgColor := ReadColor32(AConfigData, 'TimeLineColors_BgColor', FTimeLineColors.BgColor);
    FTimeLineColors.VertLinesColor := ReadColor32(AConfigData, 'TimeLineColors_VertLinesColor', FTimeLineColors.VertLinesColor);
    FTimeLineColors.YearLineColor := ReadColor32(AConfigData, 'TimeLineColors_YearLineColor', FTimeLineColors.YearLineColor);
    for I := Low(FTimeLineColors.DayLineColors) to High((FTimeLineColors.DayLineColors)) do begin
      FTimeLineColors.DayLineColors[I] := ReadColor32(AConfigData, 'TimeLineColors_DayLineColors' + IntToStr(I), FTimeLineColors.DayLineColors[I]);
    end;

    ReadFontRec(FTimeLineFont, 'TimeLineFont');
    ReadFontRec(FTimeLineHintFont, 'TimeLineHintFont');
    ReadFontRec(FDetailsPanelFont, 'DetailsPanelFont');

    FDetailsPanelColors.BgColor := ReadColor32(AConfigData, 'DetailsPanelColors_BgColor', FDetailsPanelColors.BgColor);
    FDetailsPanelColors.GridLinesColor := ReadColor32(AConfigData, 'DetailsPanelColors_GridLinesColor', FDetailsPanelColors.GridLinesColor);

    FShapesColors.DayLineColor := ReadColor32(AConfigData, 'ShapesColors_DayLineColor', FShapesColors.DayLineColor);
    FShapesColors.DayPolyLineColor := ReadColor32(AConfigData, 'ShapesColors_DayPolyLineColor', FShapesColors.DayPolyLineColor);
    FShapesColors.DaySunriseLineColor := ReadColor32(AConfigData, 'ShapesColors_DaySunriseLineColor', FShapesColors.DaySunriseLineColor);
    FShapesColors.DaySunsetLineColor := ReadColor32(AConfigData, 'ShapesColors_DaySunsetLineColor', FShapesColors.DaySunsetLineColor);
    FShapesColors.YearCircleColor := ReadColor32(AConfigData, 'ShapesColors_YearCircleColor', FShapesColors.YearCircleColor);
    FShapesColors.YearPolyLinesColor := ReadColor32(AConfigData, 'ShapesColors_YearPolyLinesColor', FShapesColors.YearPolyLinesColor);
    FShapesColors.YearPolygonFillColor := ReadColor32(AConfigData, 'ShapesColors_YearPolygonFillColor', FShapesColors.YearPolygonFillColor);

    SetChanged;
  end;
end;

procedure TSunCalcColorSchema.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);

  procedure WriteFontRec(const ARec: TSunCalcFontInfo; const APrefix: string);
  begin
    AConfigData.WriteString(APrefix + '_FontName', ARec.FontName);
    AConfigData.WriteInteger(APrefix + '_FontSize', ARec.FontSize);
    WriteColor32(AConfigData, APrefix + '_TextColor', ARec.TextColor);
    WriteColor32(AConfigData, APrefix + '_BgColor', ARec.BgColor);
  end;

var
  I: Integer;
begin
  inherited;
  if AConfigData <> nil then begin
    AConfigData.WriteString('SchemaName', FSchemaName);

    WriteColor32(AConfigData, 'TimeLineColors_BgColor', FTimeLineColors.BgColor);
    WriteColor32(AConfigData, 'TimeLineColors_VertLinesColor', FTimeLineColors.VertLinesColor);
    WriteColor32(AConfigData, 'TimeLineColors_YearLineColor', FTimeLineColors.YearLineColor);
    for I := Low(FTimeLineColors.DayLineColors) to High((FTimeLineColors.DayLineColors)) do begin
      WriteColor32(AConfigData, 'TimeLineColors_DayLineColors' + IntToStr(I), FTimeLineColors.DayLineColors[I]);
    end;

    WriteFontRec(FTimeLineFont, 'TimeLineFont');
    WriteFontRec(FTimeLineHintFont, 'TimeLineHintFont');
    WriteFontRec(FDetailsPanelFont, 'DetailsPanelFont');

    WriteColor32(AConfigData, 'DetailsPanelColors_BgColor', FDetailsPanelColors.BgColor);
    WriteColor32(AConfigData, 'DetailsPanelColors_GridLinesColor', FDetailsPanelColors.GridLinesColor);

    WriteColor32(AConfigData, 'ShapesColors_DayLineColor', FShapesColors.DayLineColor);
    WriteColor32(AConfigData, 'ShapesColors_DayPolyLineColor', FShapesColors.DayPolyLineColor);
    WriteColor32(AConfigData, 'ShapesColors_DaySunriseLineColor', FShapesColors.DaySunriseLineColor);
    WriteColor32(AConfigData, 'ShapesColors_DaySunsetLineColor', FShapesColors.DaySunsetLineColor);
    WriteColor32(AConfigData, 'ShapesColors_YearCircleColor', FShapesColors.YearCircleColor);
    WriteColor32(AConfigData, 'ShapesColors_YearPolyLinesColor', FShapesColors.YearPolyLinesColor);
    WriteColor32(AConfigData, 'ShapesColors_YearPolygonFillColor', FShapesColors.YearPolygonFillColor);
  end;
end;

function TSunCalcColorSchema.GetDetailsPanelColors: TSunCalcDetailsPanelColors;
begin
  LockRead;
  try
    Result := FDetailsPanelColors;
  finally
    UnlockRead;
  end;
end;

function TSunCalcColorSchema.GetDetailsPanelFont: TSunCalcFontInfo;
begin
  LockRead;
  try
    Result := FDetailsPanelFont;
  finally
    UnlockRead;
  end;
end;

function TSunCalcColorSchema.GetSchemaName: string;
begin
  LockRead;
  try
    Result := FSchemaName;
  finally
    UnlockRead;
  end;
end;

function TSunCalcColorSchema.GetShapesColors: TSunCalcShapesColors;
begin
  LockRead;
  try
    Result := FShapesColors;
  finally
    UnlockRead;
  end;
end;

function TSunCalcColorSchema.GetTimeLineColors: TSunCalcTimeLineColors;
begin
  LockRead;
  try
    Result := FTimeLineColors;
  finally
    UnlockRead;
  end;
end;

function TSunCalcColorSchema.GetTimeLineFont: TSunCalcFontInfo;
begin
  LockRead;
  try
    Result := FTimeLineFont;
  finally
    UnlockRead;
  end;
end;

function TSunCalcColorSchema.GetTimeLineHintFont: TSunCalcFontInfo;
begin
  LockRead;
  try
    Result := FTimeLineHintFont;
  finally
    UnlockRead;
  end;
end;

procedure TSunCalcColorSchema.SetDetailsPanelColors(
  const AValue: TSunCalcDetailsPanelColors
);
begin
  LockWrite;
  try
    if FDetailsPanelColors <> AValue then begin
      FDetailsPanelColors := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSunCalcColorSchema.SetDetailsPanelFont(
  const AValue: TSunCalcFontInfo
);
begin
  LockWrite;
  try
    if FDetailsPanelFont <> AValue then begin
      FDetailsPanelFont := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSunCalcColorSchema.SetSchemaName(const AValue: string);
begin
  LockWrite;
  try
    if FSchemaName <> AValue then begin
      FSchemaName := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSunCalcColorSchema.SetShapesColors(const AValue: TSunCalcShapesColors);
begin
  LockWrite;
  try
    if FShapesColors <> AValue then begin
      FShapesColors := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSunCalcColorSchema.SetTimeLineColors(const AValue: TSunCalcTimeLineColors);
begin
  LockWrite;
  try
    if FTimeLineColors <> AValue then begin
      FTimeLineColors := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSunCalcColorSchema.SetTimeLineFont(const AValue: TSunCalcFontInfo);
begin
  LockWrite;
  try
    if FTimeLineFont <> AValue then begin
      FTimeLineFont := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSunCalcColorSchema.SetTimeLineHintFont(
  const AValue: TSunCalcFontInfo
);
begin
  LockWrite;
  try
    if FTimeLineHintFont <> AValue then begin
      FTimeLineHintFont := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

{$ENDREGION 'TSunCalcColorSchema'}

end.
