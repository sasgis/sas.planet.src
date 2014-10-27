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

unit u_CmdLineArgProcessor;

interface

uses
  Classes,
  ArgumentParser,
  i_ViewPortState,
  i_MainFormConfig,
  i_CmdLineArgProcessor,
  u_BaseInterfacedObject;

type
  TCmdLineArgProcessor = class(TBaseInterfacedObject, ICmdLineArgProcessor)
  private
    FViewPortState: IViewPortState;
    FMainFormConfig: IMainFormConfig;
    function ProcessInternal(const AList: TStringList): Integer;
  private
    function Process: Integer; overload;
    function Process(const AArgs: string): Integer; overload;
    function GetArguments: string;
    function GetErrorFromCode(const ACode: Integer): string;
  public
    constructor Create(
      const AViewPortState: IViewPortState;
      const AMainFormConfig: IMainFormConfig
    );
  end;

implementation

uses
  SysUtils,
  RegExpr,
  c_ZeroGUID,
  c_CmdLineArgProcessor,
  t_GeoTypes,
  i_MapType,
  i_CoordConverter,
  u_GeoFunc,
  u_GeoToStrFunc;

function StrToCoord(const AStr: string): TDoublePoint;
var
  VRegExpr: TRegExpr;
begin
  VRegExpr  := TRegExpr.Create;
  try
    VRegExpr.Expression := '\((.+?),(.+?)\)';
    if VRegExpr.Exec(AStr) then begin
      Result.X := str2r(VRegExpr.Match[1]);
      Result.Y := str2r(VRegExpr.Match[2]);
    end else begin
      Result := CEmptyDoublePoint;
    end;
  finally
    FreeAndNil(VRegExpr);
  end;
end;

function GetCoords(
  const AStr: string;
  const AGeoConverter: ICoordConverter;
  out ALonLat: TDoublePoint;
  var ECode: Integer
): Boolean;
begin
  ALonLat := StrToCoord(AStr);
  Result := not PointIsEmpty(ALonLat);
  if Result then begin
    Result := AGeoConverter.CheckLonLatPos(ALonLat);
    if not Result then begin
      ECode := ECode or cCmdLineArgProcessorLonLatOutOfBounds;
    end;
  end else begin
    ECode := ECode or cCmdLineArgProcessorLonLatParserError;
  end;
end;

function GetZoom(
  const AStr: string;
  const AGeoConverter: ICoordConverter;
  var AZoom: Byte;
  var ECode: Integer
): Boolean;
begin
  AZoom := StrToIntDef(AStr, 0);
  Result := (AZoom > 0);
  if Result then begin
    AZoom := AZoom - 1;
    Result := AGeoConverter.CheckZoom(AZoom);
    if not Result then begin
      ECode := ECode or cCmdLineArgProcessorZoomOutOfBounds;
    end;
  end else begin
    ECode := ECode or cCmdLineArgProcessorZoomParserError;
  end;
end;

function GetGUID(
  const AStr: string;
  out AGUID: TGUID;
  var ECode: Integer
): Boolean;
begin
  try
    AGUID := StringToGUID(AStr);
    Result := True;
  except
    Result := False;
    ECode := ECode or cCmdLineArgProcessorGUIDParserError;
  end;
end;

{ TCmdLineArgProcessor }

constructor TCmdLineArgProcessor.Create(
  const AViewPortState: IViewPortState;
  const AMainFormConfig: IMainFormConfig
);
begin
  inherited Create;
  FViewPortState := AViewPortState;
  FMainFormConfig := AMainFormConfig;
end;

function TCmdLineArgProcessor.GetArguments: string;
var
  VList: TStringList;
begin
  VList := GetParamStrAsList(False);
  try
    Result := VList.Text;
  finally
    VList.Free;
  end;
end;

function TCmdLineArgProcessor.Process: Integer;
var
  VList: TStringList;
begin
  VList := GetParamStrAsList(False);
  try
    Result := ProcessInternal(VList);
  finally
    VList.Free;
  end;
end;

function TCmdLineArgProcessor.Process(const AArgs: string): Integer;
var
  VList: TStringList;
begin
  VList := TStringList.Create;
  try
    VList.CommaText := ' ';
    VList.Text := AArgs;
    Result := ProcessInternal(VList);
  finally
    VList.Free;
  end;
end;

function TCmdLineArgProcessor.ProcessInternal(const AList: TStringList): Integer;

var
  VGUID: TGUID;
  VZoom: Byte;
  VLonLat: TDoublePoint;
  VMap: IMapType;
  VStrValue: string;
  VParser: TArgumentParser;
  VParseResult: TParseResult;
  VGeoConverter: ICoordConverter;

  function _GetCoordConverter: ICoordConverter;
  begin
    if not Assigned(VGeoConverter) then begin
      VGeoConverter := FViewPortState.View.GetStatic.GetGeoConverter;
    end;
    Result := VGeoConverter;
  end;

begin
  Result := cCmdLineArgProcessorOk;

  VParser := TArgumentParser.Create;
  try
    VParser.AddArgument('--map', saStore);              // --map={GUID}
    VParser.AddArgument('--zoom', saStore);             // --zoom={value}
    VParser.AddArgument('--move', saStore);             // --move=({lon},{lat})
    VParser.AddArgument('--navigation', saStore);       // --navigation=({lon},{lat})
    VParser.AddArgument('--show-placemarks', saStore);  // --show-placemarks={0/1}

    VParseResult := VParser.ParseArgs(AList);
    try
      if VParseResult.HasArgument('map') then begin
        VStrValue := VParseResult.GetValue('map');
        if GetGUID(VStrValue, VGUID, Result) then begin
          VMap := FMainFormConfig.MainMapsConfig.GetAllMapsSet.GetMapTypeByGUID(VGUID);
          if VMap <> nil then begin
            if VMap.Zmp.IsLayer then begin
              FMainFormConfig.MainMapsConfig.InvertLayerSelectionByGUID(VGUID);
            end else begin
              FMainFormConfig.MainMapsConfig.SelectMainByGUID(VGUID);
            end;
          end else begin
            Result := Result or cCmdLineArgProcessorUnknownGUID;
          end;
        end;
      end;

      if VParseResult.HasArgument('zoom') then begin
        VStrValue := VParseResult.GetValue('zoom');
        if GetZoom(VStrValue, _GetCoordConverter, VZoom, Result) then begin
          FViewPortState.ChangeZoomWithFreezeAtCenter(VZoom);
        end;
      end;

      if VParseResult.HasArgument('move') then begin
        VStrValue := VParseResult.GetValue('move');
        if GetCoords(VStrValue, _GetCoordConverter, VLonLat, Result) then begin
          FViewPortState.ChangeLonLat(VLonLat);
        end;
      end;

      if VParseResult.HasArgument('navigation') then begin
        VStrValue := VParseResult.GetValue('navigation');
        if GetCoords(VStrValue, _GetCoordConverter, VLonLat, Result) then begin
          FMainFormConfig.NavToPoint.StartNavLonLat(VLonLat);
        end;
      end;

      if VParseResult.HasArgument('show-placemarks') then begin
        VStrValue := VParseResult.GetValue('show-placemarks');
        if VStrValue = '1' then begin
          FMainFormConfig.LayersConfig.MarksLayerConfig.MarksShowConfig.IsUseMarks := True;
        end else if VStrValue = '0' then begin
          FMainFormConfig.LayersConfig.MarksLayerConfig.MarksShowConfig.IsUseMarks := False;
        end else begin
          Result := Result or cCmdLineArgProcessorShowMarksParserError;
        end;
      end;

    finally
      VParseResult.Free;
    end;
  finally
    VParser.Free;
  end;
end;

function TCmdLineArgProcessor.GetErrorFromCode(const ACode: Integer): string;
const
  cSep = ' && ';
var
  VSep: string;
begin
  Result := '';
  VSep := '';

  if ACode = cCmdLineArgProcessorOk then begin
    Exit;
  end;

  if (ACode and cCmdLineArgProcessorLonLatParserError) > 0 then begin
    Result := Result + VSep + rsCmdLineArgProcessorLonLatParserError;
    VSep := cSep;
  end;

  if (ACode and cCmdLineArgProcessorLonLatOutOfBounds) > 0 then begin
    Result := Result + VSep + rsCmdLineArgProcessorLonLatOutOfBounds;
    VSep := cSep;
  end;

  if (ACode and cCmdLineArgProcessorZoomParserError) > 0 then begin
    Result := Result + VSep + rsCmdLineArgProcessorZoomParserError;
    VSep := cSep;
  end;

  if (ACode and cCmdLineArgProcessorZoomOutOfBounds) > 0 then begin
    Result := Result + VSep + rsCmdLineArgProcessorZoomOutOfBounds;
    VSep := cSep;
  end;

  if (ACode and cCmdLineArgProcessorGUIDParserError) > 0 then begin
    Result := Result + VSep + rsCmdLineArgProcessorGUIDParserError;
    VSep := cSep;
  end;

  if (ACode and cCmdLineArgProcessorUnknownGUID) > 0 then begin
    Result := Result + VSep + rsCmdLineArgProcessorUnknownGUID;
    VSep := cSep;
  end;

  if (ACode and cCmdLineArgProcessorShowMarksParserError) > 0 then begin
    Result := Result + VSep + rsCmdLineArgProcessorShowMarksParserError;
    VSep := cSep;
  end;

  if Result = '' then begin
    Result := Format(rsCmdLineArgProcessorUnknownError, [IntToHex(ACode, 8)]);
  end;

  Result := Self.ClassName + ': ' + Result;
end;

end.
