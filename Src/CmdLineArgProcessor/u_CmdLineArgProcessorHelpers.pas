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

unit u_CmdLineArgProcessorHelpers;

interface

uses
  Classes,
  t_GeoTypes,
  i_MarkSystem,
  i_MapViewGoto,
  i_ProjectionType,
  i_ProjectionSet,
  i_RegionProcess,
  i_StringListStatic,
  i_GeometryLonLatFactory,
  i_AppearanceOfMarkFactory,
  i_VectorItemTreeImporterList,
  u_MarkDbGUIHelper;

function GetCoords(
  const AStr: string;
  const AProjectionType: IProjectionType;
  out ALonLat: TDoublePoint;
  var ECode: Integer
): Boolean;

function GetZoom(
  const AStr: string;
  const AProjectionSet: IProjectionSet;
  var AZoom: Byte;
  var ECode: Integer
): Boolean;

function GetGUID(
  const AStr: string;
  out AGUID: TGUID;
  var ECode: Integer
): Boolean;

procedure ProcessImportPlacemark(
  const AStr: string;
  const AMarkSystem: IMarkSystem;
  const AGeometryLonLatFactory: IGeometryLonLatFactory
);

procedure ProcessOpenFiles(
  const AFiles: IStringListStatic;
  const AMapGoto: IMapViewGoto;
  const ARegionProcess: IRegionProcessFromFile;
  const AShowImportDlg: Boolean = False;
  const AMarkDBGUIHelper: TMarkDBGUIHelper = nil;
  const AMarkSystem: IMarkSystem = nil;
  const AImporterList: IVectorItemTreeImporterListChangeable = nil;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory = nil
);

function GetArgsAsList(const AArgs: string): TStringList;
function GetUnquotedStr(const AText: string): string;

implementation

uses
  SysUtils,
  StrUtils,
  Math,
  RegExpr,
  i_NotifierOperation,
  i_ImportConfig,
  i_MarkFactoryConfig,
  i_MarkTemplate,
  i_InterfaceListStatic,
  i_VectorDataItemSimple,
  i_VectorItemTree,
  i_VectorItemTreeImporter,
  i_GeometryLonLat,
  i_MarkCategory,
  i_MarkCategoryDB,
  i_Appearance,
  i_JpegWithExifImportConfig,
  u_JpegWithExifImportConfig,
  u_GeoFunc,
  u_GeoToStrFunc,
  u_ImportConfig,
  u_NotifierOperation,
  u_CmdLineArgProcessorAPI;

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
  const AProjectionType: IProjectionType;
  out ALonLat: TDoublePoint;
  var ECode: Integer
): Boolean;
begin
  ALonLat := StrToCoord(AStr);
  Result := not PointIsEmpty(ALonLat);
  if Result then begin
    Result := AProjectionType.CheckLonLatPos(ALonLat);
    if not Result then begin
      ALonLat := DoublePoint(0, 0);
      ECode := ECode or cCmdLineArgProcessorLonLatOutOfBounds;
    end;
  end else begin
    ECode := ECode or cCmdLineArgProcessorLonLatParserError;
  end;
end;

function GetZoom(
  const AStr: string;
  const AProjectionSet: IProjectionSet;
  var AZoom: Byte;
  var ECode: Integer
): Boolean;
begin
  AZoom := StrToIntDef(AStr, 0);
  Result := (AZoom > 0);
  if Result then begin
    AZoom := AZoom - 1;
    Result := AProjectionSet.CheckZoom(AZoom);
    if not Result then begin
      AZoom := 0;
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

function GetUnquotedStr(const AText: string): string;
var
  I: Integer;
begin
  I := Length(AText);
  if (AText[1] = '"') and (AText[I] = '"') then begin
    if I > 2 then begin
      Result := Copy(AText, 2, I-2);
    end else begin
      Result := '';
    end;
  end else begin
    Result := AText;
  end;
end;

function GetTempCategory(const ACategoryDB: IMarkCategoryDB): IMarkCategory;
const
  cTmpCategoryName = 'TEMP';
begin
  Result := ACategoryDB.GetCategoryByName(cTmpCategoryName);
  if not Assigned(Result) then begin
    Result := ACategoryDB.Factory.CreateNew(cTmpCategoryName);
    if Assigned(Result) then begin
      ACategoryDB.UpdateCategory(nil, Result);
    end else begin
      Assert(False);
    end;
  end;
end;

procedure ProcessImportPlacemark(
  const AStr: string;
  const AMarkSystem: IMarkSystem;
  const AGeometryLonLatFactory: IGeometryLonLatFactory
);
const
  cSep = ';';
var
  I, J: Integer;
  VName: string;
  VCoords: string;
  VDesc: string;
  VLonLat: TDoublePoint;
  VConfig: IMarkFactoryConfig;
  VPointTemplate: IMarkTemplatePoint;
  VMark: IVectorDataItem;
begin
  Assert(AMarkSystem <> nil);
  Assert(AGeometryLonLatFactory <> nil);

  I := Pos(cSep, AStr);
  if I > 0 then begin
    VName := Copy(AStr, 1, I-1);
    VName := GetUnquotedStr(VName);
    Inc(I);
    J := PosEx(cSep, AStr, I);
    if J > 0 then begin
      VCoords := Copy(AStr, I, J-I);
      VLonLat := StrToCoord(VCoords);
      if not PointIsEmpty(VLonLat) then begin
        Inc(J);
        I := Length(AStr);
        if J <= I then begin
          VDesc := Copy(AStr, J, I-J+1);
          VDesc := GetUnquotedStr(VDesc);
        end else begin
          VDesc := '';
        end;

        VConfig := AMarkSystem.MarkDb.Factory.Config;
        VPointTemplate := VConfig.PointTemplateConfig.DefaultTemplate;

        VMark :=
          AMarkSystem.MarkDb.Factory.CreateMark(
            AGeometryLonLatFactory.CreateLonLatPoint(VLonLat),
            VName,
            VDesc,
            GetTempCategory(AMarkSystem.CategoryDB),
            VPointTemplate.Appearance
          );

        AMarkSystem.MarkDb.UpdateMark(nil, VMark);
      end;
    end;
  end;
end;

function CreateJpegImportConfig(
  const AMarkSystem: IMarkSystem;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory
): IJpegWithExifImportConfig;
var
  VIsForceSet: Boolean;
  VConfig: IMarkFactoryConfig;
  VPointTemplate: IMarkTemplatePoint;
  VPointParams: IImportPointParams;
  VAppearance: IAppearance;
begin
  VIsForceSet := False;

  VConfig := AMarkSystem.MarkDb.Factory.Config;

  VPointTemplate := VConfig.PointTemplateConfig.DefaultTemplate;

  VAppearance :=
    AAppearanceOfMarkFactory.CreatePointAppearance(
      VPointTemplate.CaptionAppearance.TextColor,
      VPointTemplate.CaptionAppearance.TextBgColor,
      VPointTemplate.CaptionAppearance.FontSize,
      VPointTemplate.IconAppearance.PicName,
      VPointTemplate.IconAppearance.Pic,
      64
    );

  VPointParams :=
    TImportPointParams.Create(
      VAppearance,
      VIsForceSet,
      VIsForceSet,
      VIsForceSet,
      VIsForceSet,
      VIsForceSet
    );

  Result :=
    TJpegWithExifImportConfig.Create(
      GetTempCategory(AMarkSystem.CategoryDB),
      TImportCategoryParams.Create(True, False, False, False),
      VPointParams,
      True,
      600
    );
end;

function CreateCommonImportConfig(
  const AMarkSystem: IMarkSystem
): IImportConfig;
var
  VConfig: IMarkFactoryConfig;
  VPointTemplate: IMarkTemplatePoint;
  VPathTemplate: IMarkTemplateLine;
  VPolyTemplate: IMarkTemplatePoly;
  VPointParams: IImportPointParams;
  VLineParams: IImportLineParams;
  VPolyParams: IImportPolyParams;
  VIsForceSet: Boolean;
begin
  VIsForceSet := False;

  VConfig := AMarkSystem.MarkDb.Factory.Config;

  VPointTemplate := VConfig.PointTemplateConfig.DefaultTemplate;
  VPathTemplate := VConfig.LineTemplateConfig.DefaultTemplate;
  VPolyTemplate := VConfig.PolyTemplateConfig.DefaultTemplate;

  VPointParams :=
    TImportPointParams.Create(
      VPointTemplate.Appearance,
      VIsForceSet,
      VIsForceSet,
      VIsForceSet,
      VIsForceSet,
      VIsForceSet
    );

  VLineParams :=
    TImportLineParams.Create(
      VPathTemplate.Appearance,
      VIsForceSet,
      VIsForceSet
    );

  VPolyParams :=
    TImportPolyParams.Create(
      VPolyTemplate.Appearance,
      VIsForceSet,
      VIsForceSet,
      VIsForceSet
    );

  Result :=
    TImportConfig.Create(
      GetTempCategory(AMarkSystem.CategoryDB),
      TImportCategoryParams.Create(True, False, False, False),
      VPointParams,
      VLineParams,
      VPolyParams
    );
end;

function ImportFiles(
  const AFiles: IStringListStatic;
  const AImporterList: IVectorItemTreeImporterListStatic;
  const AMarkSystem: IMarkSystem;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory
): IInterfaceListStatic;

type
  TImportRec = record
    FExt: string;
    FConfig: IInterface;
    FImporter: IVectorItemTreeImporter;
  end;
  TImportRecArr = array of TImportRec;

  function _RecIndexByFileExt(AExt: string; var ARecArr: TImportRecArr): Integer;
  var
    I: Integer;
    VConfig: IInterface;
    VImporter: IVectorItemTreeImporter;
  begin
    Result := -1;
    if SameText(AExt, '.jpeg') then begin
      AExt := '.jpg';
    end;
    for I := 0 to Length(ARecArr) - 1 do begin
      if SameText(AExt, ARecArr[I].FExt) then begin
        Result := I;
        Exit;
      end;
    end;
    VImporter := AImporterList.GetImporterByExt(AExt);
    if Assigned(VImporter) then begin
      if SameText(AExt, '.jpg') then begin
        VConfig := CreateJpegImportConfig(AMarkSystem, AAppearanceOfMarkFactory);
      end else begin
        VConfig := CreateCommonImportConfig(AMarkSystem);
      end;
      if Assigned(VConfig) then begin
        I := Length(ARecArr);
        SetLength(ARecArr, I + 1);
        ARecArr[I].FExt := AExt;
        ARecArr[I].FImporter := VImporter;
        ARecArr[I].FConfig := VConfig;
        Result := I;
      end;
    end;
  end;

var
  I, J: Integer;
  VFileName: string;
  VTree: IVectorItemTree;
  VImportConfig: IImportConfig;
  VRecArr: TImportRecArr;
  VNotifier: INotifierOperation;
begin
  Result := nil;
  SetLength(VRecArr, 0);
  VNotifier := TNotifierOperationFake.Create;
  for I := 0 to AFiles.Count - 1 do begin
    VFileName := AFiles.Items[I];
    if FileExists(VFileName) then begin
      J := _RecIndexByFileExt(ExtractFileExt(VFileName), VRecArr);
      if J < 0 then begin
        Break;
      end;
      VTree :=
        VRecArr[J].FImporter.ProcessImport(
          VNotifier.CurrentOperation,
          VNotifier,
          VFileName,
          VRecArr[J].FConfig
        );
      if Assigned(VTree) then begin
        if Supports(VRecArr[J].FConfig, IImportConfig, VImportConfig) then begin
          Result := AMarkSystem.ImportItemsTree(VTree, VImportConfig);
        end else begin
          //
        end;
      end;
    end else begin
      //
    end;
  end;
end;

procedure ProcessOpenFiles(
  const AFiles: IStringListStatic;
  const AMapGoto: IMapViewGoto;
  const ARegionProcess: IRegionProcessFromFile;
  const AShowImportDlg: Boolean = False;
  const AMarkDBGUIHelper: TMarkDBGUIHelper = nil;
  const AMarkSystem: IMarkSystem = nil;
  const AImporterList: IVectorItemTreeImporterListChangeable = nil;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory = nil
);
var
  VFileName: string;
  VList: IInterfaceListStatic;
  VLastMark: IVectorDataItem;
  VPolygon: IGeometryLonLatPolygon;
begin
  if Assigned(AFiles) and (AFiles.Count > 0) then begin
    if AFiles.Count = 1 then begin
      VFileName := AFiles.Items[0];
      if LowerCase(ExtractFileExt(VFileName)) = '.sls' then begin
        ARegionProcess.StartSlsFromFile(VFileName);
        Exit;
      end else if LowerCase(ExtractFileExt(VFileName)) = '.hlg' then begin
        ARegionProcess.LoadSelFromFile(VFileName, VPolygon);
        if Assigned(VPolygon) then begin
          AMapGoto.FitRectToScreen(VPolygon.Bounds.Rect);
        end;
        Exit;
      end;
    end;
    if AShowImportDlg then begin
      if Assigned(AMarkDBGUIHelper) then begin
        VList := AMarkDBGUIHelper.ImportFilesModal(AFiles);
      end;
    end else begin
      if Assigned(AMarkSystem) and Assigned(AImporterList) and Assigned(AAppearanceOfMarkFactory) then begin
        VList := ImportFiles(AFiles, AImporterList.GetStatic, AMarkSystem, AAppearanceOfMarkFactory);
      end;
    end;
    if (VList <> nil) and (VList.Count > 0) then begin
      VLastMark := IVectorDataItem(VList[VList.Count - 1]);
      if Assigned(VLastMark) then begin
        AMapGoto.FitRectToScreen(VLastMark.Geometry.Bounds.Rect);
      end;
    end;
  end;
end;

// ====================== Copy-Paste from System.pas ===========================
{$IFNDEF UNICODE}
function _CharNext(lpsz: PChar): PChar; stdcall;
  external 'user32.dll' name 'CharNextA';
{$ELSE}
function _CharNext(lpsz: PChar): PChar; stdcall;
  external 'user32.dll' name 'CharNextW';
{$ENDIF}

function _GetParamStr(P: PChar; var Param: string): PChar;
var
  i, Len: Integer;
  Start, S, Q: PChar;
begin
  while True do
  begin
    while (P[0] <> #0) and (P[0] <= ' ') do
      P := _CharNext(P);
    if (P[0] = '"') and (P[1] = '"') then Inc(P, 2) else Break;
  end;
  Len := 0;
  Start := P;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      P := _CharNext(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Q := _CharNext(P);
        Inc(Len, Q - P);
        P := Q;
      end;
      if P[0] <> #0 then
        P := _CharNext(P);
    end
    else
    begin
      Q := _CharNext(P);
      Inc(Len, Q - P);
      P := Q;
    end;
  end;

  SetLength(Param, Len);

  P := Start;
  S := Pointer(Param);
  i := 0;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      P := _CharNext(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Q := _CharNext(P);
        while P < Q do
        begin
          S[i] := P^;
          Inc(P);
          Inc(i);
        end;
      end;
      if P[0] <> #0 then P := _CharNext(P);
    end
    else
    begin
      Q := _CharNext(P);
      while P < Q do
      begin
        S[i] := P^;
        Inc(P);
        Inc(i);
      end;
    end;
  end;

  Result := P;
end;
// =============================================================================

function GetArgsAsList(const AArgs: string): TStringList;
var
  P: PChar;
  VArg: string;
begin
  Result := TStringList.Create;
  P := PChar(AArgs);
  while True do begin
    P := _GetParamStr(P, VArg);
    if VArg <> '' then begin
      Result.Add(VArg);
    end else begin
      Break;
    end;
  end;
end;

end.
