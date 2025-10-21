{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_TileStorageImporter;

interface

uses
  Windows,
  Dialogs,
  i_ActiveMapsConfig,
  i_MapTypeSet,
  i_MapViewGoto,
  i_StringListStatic,
  i_TileStorageImporter,
  i_TileStorageImporterList;

type
  TTileStorageImporter = class
  private
    FImportersList: ITileStorageImporterListStatic;
    FAllMapsSet: IMapTypeSet;
    FMainMapConfig: IActiveMapConfig;
    FMainLayersConfig: IActiveLayersConfig;

    FOpenDialog: TOpenDialog;

    procedure InitOpenDialog;
  public
    function OpenFileDialogExecute(
      const AParentWnd: HWND
    ): IStringListStatic;

    function ProcessFile(
      const AFileName: string;
      const AShowImportDlg: Boolean;
      const AMapGoto: IMapViewGoto
    ): Boolean;
  public
    constructor Create(
      const AImportersList: ITileStorageImporterListChangeable;
      const AAllMapsSet: IMapTypeSet;
      const AMainMapConfig: IActiveMapConfig;
      const AMainLayersConfig: IActiveLayersConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  SysUtils,
  Math,
  gnugettext,
  t_GeoTypes,
  i_MapType,
  u_GeoFunc,
  u_StringListStatic;

{ TTileStorageImporter }

constructor TTileStorageImporter.Create(
  const AImportersList: ITileStorageImporterListChangeable;
  const AAllMapsSet: IMapTypeSet;
  const AMainMapConfig: IActiveMapConfig;
  const AMainLayersConfig: IActiveLayersConfig
);
begin
  inherited Create;

  FImportersList := AImportersList.GetStatic;
  FAllMapsSet := AAllMapsSet;
  FMainMapConfig := AMainMapConfig;
  FMainLayersConfig := AMainLayersConfig;

  FOpenDialog := TOpenDialog.Create(nil);
  InitOpenDialog;
end;

destructor TTileStorageImporter.Destroy;
begin
  FreeAndNil(FOpenDialog);
  inherited Destroy;
end;

procedure TTileStorageImporter.InitOpenDialog;

  function _Cleanup(const AStr: string): string;
  begin
    Result := StringReplace(AStr, ';', ' ', [rfReplaceAll]);
  end;

  function _GetFilterStr: string;
  var
    I: Integer;
    VExtStr: string;
    VFilterStr: string;
    VAllFormats: string;
    VItem: ITileStorageImporterListItem;
  begin
    VFilterStr := '';
    VAllFormats := '';
    for I := 0 to FImportersList.Count - 1 do begin
      VItem := FImportersList.Items[I];
      VExtStr := '*.' + string.Join(';*.', VItem.SupportedExt);
      VFilterStr := VFilterStr + '|' + VItem.Name + ' (' + _Cleanup(VExtStr) + ')|' + VExtStr;
      if I > 0 then begin
        VAllFormats := VAllFormats + ';';
      end;
      VAllFormats := VAllFormats + VExtStr;
    end;
    Result := _('All supported formats') + ' (' + _Cleanup(VAllFormats) + ')|' + VAllFormats + VFilterStr;
  end;

begin
  FOpenDialog.Name := 'dlgOpen' + Self.ClassName;
  FOpenDialog.Options := [ofEnableSizing];

  FOpenDialog.Filter := _GetFilterStr;
  FOpenDialog.FilterIndex := 0;
end;

function TTileStorageImporter.OpenFileDialogExecute(const AParentWnd: HWND): IStringListStatic;
var
  VStrings: TStrings;
begin
  Result := nil;
  if FOpenDialog.Execute(AParentWnd) then begin
    VStrings := FOpenDialog.Files;
    if Assigned(VStrings) and (VStrings.Count > 0) then begin
      Result := TStringListStatic.CreateByStrings(VStrings);
    end;
  end;
end;

function TTileStorageImporter.ProcessFile(
  const AFileName: string;
  const AShowImportDlg: Boolean;
  const AMapGoto: IMapViewGoto
): Boolean;
var
  VMapType: IMapType;
  VImporter: ITileStorageImporter;
  VImportResult: TTileStorageImportResult;
begin
  Result := False;

  if not Assigned(FImportersList) then begin
    Exit;
  end;

  VImporter := FImportersList.GetImporterByExt(ExtractFileExt(AFileName));

  if not Assigned(VImporter) then begin
    Exit;
  end;

  VImportResult := VImporter.ProcessFile(AFileName, AShowImportDlg, FAllMapsSet);

  case VImportResult.Status of
    tsiOk: begin
      Result := True;

      VMapType := VImportResult.MapType;
      if not Assigned(VMapType) then begin
        Assert(False);
        Exit;
      end;

      if VMapType.Zmp.IsLayer then begin
        FMainLayersConfig.SelectLayerByGUID(VMapType.GUID);
      end else begin
        FMainMapConfig.MainMapGUID := VMapType.GUID;
      end;

      if not PointIsEmpty(VImportResult.GoToPoint) then begin
        AMapGoto.GotoPos(
          VImportResult.GoToPoint,
          VImportResult.MapType.ProjectionSet.Zooms[VImportResult.GoToZoom],
          False
        );
      end;
    end;

    tsiUnsupportedFormat: begin
      Result := False;
    end;

    tsiCanceled, tsiInternalError: begin
      Result := True;
    end;
  else
    Assert(False);
  end;
end;

end.
