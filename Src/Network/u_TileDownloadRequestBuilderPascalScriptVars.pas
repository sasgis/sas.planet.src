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

unit u_TileDownloadRequestBuilderPascalScriptVars;

interface

uses
  Types,
  uPSRuntime,
  uPSCompiler,
  i_CoordConverterSimple,
  i_TileRequest,
  i_LastResponseInfo,
  i_ProjConverter,
  i_SimpleHttpDownloader;

type
  TRequestBuilderVars = record
  // private
    FpResultUrl: PPSVariantAString;
    FpPostData: PPSVariantAString;
    FpGetURLBase: PPSVariantAString;
    FpRequestHead: PPSVariantAString;
    FpResponseHead: PPSVariantAString;
    FpScriptBuffer: PPSVariantAString;
    FpVersion: PPSVariantAString;
    FpLang: PPSVariantAString;
    FpGetX: PPSVariantS32;
    FpGetY: PPSVariantS32;
    FpGetZ: PPSVariantS32;
    FpGetLlon: PPSVariantDouble;
    FpGetTLat: PPSVariantDouble;
    FpGetBLat: PPSVariantDouble;
    FpGetRLon: PPSVariantDouble;
    FpGetLMetr: PPSVariantDouble;
    FpGetRMetr: PPSVariantDouble;
    FpGetTMetr: PPSVariantDouble;
    FpGetBMetr: PPSVariantDouble;
    FpConverter: PPSVariantInterface;
    FpDownloader: PPSVariantInterface;
    FpDefProjConverter: PPSVariantInterface;
    FpProjFactory: PPSVariantInterface;

  // public

    function ResultUrl: AnsiString; inline;
    function RequestHead: AnsiString; inline;
    function PostData: AnsiString; inline;
    function ScriptBuffer: AnsiString; inline;

    procedure ExecTimeInit(const APSExec: TPSExec); inline;

    procedure ExecTimeSet(
      const AUrlBase: AnsiString;
      const ARequestHeader: AnsiString;
      const AScriptBuffer: AnsiString;
      const ALang: AnsiString;
      const ACoordConverter: ICoordConverterSimple;
      const ASimpleDownloader: ISimpleHttpDownloader;
      const ALastResponseInfo: ILastResponseInfo;
      const ASource: ITileRequest;
      const ADefProjConverter: IProjConverter;
      const AProjFactory: IProjConverterFactory
    ); inline;
  end;

procedure CompileTimeReg_RequestBuilderVars(const APSComp: TPSPascalCompiler);

implementation

uses
  t_GeoTypes;

procedure CompileTimeReg_RequestBuilderVars(const APSComp: TPSPascalCompiler);
var
  VType: TPSType;
begin
  VType := APSComp.FindType('ISimpleHttpDownloader');
  APSComp.AddUsedVariable('Downloader', VType);

  VType := APSComp.FindType('IProjConverter');
  APSComp.AddUsedVariable('DefProjConverter', VType);

  VType := APSComp.FindType('IProjConverterFactory');
  APSComp.AddUsedVariable('ProjFactory', VType);

  VType := APSComp.FindType('ICoordConverter');
  APSComp.AddUsedVariable('Converter', VType);

  VType := APSComp.FindType('AnsiString');
  APSComp.AddUsedVariable('ResultURL', VType);
  APSComp.AddUsedVariable('PostData', VType);
  APSComp.AddUsedVariable('GetURLBase', VType);
  APSComp.AddUsedVariable('RequestHead', VType);
  APSComp.AddUsedVariable('ResponseHead', VType);
  APSComp.AddUsedVariable('ScriptBuffer', VType);
  APSComp.AddUsedVariable('Version', VType);
  APSComp.AddUsedVariable('Lang', VType);

  VType := APSComp.FindType('Integer');
  APSComp.AddUsedVariable('GetX', VType);
  APSComp.AddUsedVariable('GetY', VType);
  APSComp.AddUsedVariable('GetZ', VType);

  VType := APSComp.FindType('Double');
  APSComp.AddUsedVariable('GetLlon', VType);
  APSComp.AddUsedVariable('GetTLat', VType);
  APSComp.AddUsedVariable('GetBLat', VType);
  APSComp.AddUsedVariable('GetRLon', VType);
  APSComp.AddUsedVariable('GetLMetr', VType);
  APSComp.AddUsedVariable('GetRMetr', VType);
  APSComp.AddUsedVariable('GetTMetr', VType);
  APSComp.AddUsedVariable('GetBMetr', VType);
end;

procedure TRequestBuilderVars.ExecTimeInit(const APSExec: TPSExec);
begin
  FpResultUrl := PPSVariantAString(APSExec.GetVar2('ResultURL'));
  FpPostData := PPSVariantAString(APSExec.GetVar2('PostData'));
  FpGetURLBase := PPSVariantAString(APSExec.GetVar2('GetURLBase'));
  FpGetURLBase.Data := '';
  FpRequestHead := PPSVariantAString(APSExec.GetVar2('RequestHead'));
  FpRequestHead.Data := '';
  FpResponseHead := PPSVariantAString(APSExec.GetVar2('ResponseHead'));
  FpResponseHead.Data := '';
  FpVersion := PPSVariantAString(APSExec.GetVar2('Version'));
  FpVersion.Data := '';
  FpLang := PPSVariantAString(APSExec.GetVar2('Lang'));
  FpLang.Data := '';
  FpScriptBuffer := PPSVariantAString(APSExec.GetVar2('ScriptBuffer'));
  FpGetX := PPSVariantS32(APSExec.GetVar2('GetX'));
  FpGetY := PPSVariantS32(APSExec.GetVar2('GetY'));
  FpGetZ := PPSVariantS32(APSExec.GetVar2('GetZ'));
  FpGetLlon := PPSVariantDouble(APSExec.GetVar2('GetLlon'));
  FpGetTLat := PPSVariantDouble(APSExec.GetVar2('GetTLat'));
  FpGetBLat := PPSVariantDouble(APSExec.GetVar2('GetBLat'));
  FpGetRLon := PPSVariantDouble(APSExec.GetVar2('GetRLon'));
  FpGetLMetr := PPSVariantDouble(APSExec.GetVar2('GetLmetr'));
  FpGetTMetr := PPSVariantDouble(APSExec.GetVar2('GetTmetr'));
  FpGetBMetr := PPSVariantDouble(APSExec.GetVar2('GetBmetr'));
  FpGetRMetr := PPSVariantDouble(APSExec.GetVar2('GetRmetr'));
  FpConverter := PPSVariantInterface(APSExec.GetVar2('Converter'));
  FpDownloader := PPSVariantInterface(APSExec.GetVar2('Downloader'));
  FpDefProjConverter := PPSVariantInterface(APSExec.GetVar2('DefProjConverter'));
  FpProjFactory := PPSVariantInterface(APSExec.GetVar2('ProjFactory'));
end;

procedure TRequestBuilderVars.ExecTimeSet(
  const AUrlBase: AnsiString;
  const ARequestHeader: AnsiString;
  const AScriptBuffer: AnsiString;
  const ALang: AnsiString;
  const ACoordConverter: ICoordConverterSimple;
  const ASimpleDownloader: ISimpleHttpDownloader;
  const ALastResponseInfo: ILastResponseInfo;
  const ASource: ITileRequest;
  const ADefProjConverter: IProjConverter;
  const AProjFactory: IProjConverterFactory
);
var
  XY: TPoint;
  VLonLat: TDoublePoint;
  VTile: TPoint;
  VZoom: Byte;
begin
  VTile := ASource.Tile;
  VZoom := ASource.Zoom;

  FpGetX.Data := VTile.X;
  FpGetY.Data := VTile.Y;
  FpGetZ.Data := VZoom + 1;

  VLonLat := ACoordConverter.Pos2LonLat(VTile, VZoom);
  FpGetLlon.Data := VLonLat.X;
  FpGetTLat.Data := VLonLat.Y;

  VLonLat := ACoordConverter.LonLat2Metr(VLonLat);
  FpGetLMetr.Data := VLonLat.X;
  FpGetTMetr.Data := VLonLat.Y;

  XY := VTile;
  Inc(XY.X);
  Inc(XY.Y);

  VLonLat := ACoordConverter.Pos2LonLat(XY, VZoom);
  FpGetRLon.Data := VLonLat.X;
  FpGetBLat.Data := VLonLat.Y;

  VLonLat := ACoordConverter.LonLat2Metr(VLonLat);
  FpGetRMetr.Data := VLonLat.X;
  FpGetBMetr.Data := VLonLat.Y;

  FpConverter.Data := ACoordConverter;

  FpResultUrl.Data := '';
  FpPostData.Data := '';

  FpGetURLBase.Data := AUrlBase;
  FpRequestHead.Data := ARequestHeader;

  if ALastResponseInfo <> nil then begin
    FpResponseHead.Data := ALastResponseInfo.ResponseHead;
  end else begin
    FpResponseHead.Data := '';
  end;

  FpScriptBuffer.Data := AScriptBuffer;

  if ASource.VersionInfo <> nil then begin
    FpVersion.Data := AnsiString(ASource.VersionInfo.UrlString);
  end else begin
    FpVersion.Data := '';
  end;

  FpLang.Data := ALang;

  FpDownloader.Data := ASimpleDownloader;
  FpDefProjConverter.Data := ADefProjConverter;
  FpProjFactory.Data := AProjFactory;
end;

function TRequestBuilderVars.ResultUrl: AnsiString;
begin
  Result := FpResultUrl.Data;
end;

function TRequestBuilderVars.RequestHead: AnsiString;
begin
  Result := FpRequestHead.Data;
end;

function TRequestBuilderVars.PostData: AnsiString;
begin
  Result := FpPostData.Data;
end;

function TRequestBuilderVars.ScriptBuffer: AnsiString;
begin
  Result := FpScriptBuffer.Data;
end;

end.
