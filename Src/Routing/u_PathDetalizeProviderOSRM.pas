{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2020, SAS.Planet development team.                      *}
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

unit u_PathDetalizeProviderOSRM;

interface

uses
  Math,
  SysUtils,
  t_GeoTypes,
  i_NotifierOperation,
  i_InetConfig,
  i_Downloader,
  i_DownloadRequest,
  i_DownloadResult,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_DoublePointsAggregator,
  u_PathDetalizeProviderBase;

type
  EPathDetalizeProviderOSRM = class(Exception);

  TPathDetalizeProviderOSRM = class(TPathDetalizeProviderBase)
  private
    function BuildRequest(
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer;
      const ALonLatLine: IGeometryLonLatSingleLine
    ): IDownloadRequest;
    procedure ParseResponse(
      const AResponse: IDownloadResultOk;
      const APointsAggregator: IDoublePointsAggregator
    );
  protected
    function ProcessSingleLine(
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer;
      const ALonLatLine: IGeometryLonLatSingleLine;
      const APointsAggregator: IDoublePointsAggregator;
      const ABuilder: IGeometryLonLatLineBuilder
    ): Boolean; override;
  end;

implementation

uses
  superobject,
  u_DownloadRequest,
  u_GeoFunc,
  u_GeoToStrFunc,
  u_ResStrings;

{ TPathDetalizeProviderOSRM }

function TPathDetalizeProviderOSRM.BuildRequest(
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Integer;
  const ALonLatLine: IGeometryLonLatSingleLine
): IDownloadRequest;
const
  CSep: array[Boolean] of AnsiString = ('', ';');
var
  I: Integer;
  VUrl: AnsiString;
  VPoints: PDoublePointArray;
begin
  if ALonLatLine.Count < 2 then begin
    raise EPathDetalizeProviderOSRM.Create('Expected at least 2 points!');
  end;;

  VUrl := '';
  VPoints := ALonLatLine.Points;

  for I := 0 to ALonLatLine.Count - 1 do begin
    Assert( not PointIsEmpty(VPoints[I]) );
    VUrl := VUrl + CSep[I>0] + RoundExAnsi(VPoints[I].X, 6) + ',' + RoundExAnsi(VPoints[I].Y, 6);
  end;

  Result := TDownloadRequest.Create(
    FBaseUrl + VUrl + '?geometries=geojson&overview=full',
    '',
    FInetConfig.GetStatic
  );
end;

procedure TPathDetalizeProviderOSRM.ParseResponse(
  const AResponse: IDownloadResultOk;
  const APointsAggregator: IDoublePointsAggregator
);

  function _GetPoint(const ACoord: TSuperArray; out APoint: TDoublePoint): Boolean;
  var
    VFormatSettings: TFormatSettings;
  begin
    if not Assigned(ACoord) or (ACoord.Length <> 2) then begin
      Result := False;
      Exit;
    end;
    VFormatSettings.DecimalSeparator := '.';
    try
      APoint.X := StrToFloat(ACoord.S[0], VFormatSettings);
      APoint.Y := StrToFloat(ACoord.S[1], VFormatSettings);
      Result := True;
    except
      raise EPathDetalizeProviderOSRM.CreateFmt(
        SAS_ERR_CoordParseError, [ACoord.S[0], ACoord.S[1]]
      );
    end;
  end;

var
  I: Integer;
  VStr: AnsiString;
  VCode: string;
  VJsonObject: ISuperObject;
  VRoutes, VCoordinates: TSuperArray;
  VPoint: TDoublePoint;
begin
  SetLength(VStr, AResponse.Data.Size);
  Move(AResponse.Data.Buffer^, VStr[1], AResponse.Data.Size);

  VJsonObject := SO(Utf8ToAnsi(VStr));

  if not Assigned(VJsonObject) then begin
    raise EPathDetalizeProviderOSRM.Create('Can''t parse response as a Json object!');
  end;

  VCode := VJsonObject.S['code'];
  if LowerCase(VCode) <> 'ok' then begin
    raise EPathDetalizeProviderOSRM.Create('Unexpected "code" value: ' + VCode);
  end;

  VRoutes := VJsonObject.A['routes'];
  if not Assigned(VRoutes) or (VRoutes.Length < 1) then begin
    Exit;
  end;

  VCoordinates := VRoutes.O[0].A['geometry.coordinates'];
  if Assigned(VCoordinates) then begin
    for I := 0 to VCoordinates.Length - 1 do begin
      if _GetPoint(VCoordinates.O[I].AsArray, VPoint) then begin
        APointsAggregator.Add(VPoint);
      end else begin
        raise EPathDetalizeProviderOSRM.Create(
          'Invalid point coordinates: ' + VCoordinates.S[I]
        );
      end;
    end;
  end;
end;

function TPathDetalizeProviderOSRM.ProcessSingleLine(
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Integer;
  const ALonLatLine: IGeometryLonLatSingleLine;
  const APointsAggregator: IDoublePointsAggregator;
  const ABuilder: IGeometryLonLatLineBuilder
): Boolean;
var
  VRequest: IDownloadRequest;
  VResult: IDownloadResult;
  VResultOk: IDownloadResultOk;
begin
  Result := True;

  if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
    Result := False;
    Exit;
  end;

  VRequest := BuildRequest(ACancelNotifier, AOperationID, ALonLatLine);
  VResult := FDownloader.DoRequest(VRequest, ACancelNotifier, AOperationID);

  if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
    Result := False;
    Exit;
  end;

  if not Supports(VResult, IDownloadResultOk, VResultOk) then begin
    Result := False;
    Exit;
  end;

  ParseResponse(VResultOk, APointsAggregator);

  if APointsAggregator.Count > 0 then begin
    ABuilder.AddLine(APointsAggregator.MakeStaticAndClear);
  end;
end;

end.
