{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
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

unit u_PathDetalizeProviderBase;

interface

uses
  t_GeoTypes,
  i_NotifierOperation,
  i_InetConfig,
  i_Downloader,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_PathDetalizeProvider,
  i_DoublePointsAggregator,
  u_BaseInterfacedObject;

type
  TPathDetalizeProviderBase = class(TBaseInterfacedObject, IPathDetalizeProvider)
  protected
    FBaseUrl: AnsiString;
    FDownloader: IDownloader;
    FInetConfig: IInetConfig;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  protected
    function ProcessSingleLine(
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer;
      const ASource: IGeometryLonLatSingleLine;
      const APointsAggregator: IDoublePointsAggregator;
      const ABuilder: IGeometryLonLatLineBuilder;
      out AErrorMessage: string
    ): Boolean; virtual; abstract;
    procedure OnBeforeGetRoute; virtual;
    procedure OnAfterGetRoute; virtual;
  private
    { IPathDetalizeProvider }
    function GetRoute(
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer;
      const ASource: IGeometryLonLatLine;
      out AComment: string;
      out AErrorMessage: string
    ): IGeometryLonLatLine;
  public
    constructor Create(
      const ABaseUrl: AnsiString;
      const ADownloader: IDownloader;
      const AInetConfig: IInetConfig;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
    );
  end;

implementation

uses
  SysUtils,
  u_DoublePointsAggregator,
  u_GeoFunc;

{ TPathDetalizeProviderBase }

constructor TPathDetalizeProviderBase.Create(
  const ABaseUrl: AnsiString;
  const ADownloader: IDownloader;
  const AInetConfig: IInetConfig;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
);
begin
  inherited Create;
  FBaseUrl := ABaseUrl;
  FDownloader := ADownloader;
  FInetConfig := AInetConfig;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
end;

function TPathDetalizeProviderBase.GetRoute(
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Integer;
  const ASource: IGeometryLonLatLine;
  out AComment: string;
  out AErrorMessage: string
): IGeometryLonLatLine;
var
  I: Integer;
  VIsLineProcessed: Boolean;
  VPointsAggregator: IDoublePointsAggregator;
  VSingleLine: IGeometryLonLatSingleLine;
  VMultiLine: IGeometryLonLatMultiLine;
  VBuilder: IGeometryLonLatLineBuilder;
begin
  Result := nil;
  AComment := '';
  AErrorMessage := '';

  OnBeforeGetRoute;
  try
    VIsLineProcessed := False;
    VPointsAggregator := TDoublePointsAggregator.Create;
    VBuilder := FVectorGeometryLonLatFactory.MakeLineBuilder;

    if Supports(ASource, IGeometryLonLatSingleLine, VSingleLine) then begin
      VIsLineProcessed :=
        ProcessSingleLine(
          ACancelNotifier,
          AOperationID,
          VSingleLine,
          VPointsAggregator,
          VBuilder,
          AErrorMessage
        );
    end else if Supports(ASource, IGeometryLonLatMultiLine, VMultiLine) then begin
      for I := 0 to VMultiLine.Count - 1 do begin
        if
          ProcessSingleLine(
            ACancelNotifier,
            AOperationID,
            VMultiLine.Item[I],
            VPointsAggregator,
            VBuilder,
            AErrorMessage
          )
        then begin
          VIsLineProcessed := True;
          VPointsAggregator.Add(CEmptyDoublePoint);
        end else begin
          Break;
        end;
      end;
    end else begin
      Assert(False);
    end;

    if VIsLineProcessed then begin
      Result := VBuilder.MakeStaticAndClear;
    end;
  finally
    OnAfterGetRoute;
  end;
end;

procedure TPathDetalizeProviderBase.OnBeforeGetRoute;
begin
  // empty
end;

procedure TPathDetalizeProviderBase.OnAfterGetRoute;
begin
  // empty
end;

end.

