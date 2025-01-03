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

unit u_GeoCoderBasic;

interface

uses
  Classes,
  SysUtils,
  i_NotifierOperation,
  i_InterfaceListSimple,
  i_InetConfig,
  i_NotifierTime,
  i_DownloaderFactory,
  i_DownloadRequest,
  i_DownloadResult,
  i_Downloader,
  i_GeoCoder,
  i_VectorItemSubset,
  i_VectorItemSubsetBuilder,
  i_LocalCoordConverter,
  u_BaseInterfacedObject;

type
  TGeoCoderBasic = class(TBaseInterfacedObject, IGeoCoder)
  private
    FDownloader: IDownloader;
    FInetSettings: IInetConfig;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FPlacemarkFactory: IGeoCodePlacemarkFactory;
    function BuildSortedSubset(
      const AList: IInterfaceListSimple;
      const ALocalConverter: ILocalCoordConverter
    ): IVectorItemSubset;
  protected
    FApiKey: string;

    function PrepareRequestByURL(const AUrl: AnsiString): IDownloadRequest;
    function URLEncode(const S: AnsiString): AnsiString;

    function PrepareRequest(
      const ASearch: string;
      const ALocalConverter: ILocalCoordConverter
    ): IDownloadRequest; virtual; abstract;

    function ParseResultToPlacemarksList(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AResult: IDownloadResultOk;
      const ASearch: string;
      const ALocalConverter: ILocalCoordConverter
    ): IInterfaceListSimple; virtual; abstract;

    property PlacemarkFactory: IGeoCodePlacemarkFactory read FPlacemarkFactory;
    property Downloader: IDownloader read FDownloader;
    property InetSettings: IInetConfig read FInetSettings;
  private
    { IGeoCoder }
    function GetLocations(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const ASearch: string;
      const ALocalConverter: ILocalCoordConverter
    ): IGeoCodeResult;
  public
    constructor Create(
      const AInetSettings: IInetConfig;
      const AGCNotifier: INotifierTime;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const APlacemarkFactory: IGeoCodePlacemarkFactory;
      const ADownloaderFactory: IDownloaderFactory;
      const AApiKey: string = ''
    );
  end;

  EProxyAuthError = class(Exception);
  EAnswerParseError = class(Exception);

implementation

uses
  gnugettext,
  i_VectorDataItemSimple,
  i_Datum,
  u_NetworkStrFunc,
  u_DownloaderHttpWithTTL,
  u_DownloadRequest,
  u_SortFunc,
  u_GeoCodeResult;

{ TGeoCoderBasic }

constructor TGeoCoderBasic.Create(
  const AInetSettings: IInetConfig;
  const AGCNotifier: INotifierTime;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const APlacemarkFactory: IGeoCodePlacemarkFactory;
  const ADownloaderFactory: IDownloaderFactory;
  const AApiKey: string
);
begin
  inherited Create;
  FInetSettings := AInetSettings;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FPlacemarkFactory := APlacemarkFactory;
  FDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, ADownloaderFactory);
  FApiKey := AApiKey;
end;

function TGeoCoderBasic.BuildSortedSubset(
  const AList: IInterfaceListSimple;
  const ALocalConverter: ILocalCoordConverter
): IVectorItemSubset;
var
  I: Integer;
  VMark: IVectorDataItem;
  VDatum: IDatum;
  VDistArr: array of Double;
  VSubsetBuilder: IVectorItemSubsetBuilder;
begin
  Result := nil;
  if Assigned(AList) then begin
    if AList.Count > 1 then begin
      VDatum := ALocalConverter.Projection.ProjectionType.Datum;
      SetLength(VDistArr, AList.Count);
      for I := 0 to AList.GetCount - 1 do begin
        VMark := IVectorDataItem(AList.Items[I]);
        VDistArr[I] := VDatum.CalcDist(ALocalConverter.GetCenterLonLat, VMark.Geometry.Bounds.CalcRectCenter);
      end;
      SortInterfaceListByDoubleMeasure(AList, VDistArr);
    end;
    VSubsetBuilder := FVectorItemSubsetBuilderFactory.Build;
    for I := 0 to AList.GetCount - 1 do begin
      VMark := IVectorDataItem(AList.Items[I]);
      VSubsetBuilder.Add(VMark);
    end;
    Result := VSubsetBuilder.MakeStaticAndClear;
  end;
end;

function TGeoCoderBasic.GetLocations(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const ASearch: string;
  const ALocalConverter: ILocalCoordConverter
): IGeoCodeResult;
var
  VList: IInterfaceListSimple;
  VResultCode: Integer;
  VMessage: string;
  VRequest: IDownloadRequest;
  VResult: IDownloadResult;
  VResultOk: IDownloadResultOk;
  VResultError: IDownloadResultError;
  VResultNotFound: IDownloadResultDataNotExists;
  VSubset: IVectorItemSubset;
begin
  Result := nil;

  VResultCode := 200;
  VMessage := '';

  if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
    Exit;
  end;

  VSubset := nil;
  try
    if ASearch <> '' then begin
      VList := nil;
      VRequest := PrepareRequest(ASearch, ALocalConverter);
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Exit;
      end;

      if VRequest <> nil then begin
        VResult := FDownloader.DoRequest(VRequest, ACancelNotifier, AOperationID);
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          Exit;
        end;

        if Supports(VResult, IDownloadResultOk, VResultOk) then begin
          VList :=
            ParseResultToPlacemarksList(
              ACancelNotifier,
              AOperationID,
              VResultOk,
              ASearch,
              ALocalConverter
            );
        end else
        if Supports(VResult, IDownloadResultError, VResultError) then begin
          if VResultError.IsServerExists then begin
            VResultCode := CGeoCodeDownloadErrorResultCode;
          end else begin
            VResultCode := CGeoCodeNoInternetConnectionResultCode;
          end;
          VMessage := VResultError.ErrorText;
        end else
        if Supports(VResult, IDownloadResultDataNotExists, VResultNotFound) then begin
          VResultCode := CGeoCodeNotFoundResultCode;
          VMessage := VResultNotFound.ReasonText;
        end else begin
          VResultCode := CGeoCodeDownloadErrorResultCode;
          VMessage := _('Unknown HTTP request error!');
        end;
      end else begin
        // local (offline) search
        VList :=
          ParseResultToPlacemarksList(
            ACancelNotifier,
            AOperationID,
            nil,
            ASearch,
            ALocalConverter
          );
      end;
      VSubset := BuildSortedSubset(VList, ALocalConverter);
    end;
  except
    on E: Exception do begin
      VResultCode := CGeoCodeExceptionResultCode;
      VMessage := E.Message;
    end;
  end;

  if (VResultCode = 200) and ((VSubset = nil) or (VSubset.Count = 0)) then begin
    VResultCode := CGeoCodeNotFoundResultCode;
    VMessage := _('Not Found');
  end;

  Result := TGeoCodeResult.Create(ASearch, VResultCode, VMessage, VSubset);
end;

function TGeoCoderBasic.PrepareRequestByURL(const AUrl: AnsiString): IDownloadRequest;
begin
  Result := TDownloadRequest.Create(AUrl, '', FInetSettings.GetStatic);
end;

function TGeoCoderBasic.URLEncode(const S: AnsiString): AnsiString;
begin
  Result := u_NetworkStrFunc.UrlEncode(S);
end;

end.
