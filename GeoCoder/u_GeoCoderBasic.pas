{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_GeoCoderBasic;

interface

uses
  SysUtils,
  i_NotifierOperation,
  i_InterfaceListSimple,
  i_InetConfig,
  i_NotifierTime,
  i_DownloadResultFactory,
  i_DownloadRequest,
  i_DownloadResult,
  i_Downloader,
  i_GeoCoder,
  i_LocalCoordConverter,
  u_BaseInterfacedObject;

type
  TGeoCoderBasic = class(TBaseInterfacedObject, IGeoCoder)
  private
    FDownloader: IDownloader;
    FInetSettings: IInetConfig;
    FPlacemarkFactory: IGeoCodePlacemarkFactory;
  protected
    property PlacemarkFactory: IGeoCodePlacemarkFactory read FPlacemarkFactory;
    property Downloader: IDownloader read FDownloader;
    property InetSettings: IInetConfig read FInetSettings;
    function PrepareRequestByURL(const AUrl: AnsiString): IDownloadRequest;
    function URLEncode(const S: AnsiString): AnsiString;
    function PrepareRequest(
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IDownloadRequest; virtual; abstract;
    function ParseResultToPlacemarksList(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AResult: IDownloadResultOk;
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IInterfaceListSimple; virtual; abstract;
    procedure SortIt(
      const AList:IInterfaceListSimple;
      const ALocalConverter: ILocalCoordConverter
    );
  private
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
      const APlacemarkFactory: IGeoCodePlacemarkFactory;
      const AResultFactory: IDownloadResultFactory
    );
  end;

  EProxyAuthError = class(Exception);
  EAnswerParseError = class(Exception);

implementation

uses
  u_InterfaceListSimple,
  u_DownloaderHttpWithTTL,
  u_DownloadRequest,
  u_SortFunc,
  u_GeoCodeResult;

{ TGeoCoderBasic }

constructor TGeoCoderBasic.Create(
  const AInetSettings: IInetConfig;
  const AGCNotifier: INotifierTime;
  const APlacemarkFactory: IGeoCodePlacemarkFactory;
  const AResultFactory: IDownloadResultFactory
);
begin
  inherited Create;
  FInetSettings := AInetSettings;
  FPlacemarkFactory := APlacemarkFactory;
  FDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
end;

procedure TGeoCoderBasic.SortIt(
  const AList:IInterfaceListSimple;
  const ALocalConverter: ILocalCoordConverter
  );
var
  i: integer;
  VMark: IGeoCodePlacemark;
  VDistArr: array of Double;
begin
  SetLength(VDistArr, AList.Count);
  for i := 0 to AList.GetCount-1 do begin
    VMark := IGeoCodePlacemark(AList.Items[i]);
    VDistArr[i] := ALocalConverter.GetGeoConverter.Datum.CalcDist(ALocalConverter.GetCenterLonLat, VMark.GetPoint);
  end;
  SortInterfaceListByDoubleMeasure(AList, VDistArr);
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
  VMessage: WideString;
  VRequest: IDownloadRequest;
  VResult: IDownloadResult;
  VResultOk: IDownloadResultOk;
  VResultError: IDownloadResultError;
begin
  VResultCode := 200;
  VMessage := '';
  VList := nil;
  Result := nil;
  if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
    Exit;
  end;

  try
    if not (ASearch = '') then begin
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
        end else if Supports(VResult, IDownloadResultError, VResultError) then begin
          VResultCode := 503;
          VMessage := VResultError.ErrorText;
        end else begin
          VResultCode := 417;
          VMessage := 'Unknown error';
        end;
      end else begin
        VList :=
          ParseResultToPlacemarksList(
            ACancelNotifier,
            AOperationID,
            nil,
            ASearch,
            ALocalConverter
          );
      end;
    end;
  except
    on E: Exception do begin
      VResultCode := 417;
      VMessage := E.Message;
    end;
  end;
  if VList = nil then begin
    VList := TInterfaceListSimple.Create;
  end;
  if VList.Count = 0 then begin
    VResultCode := 404;
    VMessage := 'Не найдено';
  end;
  Result := TGeoCodeResult.Create(ASearch, VResultCode, VMessage, VList.MakeStaticAndClear);
end;

function TGeoCoderBasic.PrepareRequestByURL(const AUrl: AnsiString): IDownloadRequest;
begin
  Result := TDownloadRequest.Create(AUrl, '', FInetSettings.GetStatic);
end;

function TGeoCoderBasic.URLEncode(const S: AnsiString): AnsiString;
  function DigitToHex(Digit: Integer): AnsiChar;
  begin
    case Digit of
      0..9:
      begin
        Result := AnsiChar(Digit + Ord('0'));
      end;
      10..15:
      begin
        Result := AnsiChar(Digit - 10 + Ord('A'));
      end;
    else begin
      Result := '0';
    end;
    end;
  end; // DigitToHex
var
  i, idx, len: Integer;
begin
  len := 0;
  for i := 1 to Length(S) do begin
    if ((S[i] >= '0') and (S[i] <= '9')) or
      ((S[i] >= 'A') and (S[i] <= 'Z')) or
      ((S[i] >= 'a') and (S[i] <= 'z')) or (S[i] = ' ') or
      (S[i] = '_') or (S[i] = '*') or (S[i] = '-') or (S[i] = '.') then begin
      len := len + 1;
    end else begin
      len := len + 3;
    end;
  end;
  SetLength(Result, len);
  idx := 1;
  for i := 1 to Length(S) do begin
    if S[i] = ' ' then begin
      Result[idx] := '+';
      idx := idx + 1;
    end else if ((S[i] >= '0') and (S[i] <= '9')) or
      ((S[i] >= 'A') and (S[i] <= 'Z')) or
      ((S[i] >= 'a') and (S[i] <= 'z')) or
      (S[i] = '_') or (S[i] = '*') or (S[i] = '-') or (S[i] = '.') then begin
      Result[idx] := S[i];
      idx := idx + 1;
    end else begin
      Result[idx] := '%';
      Result[idx + 1] := DigitToHex(Ord(S[i]) div 16);
      Result[idx + 2] := DigitToHex(Ord(S[i]) mod 16);
      idx := idx + 3;
    end;
  end;
end;

end.
