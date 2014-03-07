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

unit u_AvailPicsAbstract;

interface

uses
  SysUtils,
  Classes,
  i_InetConfig,
  i_DownloadResult,
  i_DownloadRequest,
  i_LocalCoordConverter,
  i_MapSvcScanStorage,
  t_GeoTypes;

type
  // to add items to form
  TAddAvailImageItemProc = function (Sender: TObject;
                                     const ADate: String;
                                     const AId: String;
                                     const AExisting: Boolean;
                                     const AFetched: TDateTime;
                                     var AParams: TStrings): Boolean of object;

  PAvailPicsTileInfo = ^TAvailPicsTileInfo;
  TAvailPicsTileInfo = record
    // common
    AddImageProc: TAddAvailImageItemProc;
    LonLat: TDoublePoint;
    Zoom: Byte;
    LowResToo: Boolean;
    // for DG
    mpp: Extended;
    hi,wi: Integer;
    // for ESRI
    TileRect: TDoubleRect;
  end;

  TAvailPicsAbstract = class(TObject)
  protected
    FTileInfoPtr: PAvailPicsTileInfo;
    FLocalConverter: ILocalCoordConverter;
    FMapSvcScanStorage: IMapSvcScanStorage;
    FBaseStorageName: String;
  public
    constructor Create(
      const ATileInfoPtr: PAvailPicsTileInfo;
      const AMapSvcScanStorage: IMapSvcScanStorage
    );
    destructor Destroy; override;

    procedure SetLocalConverter(const ALocalConverter: ILocalCoordConverter);

    function ContentType: String; virtual; abstract;

    // parse response from server, returns number of added items
    function ParseResponse(const AResultOk: IDownloadResultOk): Integer; virtual; abstract;

    // Request or PostRequest
    function GetRequest(const AInetConfig: IInetConfig): IDownloadRequest; virtual; abstract;

    // check item exists, if not - add it to storage
    function ItemExists(
      const AServiceName: String;
      const AIdentifier: WideString;
      const AFetchedDate: PDateTime
    ): Boolean;
  end;

  TAvailPicsByKey = class(TAvailPicsAbstract)
  protected
    // TODO: obtain key online or get it from zmp
    FDefaultKey: String;
  end;

function CheckHiResResolution(const AStrResolution: String): Boolean;

function GetDateForCaption(const ADate: String): String;
function GetDateCaptionFromParams(const ASLParams: TStrings): String;

implementation

uses
  u_GeoToStrFunc;

function CheckHiResResolution(const AStrResolution: String): Boolean;
var VRes: String;
begin
  if (0=Length(AStrResolution)) then begin
    // if no resolution info - show image
    Result:=TRUE;
  end else begin
    // try co check landsat
    VRes:=AStrResolution;
    try
      if (DecimalSeparator<>'.') and (System.Pos(DecimalSeparator,VRes)>0) then
        VRes:=StringReplace(VRes, DecimalSeparator, '.', []);
      // do not show "landsat" with 15 and 25 meters
      Result:=(StrPointToFloat(VRes)<=14);
    except
      Result:=TRUE;
    end;
  end;
end;

function GetDateForCaption(const ADate: String): String;
begin
  Result := System.Copy(ADate, 1, 10);
  if (0 < Length(Result)) then
  try
    Result[5] := DateSeparator;
    Result[8] := DateSeparator;
  except
  end;
end;

function GetDateCaptionFromParams(const ASLParams: TStrings): String;
var
  VPrevDate: String;
begin
  // get single date at acquisitionDate
  // or 2 dates from earliestAcquisitionDate to latestAcquisitionDate
  Result := GetDateForCaption(ASLParams.Values['latestAcquisitionDate']);
  if (0<Length(Result)) then begin
    VPrevDate := GetDateForCaption(ASLParams.Values['earliestAcquisitionDate']);
    if (Result<>VPrevDate) then
      Result := VPrevDate + ' - ' + Result;
  end else begin
    // single date
    VPrevDate := ASLParams.Values['acquisitionDate'];
    if (0 = Length(VPrevDate)) then begin
      VPrevDate := ASLParams.Values['formattedDate'];
    end;
    Result := GetDateForCaption(VPrevDate);
  end;
end;

{ TAvailPicsAbstract }

constructor TAvailPicsAbstract.Create(
  const ATileInfoPtr: PAvailPicsTileInfo;
  const AMapSvcScanStorage: IMapSvcScanStorage
);
begin
  inherited Create;
  FMapSvcScanStorage := AMapSvcScanStorage;
  FTileInfoPtr := ATileInfoPtr;
  FLocalConverter := nil;
  FBaseStorageName := Classname;
  System.Delete(FBaseStorageName, 1, 10);
end;

destructor TAvailPicsAbstract.Destroy;
begin
  FLocalConverter:=nil;
  inherited;
end;

function TAvailPicsAbstract.ItemExists(
  const AServiceName: String;
  const AIdentifier: WideString;
  const AFetchedDate: PDateTime
): Boolean;
begin
  // check existing
  Result := FMapSvcScanStorage.ItemExists(AServiceName, AIdentifier, AFetchedDate);
  if Result then
    Exit;
  // item not found - register by current date
  AFetchedDate^ := Now;
  FMapSvcScanStorage.AddItem(AServiceName, AIdentifier, AFetchedDate^);
end;

procedure TAvailPicsAbstract.SetLocalConverter(const ALocalConverter: ILocalCoordConverter);
begin
  FLocalConverter := ALocalConverter;
end;

end.
