{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2016, SAS.Planet development team.                      *}
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

unit u_UrlByCoordProvider;

interface

uses
  Types,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_DownloadRequest,
  i_UrlByCoordProvider,
  i_Projection,
  i_ProjectionSet,
  i_ProjectionSetFactory,
  u_BaseInterfacedObject;

type
  TUrlByCoordProviderBase = class(TBaseInterfacedObject, IUrlByCoordProvider)
  private
    FMainProjectionSet: IProjectionSet;
  protected
    function GetUrlByLonLat(
      const AProjection: IProjection;
      const ALonLat: TDoublePoint
    ): AnsiString; virtual; abstract;
  private
    function GetUrl(
      const AConverter: ILocalCoordConverter;
      const ALocalPoint: TPoint
    ): IDownloadRequest;
  public
    constructor Create(
      const AMainProjectionSet: IProjectionSet
    );
  end;

  TUrlByCoordProviderLonLatBase = class(TBaseInterfacedObject, IUrlByCoordProvider)
  protected
    function GetUrlByLonLat(
      const ALonLat: TDoublePoint
    ): AnsiString; virtual; abstract;
  private
    function GetUrl(
      const AConverter: ILocalCoordConverter;
      const ALocalPoint: TPoint
    ): IDownloadRequest;
  public
    constructor Create;
  end;

  TUrlByCoordProviderBing = class(TUrlByCoordProviderBase)
  protected
    function GetUrlByLonLat(
      const AProjection: IProjection;
      const ALonLat: TDoublePoint
    ): AnsiString; override;
  public
    constructor Create(
      const AProjectionSetFactory: IProjectionSetFactory
    );
  end;

  TUrlByCoordProviderKosmosnimki = class(TUrlByCoordProviderBase)
  protected
    function GetUrlByLonLat(
      const AProjection: IProjection;
      const ALonLat: TDoublePoint
    ): AnsiString; override;
  public
    constructor Create(
      const AProjectionSetFactory: IProjectionSetFactory
    );
  end;

  TUrlByCoordProviderYandex = class(TUrlByCoordProviderBase)
  protected
    function GetUrlByLonLat(
      const AProjection: IProjection;
      const ALonLat: TDoublePoint
    ): AnsiString; override;
  public
    constructor Create(
      const AProjectionSetFactory: IProjectionSetFactory
    );
  end;

  TUrlByCoordProviderGoogle = class(TUrlByCoordProviderBase)
  protected
    function GetUrlByLonLat(
      const AProjection: IProjection;
      const ALonLat: TDoublePoint
    ): AnsiString; override;
  public
    constructor Create(
      const AProjectionSetFactory: IProjectionSetFactory
    );
  end;

  TUrlByCoordProviderGTopo30 = class(TUrlByCoordProviderLonLatBase)
  protected
    function GetUrlByLonLat(
      const ALonLat: TDoublePoint
    ): AnsiString; override;
  public
    constructor Create;
  end;

  TUrlByCoordProviderSTRM3 = class(TUrlByCoordProviderLonLatBase)
  protected
    function GetUrlByLonLat(
      const ALonLat: TDoublePoint
    ): AnsiString; override;
  public
    constructor Create;
  end;

  TUrlByCoordProviderOSM = class(TUrlByCoordProviderBase)
  protected
    function GetUrlByLonLat(
      const AProjection: IProjection;
      const ALonLat: TDoublePoint
    ): AnsiString; override;
  public
    constructor Create(
      const AProjectionSetFactory: IProjectionSetFactory
    );
  end;

  TUrlByCoordProviderNokia = class(TUrlByCoordProviderBase)
  protected
    function GetUrlByLonLat(
      const AProjection: IProjection;
      const ALonLat: TDoublePoint
    ): AnsiString; override;
  public
    constructor Create(
      const AProjectionSetFactory: IProjectionSetFactory
    );
  end;

  TUrlByCoordProviderNoaaForecast = class(TBaseInterfacedObject, IUrlByCoordProvider)
  private
    function GetUrl(
      const AConverter: ILocalCoordConverter;
      const ALocalPoint: TPoint
    ): IDownloadRequest;
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  ALString,
  c_CoordConverter,
  i_BinaryData,
  u_BinaryData,
  u_DownloadRequest,
  u_GeoToStrFunc;

{ TUrlByCoordProviderBase }

constructor TUrlByCoordProviderBase.Create(
  const AMainProjectionSet: IProjectionSet
);
begin
  Assert(Assigned(AMainProjectionSet));
  inherited Create;
  FMainProjectionSet := AMainProjectionSet;
end;

function TUrlByCoordProviderBase.GetUrl(
  const AConverter: ILocalCoordConverter;
  const ALocalPoint: TPoint
): IDownloadRequest;
var
  VProjection: IProjection;
  VProjectionMain: IProjection;
  VMapPoint: TDoublePoint;
  VLonLat: TDoublePoint;
  VUrl: AnsiString;
begin
  VProjection := AConverter.Projection;
  VMapPoint := AConverter.LocalPixel2MapPixelFloat(ALocalPoint);
  VProjection.ValidatePixelPosFloatStrict(VMapPoint, False);
  VLonLat := VProjection.PixelPosFloat2LonLat(VMapPoint);
  VProjectionMain := FMainProjectionSet.GetSuitableProjection(VProjection);
  VUrl := GetUrlByLonLat(VProjectionMain, VLonLat);
  Result := TDownloadRequest.Create(VUrl, '', nil);
end;

{ TUrlByCoordProviderLonLatBase }

constructor TUrlByCoordProviderLonLatBase.Create;
begin
  inherited Create;
end;

function TUrlByCoordProviderLonLatBase.GetUrl(
  const AConverter: ILocalCoordConverter;
  const ALocalPoint: TPoint
): IDownloadRequest;
var
  VProjection: IProjection;
  VMapPoint: TDoublePoint;
  VLonLat: TDoublePoint;
  VUrl: AnsiString;
begin
  VProjection := AConverter.Projection;
  VMapPoint := AConverter.LocalPixel2MapPixelFloat(ALocalPoint);
  VProjection.ValidatePixelPosFloatStrict(VMapPoint, False);
  VLonLat := VProjection.PixelPosFloat2LonLat(VMapPoint);
  VUrl := GetUrlByLonLat(VLonLat);
  Result := TDownloadRequest.Create(VUrl, '', nil);
end;

{ TUrlByCoordProviderBing }

constructor TUrlByCoordProviderBing.Create(
  const AProjectionSetFactory: IProjectionSetFactory
);
begin
  inherited Create(AProjectionSetFactory.GetProjectionSetByCode(CGoogleProjectionEPSG, CTileSplitQuadrate256x256));
end;

function TUrlByCoordProviderBing.GetUrlByLonLat(
  const AProjection: IProjection;
  const ALonLat: TDoublePoint
): AnsiString;
begin
  Result :=
    'http://www.bing.com/maps/default.aspx?v=2&cp=' +
    R2AnsiStrPoint(ALonLat.y) + '~' +
    R2AnsiStrPoint(ALonLat.x) +
    '&style=h&lvl=' + ALIntToStr(AProjection.Zoom);
end;

{ TUrlByCoordProviderKosmosnimki }

constructor TUrlByCoordProviderKosmosnimki.Create(
  const AProjectionSetFactory: IProjectionSetFactory
);
begin
  inherited Create(AProjectionSetFactory.GetProjectionSetByCode(CGoogleProjectionEPSG, CTileSplitQuadrate256x256));
end;

function TUrlByCoordProviderKosmosnimki.GetUrlByLonLat(
  const AProjection: IProjection;
  const ALonLat: TDoublePoint
): AnsiString;
begin
  Result :=
    'http://kosmosnimki.ru/?x=' +
    R2AnsiStrPoint(ALonLat.x) +
    '&y=' + R2AnsiStrPoint(ALonLat.y) +
    '&z=' + ALIntToStr(AProjection.Zoom) +
    '&fullscreen=False&mode=satellite';
end;

{ TUrlByCoordProviderYandex }

constructor TUrlByCoordProviderYandex.Create(
  const AProjectionSetFactory: IProjectionSetFactory
);
begin
  inherited Create(AProjectionSetFactory.GetProjectionSetByCode(CYandexProjectionEPSG, CTileSplitQuadrate256x256));
end;

function TUrlByCoordProviderYandex.GetUrlByLonLat(
  const AProjection: IProjection;
  const ALonLat: TDoublePoint
): AnsiString;
begin
  Result :=
    'http://maps.yandex.ru/?ll=' +
    R2AnsiStrPoint(round(ALonLat.x * 100000) / 100000) + '%2C' +
    R2AnsiStrPoint(round(ALonLat.y * 100000) / 100000) +
    '&z=' + ALIntToStr(AProjection.Zoom) +
    '&l=sat';
end;

{ TUrlByCoordProviderGoogle }

constructor TUrlByCoordProviderGoogle.Create(
  const AProjectionSetFactory: IProjectionSetFactory
);
begin
  inherited Create(AProjectionSetFactory.GetProjectionSetByCode(CGoogleProjectionEPSG, CTileSplitQuadrate256x256));
end;

function TUrlByCoordProviderGoogle.GetUrlByLonLat(
  const AProjection: IProjection;
  const ALonLat: TDoublePoint
): AnsiString;
begin
  Result :=
    'http://maps.google.com/?ie=UTF8&ll=' +
    R2AnsiStrPoint(ALonLat.y) + ',' +
    R2AnsiStrPoint(ALonLat.x) +
    '&spn=57.249013,100.371094&t=h&z=' + ALIntToStr(AProjection.Zoom);
end;

{ TUrlByCoordProviderGTopo30 }

constructor TUrlByCoordProviderGTopo30.Create;
begin
  inherited Create;
end;

function TUrlByCoordProviderGTopo30.GetUrlByLonLat(
  const ALonLat: TDoublePoint
): AnsiString;
begin
  Result :=
    'http://api.geonames.org/gtopo30?' +
    'lat=' + R2AnsiStrPoint(ALonLat.Y) + '&' +
    'lng=' + R2AnsiStrPoint(ALonLat.X) + '&' +
    'username=sasgis';
end;

{ TUrlByCoordProviderSTRM3 }

constructor TUrlByCoordProviderSTRM3.Create;
begin
  inherited Create;
end;

function TUrlByCoordProviderSTRM3.GetUrlByLonLat(
  const ALonLat: TDoublePoint
): AnsiString;
begin
  Result :=
    'http://api.geonames.org/srtm3?' +
    'lat=' + R2AnsiStrPoint(ALonLat.Y) + '&' +
    'lng=' + R2AnsiStrPoint(ALonLat.X) + '&' +
    'username=sasgis';
end;

{ TUrlByCoordProviderOSM }

constructor TUrlByCoordProviderOSM.Create(
  const AProjectionSetFactory: IProjectionSetFactory
);
begin
  inherited Create(AProjectionSetFactory.GetProjectionSetByCode(CGoogleProjectionEPSG, CTileSplitQuadrate256x256));
end;

function TUrlByCoordProviderOSM.GetUrlByLonLat(
  const AProjection: IProjection;
  const ALonLat: TDoublePoint
): AnsiString;
begin
  Result :=
    'http://www.openstreetmap.org/?lat=' +
    R2AnsiStrPoint(ALonLat.y) +
    '&lon=' + R2AnsiStrPoint(ALonLat.x) +
    '&mlat=' + R2AnsiStrPoint(ALonLat.y) +
    '&mlon=' + R2AnsiStrPoint(ALonLat.x) +
    '&zoom=' + ALIntToStr(AProjection.Zoom);
end;

{ TUrlByCoordProviderNokia }

constructor TUrlByCoordProviderNokia.Create(
  const AProjectionSetFactory: IProjectionSetFactory
);
begin
  inherited Create(AProjectionSetFactory.GetProjectionSetByCode(CGoogleProjectionEPSG, CTileSplitQuadrate256x256));
end;

function TUrlByCoordProviderNokia.GetUrlByLonLat(
  const AProjection: IProjection;
  const ALonLat: TDoublePoint
): AnsiString;
begin
  Result :=
    'http://maps.nokia.com/mapcreator/?ns=True#|' +
    R2AnsiStrPoint(ALonLat.y) + '|' +
    R2AnsiStrPoint(ALonLat.x) + '|' +
    ALIntToStr(AProjection.Zoom) +
    '|0|0|';
end;

{ TUrlByCoordProviderNoaaForecast }

constructor TUrlByCoordProviderNoaaForecast.Create;
begin
  inherited Create;
end;

function TUrlByCoordProviderNoaaForecast.GetUrl(
  const AConverter: ILocalCoordConverter;
  const ALocalPoint: TPoint
): IDownloadRequest;
var
  VProjection: IProjection;
  VMapPoint: TDoublePoint;
  VLonLat: TDoublePoint;
  VUrl: AnsiString;
  VReferer: AnsiString;
  VHeaders: AnsiString;
  VPostData: IBinaryData;
begin
  VProjection := AConverter.Projection;
  VMapPoint := AConverter.LocalPixel2MapPixelFloat(ALocalPoint);
  VProjection.ValidatePixelPosFloatStrict(VMapPoint, False);
  VLonLat := VProjection.PixelPosFloat2LonLat(VMapPoint);

  VUrl := 'http://ready.arl.noaa.gov/ready2-bin/main.pl';
  VReferer := 'http://ready.arl.noaa.gov/READYcmet.php';
  VHeaders := 'Referer: ' + VReferer + #$D#$A +
    'Content-Type: application/x-www-form-urlencoded';

  VPostData :=
    TBinaryData.CreateByAnsiString(
      'userid=&map=WORLD&newloc=1&WMO=&city=Or+choose+a+city+--%3E&Lat=' + RoundExAnsi(VLonLat.y, 2) + '&Lon=' + RoundExAnsi(VLonLat.x, 2)
    );
  Result := TDownloadPostRequest.Create(VUrl, VHeaders, VPostData, nil);
end;

end.
