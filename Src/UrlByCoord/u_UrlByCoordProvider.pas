{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2018, SAS.Planet development team.                      *}
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
  end;

  TUrlByCoordProviderSTRM3 = class(TUrlByCoordProviderLonLatBase)
  protected
    function GetUrlByLonLat(
      const ALonLat: TDoublePoint
    ): AnsiString; override;
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

  TUrlByCoordProviderWeatherUnderground = class(TUrlByCoordProviderLonLatBase)
  protected
    function GetUrlByLonLat(
      const ALonLat: TDoublePoint
    ): AnsiString; override;
  end;

  TUrlByCoordProviderYandexWeather = class(TUrlByCoordProviderLonLatBase)
  protected
    function GetUrlByLonLat(
      const ALonLat: TDoublePoint
    ): AnsiString; override;
  end;

  TUrlByCoordProviderRosreestr = class(TUrlByCoordProviderBase)
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

  TUrlByCoordProviderTerraserver = class(TUrlByCoordProviderLonLatBase)
  protected
    function GetUrlByLonLat(
      const ALonLat: TDoublePoint
    ): AnsiString; override;
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
  VProjectionMain.ProjectionType.ValidateLonLatPos(VLonLat);
  VUrl := GetUrlByLonLat(VProjectionMain, VLonLat);
  Result := TDownloadRequest.Create(VUrl, '', nil);
end;

{ TUrlByCoordProviderLonLatBase }

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

{ TUrlByCoordProviderWeatherUnderground }

function TUrlByCoordProviderWeatherUnderground.GetUrlByLonLat(
  const ALonLat: TDoublePoint
): AnsiString;
begin
  Result :=
    'https://www.wunderground.com/cgi-bin/findweather/getForecast?query=' +
    RoundExAnsi(ALonLat.Y, 4) + ',' + RoundExAnsi(ALonLat.X, 4);
end;

{ TUrlByCoordProviderYandexWeather }

function TUrlByCoordProviderYandexWeather.GetUrlByLonLat(
  const ALonLat: TDoublePoint
): AnsiString;
begin
  Result :=
    'https://yandex.ru/pogoda?' +
    'lat=' + RoundExAnsi(ALonLat.Y, 4) + '&' +
    'lon=' + RoundExAnsi(ALonLat.X, 4);
end;

{ TUrlByCoordProviderRosreestr }

constructor TUrlByCoordProviderRosreestr.Create(
  const AProjectionSetFactory: IProjectionSetFactory
);
begin
  inherited Create(AProjectionSetFactory.GetProjectionSetByCode(CGoogleProjectionEPSG, CTileSplitQuadrate256x256));
end;

function TUrlByCoordProviderRosreestr.GetUrlByLonLat(
  const AProjection: IProjection;
  const ALonLat: TDoublePoint
): AnsiString;
var
  VMetr: TDoublePoint;
begin
  VMetr := AProjection.ProjectionType.LonLat2Metr(ALonLat);
  Result :=
    'http://pkk5.rosreestr.ru/#' +
    'x=' + RoundExAnsi(VMetr.x, 9) +
    '&y=' + RoundExAnsi(VMetr.y, 9) +
    '&z=' + ALIntToStr(AProjection.Zoom);
end;

{ TUrlByCoordProviderTerraserver }

function TUrlByCoordProviderTerraserver.GetUrlByLonLat(
  const ALonLat: TDoublePoint
): AnsiString;
begin
  Result :=
    'http://www.terraserver.com/view.asp?' +
    'cx=' + RoundExAnsi(ALonLat.x, 4) +
    '&cy=' + RoundExAnsi(ALonLat.y, 4) +
    // scale (by zoom and lat):
    // 10 (failed), 5 if (zoom <= 15), 2.5 if (zoom = 16), 1.5 if (zoom = 17), 1, 0.75 (zoom = 18), 0.5, 0.25, 0.15 (failed)
    // select from values above (round up to nearest) for another values
    '&mpp=5' +
    '&proj=4326&pic=img&prov=-1&stac=-1&ovrl=-1&vic=';
end;

end.
