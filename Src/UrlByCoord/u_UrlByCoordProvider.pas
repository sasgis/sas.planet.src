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

  TUrlByCoordProviderGoogleEarthWeb = class(TUrlByCoordProviderBase)
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

  TUrlByCoordProviderNakarte = class(TUrlByCoordProviderBase)
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

implementation

uses
  SysUtils,
  Math,
  c_CoordConverter,
  i_BinaryData,
  u_AnsiStr,
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
    'https://www.openstreetmap.org/?' +
    'mlat=' + R2AnsiStrPoint(ALonLat.Y) +
    '&mlon=' + R2AnsiStrPoint(ALonLat.X) +
    '#map=' + IntToStrA(AProjection.Zoom) +
    '/' + RoundExAnsi(ALonLat.Y, 5) +
    '/' + RoundExAnsi(ALonLat.X, 5);
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
    'https://maps.google.com/?ie=UTF8&ll=' +
    R2AnsiStrPoint(ALonLat.Y) + ',' +
    R2AnsiStrPoint(ALonLat.X) +
    '&spn=57.249013,100.371094&t=h&z=' + IntToStrA(AProjection.Zoom);
end;

{ TUrlByCoordProviderGoogleEarthWeb }

constructor TUrlByCoordProviderGoogleEarthWeb.Create(
  const AProjectionSetFactory: IProjectionSetFactory
);
begin
  inherited Create(AProjectionSetFactory.GetProjectionSetByCode(CGoogleProjectionEPSG, CTileSplitQuadrate256x256));
end;

function TUrlByCoordProviderGoogleEarthWeb.GetUrlByLonLat(
  const AProjection: IProjection;
  const ALonLat: TDoublePoint
): AnsiString;

  function _CalcAltitude(AZoom: Byte): Double;
  const
    cAltitudeOnZoom8 = 63170000; // max
  begin
    Inc(AZoom); // count zoom from 1
    if AZoom <= 8 then begin
      Result := cAltitudeOnZoom8;
    end else begin
      Result := cAltitudeOnZoom8 / Math.Power(2, AZoom - 8);
    end;
  end;

begin
  Result :=
    'https://earth.google.com/web/@' +
    RoundExAnsi(ALonLat.Y, 8) + ',' +
    RoundExAnsi(ALonLat.X, 8) + ',' +
    '0a,' +
    RoundExAnsi(_CalcAltitude(AProjection.Zoom), 3) + 'd';
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
    'https://www.bing.com/maps?v=2&cp=' +
    R2AnsiStrPoint(ALonLat.Y) + '~' +
    R2AnsiStrPoint(ALonLat.X) +
    '&style=h&lvl=' + IntToStrA(AProjection.Zoom);
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
    'https://yandex.ru/maps/?ll=' +
    R2AnsiStrPoint(Round(ALonLat.X * 100000) / 100000) + '%2C' +
    R2AnsiStrPoint(Round(ALonLat.Y * 100000) / 100000) +
    '&z=' + IntToStrA(AProjection.Zoom) +
    '&l=sat';
end;

{ TUrlByCoordProviderNakarte }

constructor TUrlByCoordProviderNakarte.Create(const AProjectionSetFactory: IProjectionSetFactory);
begin
  inherited Create(AProjectionSetFactory.GetProjectionSetByCode(CGoogleProjectionEPSG, CTileSplitQuadrate256x256));
end;

function TUrlByCoordProviderNakarte.GetUrlByLonLat(
  const AProjection: IProjection;
  const ALonLat: TDoublePoint
): AnsiString;
begin
  Result :=
    'https://nakarte.me/' +
    '#m=' + IntToStrA(AProjection.Zoom) +
    '/' + RoundExAnsi(ALonLat.Y, 5) +
    '/' + RoundExAnsi(ALonLat.X, 5);
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
    'https://kosmosnimki.ru/?x=' +
    R2AnsiStrPoint(ALonLat.X) +
    '&y=' + R2AnsiStrPoint(ALonLat.Y) +
    '&z=' + IntToStrA(AProjection.Zoom) +
    '&fullscreen=False&mode=satellite';
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

end.
