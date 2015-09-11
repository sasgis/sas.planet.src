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

unit fr_Delete;

interface

uses
  Classes,
  Controls,
  Forms,
  ComCtrls,
  i_LanguageManager,
  i_GeometryLonLat,
  i_CoordConverterFactory,
  i_LocalCoordConverterChangeable,
  i_GeometryProjectedFactory,
  i_MarkSystem,
  i_RegionProcessProgressInfoInternalFactory,
  i_RegionProcessProvider,
  fr_MapSelect,
  u_CommonFormAndFrameParents;

type
  TfrDelete = class(TFrame)
    PageControl1: TPageControl;
    tsTiles: TTabSheet;
    tsMarks: TTabSheet;
  private
    FMarks: IRegionProcessProvider;
    FTiles: IRegionProcessProvider;
  public
    procedure StartProcess(const APolygon: IGeometryLonLatPolygon);
    procedure Show(
      AParent: TWinControl;
      AZoom: byte;
      const APolygon: IGeometryLonLatPolygon
    );
    function Validate: Boolean;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const APosition: ILocalCoordConverterChangeable;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AMarkSystem: IMarkSystem
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  u_ProviderDeleteTiles,
  u_ProviderDeleteMarks;

{$R *.dfm}

{ TfrDelete }

constructor TfrDelete.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const APosition: ILocalCoordConverterChangeable;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AMarkSystem: IMarkSystem
);
begin
  inherited Create(ALanguageManager);
  FTiles :=
    TProviderDeleteTiles.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AVectorGeometryProjectedFactory
    );
  FMarks :=
    TProviderDeleteMarks.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      APosition,
      AVectorGeometryProjectedFactory,
      AMarkSystem
    );
end;

destructor TfrDelete.Destroy;
begin
  FMarks := nil;
  FTiles := nil;
  inherited;
end;

procedure TfrDelete.Show(
  AParent: TWinControl;
  AZoom: byte;
  const APolygon: IGeometryLonLatPolygon
);
begin
  Parent := AParent;
  FMarks.Show(tsMarks, AZoom, APolygon);
  FTiles.Show(tsTiles, AZoom, APolygon);
end;

procedure TfrDelete.StartProcess(const APolygon: IGeometryLonLatPolygon);
begin
  if PageControl1.ActivePageIndex = 0 then begin
    FTiles.StartProcess(APolygon);
  end else begin
    FMarks.StartProcess(APolygon);
  end;
end;

function TfrDelete.Validate: Boolean;
begin
  if PageControl1.ActivePageIndex = 0 then begin
    Result := FTiles.Validate;
  end else begin
    Result := FMarks.Validate;
  end;
end;

end.
