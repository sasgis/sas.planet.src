{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit fr_RegionProcessComplexPageControl;

interface

uses
  Classes,
  Controls,
  Forms,
  ComCtrls,
  i_LanguageManager,
  i_GeometryLonLat,
  i_RegionProcessProvider,
  i_InterfaceListStatic,
  i_RegionProcessParamsFrame,
  u_CommonFormAndFrameParents;

type
  TfrRegionProcessComplexPageControl = class(TFrame, IRegionProcessComplexFrame)
    PageControl: TPageControl;
    procedure PageControlChange(Sender: TObject);
  private
    FProviders: IInterfaceListStatic;
    FInited: array of Boolean;
    FZoom: Byte;
    FPolygon: IGeometryLonLatPolygon;
  private
    { IRegionProcessComplexFrame }
    procedure Init(
      const AZoom: Byte;
      const APolygon: IGeometryLonLatPolygon
    );
    function GetActiveProvider: IRegionProcessProvider;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AProviders: IInterfaceListStatic
    ); reintroduce;
    destructor Destroy; override;
    procedure RefreshTranslation; override;
  end;

implementation

{$R *.dfm}

{ TfrRegionProcessComplexPageControl }

constructor TfrRegionProcessComplexPageControl.Create(
  const ALanguageManager: ILanguageManager;
  const AProviders: IInterfaceListStatic
);
var
  I: Integer;
  VExportProvider: IRegionProcessProvider;
  VTabSheet: TTabSheet;
begin
  Assert(Assigned(AProviders));

  inherited Create(ALanguageManager);

  FProviders := AProviders;

  for I := 0 to FProviders.Count - 1 do begin
    VExportProvider := IRegionProcessProvider(FProviders.Items[I]);
    VTabSheet := TTabSheet.Create(PageControl);
    VTabSheet.Caption := VExportProvider.GetCaption;
    VTabSheet.PageControl := PageControl;
  end;
  SetLength(FInited, FProviders.Count);

  Assert(PageControl.PageCount = FProviders.Count);
  PageControl.Pages[0].Show;
end;

destructor TfrRegionProcessComplexPageControl.Destroy;
begin
  FProviders := nil;
  inherited;
end;

function TfrRegionProcessComplexPageControl.GetActiveProvider: IRegionProcessProvider;
begin
  Result := IRegionProcessProvider(FProviders.Items[PageControl.ActivePageIndex]);
end;

procedure TfrRegionProcessComplexPageControl.Init(
  const AZoom: Byte;
  const APolygon: IGeometryLonLatPolygon
);
var
  I: Integer;
  VExportProvider: IRegionProcessProvider;
  VTabSheet: TTabSheet;
begin
  FZoom := AZoom;
  FPolygon := APolygon;
  for I := 0 to FProviders.Count - 1 do begin
    FInited[I] := False;
  end;
  I := PageControl.ActivePageIndex;
  VExportProvider := IRegionProcessProvider(FProviders.Items[I]);
  VTabSheet := PageControl.Pages[I];
  VExportProvider.Show(VTabSheet, AZoom, APolygon);
  FInited[I] := True;
end;

procedure TfrRegionProcessComplexPageControl.PageControlChange(Sender: TObject);
var
  I: Integer;
  VExportProvider: IRegionProcessProvider;
  VTabSheet: TTabSheet;
begin
  I := PageControl.ActivePageIndex;
  if not FInited[I] then begin
    VExportProvider := IRegionProcessProvider(FProviders.Items[I]);
    VTabSheet := PageControl.Pages[I];
    VExportProvider.Show(VTabSheet, FZoom, FPolygon);
    FInited[I] := True;
  end;
end;

procedure TfrRegionProcessComplexPageControl.RefreshTranslation;
var
  I: Integer;
  VExportProvider: IRegionProcessProvider;
  VTabSheet: TTabSheet;
begin
  inherited;
  for I := 0 to FProviders.Count - 1 do begin
    VExportProvider := IRegionProcessProvider(FProviders.Items[I]);
    VTabSheet := PageControl.Pages[I];
    VTabSheet.Caption := VExportProvider.GetCaption;
  end;
end;

end.
