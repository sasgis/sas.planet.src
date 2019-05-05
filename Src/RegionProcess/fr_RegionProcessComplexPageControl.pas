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
  public
    procedure Init(
      const AZoom: byte;
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

{ TfrDelete }

constructor TfrRegionProcessComplexPageControl.Create(
  const ALanguageManager: ILanguageManager;
  const AProviders: IInterfaceListStatic
);
var
  i: Integer;
  VExportProvider: IRegionProcessProvider;
  VTabSheet: TTabSheet;
begin
  Assert(Assigned(AProviders));
  inherited Create(ALanguageManager);
  FProviders := AProviders;
  for i := 0 to FProviders.Count - 1 do begin
    VExportProvider := IRegionProcessProvider(FProviders.Items[i]);
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
  const AZoom: byte;
  const APolygon: IGeometryLonLatPolygon
);
var
  i: Integer;
  VExportProvider: IRegionProcessProvider;
  VTabSheet: TTabSheet;
begin
  FZoom := AZoom;
  FPolygon := APolygon;
  for i := 0 to FProviders.Count - 1 do begin
    FInited[i] := False;
  end;
  i := PageControl.ActivePageIndex;
  VExportProvider := IRegionProcessProvider(FProviders.Items[i]);
  VTabSheet := PageControl.Pages[i];
  VExportProvider.Show(VTabSheet, AZoom, APolygon);
  FInited[i] := True;
end;

procedure TfrRegionProcessComplexPageControl.PageControlChange(Sender: TObject);
var
  i: Integer;
  VExportProvider: IRegionProcessProvider;
  VTabSheet: TTabSheet;
begin
  i := PageControl.ActivePageIndex;
  if not FInited[i] then begin
    VExportProvider := IRegionProcessProvider(FProviders.Items[i]);
    VTabSheet := PageControl.Pages[i];
    VExportProvider.Show(VTabSheet, FZoom, FPolygon);
    FInited[i] := True;
  end;
end;

procedure TfrRegionProcessComplexPageControl.RefreshTranslation;
var
  i: Integer;
  VExportProvider: IRegionProcessProvider;
  VTabSheet: TTabSheet;
begin
  inherited;
  for i := 0 to FProviders.Count - 1 do begin
    VExportProvider := IRegionProcessProvider(FProviders.Items[i]);
    VTabSheet := PageControl.Pages[i];
    VTabSheet.Caption := VExportProvider.GetCaption;
  end;
end;

end.
