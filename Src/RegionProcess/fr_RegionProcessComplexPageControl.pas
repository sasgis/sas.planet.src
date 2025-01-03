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

unit fr_RegionProcessComplexPageControl;

interface

uses
  Classes,
  Controls,
  Forms,
  ComCtrls,
  SysUtils,
  i_LanguageManager,
  i_GeometryLonLat,
  i_RegionProcessProvider,
  i_InterfaceListStatic,
  i_RegionProcessParamsFrame,
  u_CommonFormAndFrameParents;

type
  TfrRegionProcessComplexPageControl = class(TFrame, IRegionProcessComplexFrame)
    pgcMain: TPageControl;
    procedure pgcMainChange(Sender: TObject);
  private
    FProviders: IInterfaceListStatic;
    FInited: array of Boolean;
    FZoom: Byte;
    FPolygon: IGeometryLonLatPolygon;
  protected
    procedure OnShow(const AIsFirstTime: Boolean); override;
    procedure OnHide; override;
    procedure RefreshTranslation; override;
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
    VTabSheet := TTabSheet.Create(pgcMain);
    VTabSheet.Name := 'tsExportProvider' + IntToStr(I); // todo: use some guid?
    VTabSheet.Caption := VExportProvider.GetCaption;
    VTabSheet.PageControl := pgcMain;
  end;
  SetLength(FInited, FProviders.Count);

  Assert(pgcMain.PageCount = FProviders.Count);

  FPropertyState := CreateComponentPropertyState(
    Self,
    [],    // ignored components
    [],    // temp components
    True,  // save on hide
    False, // save on free
    True,  // restore on show
    True   // ignore secondary restore calls
  );
end;

procedure TfrRegionProcessComplexPageControl.OnShow(const AIsFirstTime: Boolean);
begin
  inherited;

  if AIsFirstTime then begin
    if pgcMain.ActivePageIndex < 0 then begin
      pgcMain.ActivePageIndex := 0;
    end;
  end;
end;

procedure TfrRegionProcessComplexPageControl.OnHide;
var
  I: Integer;
begin
  inherited;

  for I := 0 to FProviders.Count - 1 do begin
    if FInited[I] and (I <> pgcMain.ActivePageIndex) then begin
      IRegionProcessProvider(FProviders.Items[I]).Hide;
      FInited[I] := False;
    end;
  end;

  // hide the active page on the last step
  I := pgcMain.ActivePageIndex;
  if FInited[I] then begin
    IRegionProcessProvider(FProviders.Items[I]).Hide;
    FInited[I] := False;
  end;
end;

function TfrRegionProcessComplexPageControl.GetActiveProvider: IRegionProcessProvider;
begin
  Result := IRegionProcessProvider(FProviders.Items[pgcMain.ActivePageIndex]);
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
  I := pgcMain.ActivePageIndex;
  VExportProvider := IRegionProcessProvider(FProviders.Items[I]);
  VTabSheet := pgcMain.Pages[I];
  VExportProvider.Show(VTabSheet, AZoom, APolygon);
  FInited[I] := True;
end;

procedure TfrRegionProcessComplexPageControl.pgcMainChange(Sender: TObject);
var
  I: Integer;
  VExportProvider: IRegionProcessProvider;
  VTabSheet: TTabSheet;
begin
  I := pgcMain.ActivePageIndex;
  if not FInited[I] then begin
    VExportProvider := IRegionProcessProvider(FProviders.Items[I]);
    VTabSheet := pgcMain.Pages[I];
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
    VTabSheet := pgcMain.Pages[I];
    VTabSheet.Caption := VExportProvider.GetCaption;
  end;
end;

end.
