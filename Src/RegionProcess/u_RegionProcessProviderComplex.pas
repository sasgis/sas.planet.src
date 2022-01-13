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

unit u_RegionProcessProviderComplex;

interface


uses
  Windows,
  Forms,
  Controls,
  i_LanguageManager,
  i_GeometryLonLat,
  i_RegionProcessProvider,
  i_RegionProcessParamsFrame,
  i_InterfaceListStatic,
  u_BaseInterfacedObject;

type
  TRegionProcessProviderComplex = class(TBaseInterfacedObject, IRegionProcessProvider)
  private
    FLanguageManager: ILanguageManager;
    FProviders: IInterfaceListStatic;
    FUseTabs: Boolean;
    FCaption: string;
    FHeader: string;
    FLabel: string;

    FFrame: TFrame;

    function CreateFrame: TFrame;

    function GetFrame: IRegionProcessComplexFrame;
    property Frame: IRegionProcessComplexFrame read GetFrame;
  private
    function GetCaption: string;
    procedure Show(
      AParent: TWinControl;
      AZoom: byte;
      const APolygon: IGeometryLonLatPolygon
    );
    procedure Hide;
    function Validate(
      const APolygon: IGeometryLonLatPolygon
    ): Boolean;
    procedure StartProcess(
      const APolygon: IGeometryLonLatPolygon
    );
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AProviders: IInterfaceListStatic;
      const AUseTabs: Boolean;
      const ACaption: string;
      const AHeader: string;
      const ALabel: string
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  gnugettext,
  fr_RegionProcessComplexPageControl,
  fr_RegionProcessComplexComboBox;

{ TRegionProcessProviderComplex }

constructor TRegionProcessProviderComplex.Create(
  const ALanguageManager: ILanguageManager;
  const AProviders: IInterfaceListStatic;
  const AUseTabs: Boolean;
  const ACaption: string;
  const AHeader: string;
  const ALabel: string
);
begin
  Assert(Assigned(ALanguageManager));
  Assert(Assigned(AProviders));
  inherited Create;
  FLanguageManager := ALanguageManager;
  FProviders := AProviders;
  FUseTabs := AUseTabs;
  FCaption := ACaption;
  FHeader := AHeader;
  FLabel := ALabel;
end;

destructor TRegionProcessProviderComplex.Destroy;
begin
  FProviders := nil;
  FreeAndNil(FFrame);
  inherited;
end;

function TRegionProcessProviderComplex.CreateFrame: TFrame;
begin
  if FUseTabs then begin
    Result :=
      TfrRegionProcessComplexPageControl.Create(
        FLanguageManager,
        FProviders
      );
  end else begin
    Result :=
      TfrRegionProcessComplexComboBox.Create(
        FLanguageManager,
        FProviders,
        FHeader,
        FLabel
      );
  end;
end;

function TRegionProcessProviderComplex.GetCaption: string;
begin
  Result := _(FCaption);
end;

function TRegionProcessProviderComplex.GetFrame: IRegionProcessComplexFrame;
begin
  if not Supports(FFrame, IRegionProcessComplexFrame, Result) then begin
    Result := nil;
  end;
end;

procedure TRegionProcessProviderComplex.Hide;
begin
  if FFrame <> nil then begin
    if FFrame.Visible then begin
      FFrame.Hide;
    end;
  end;
end;

procedure TRegionProcessProviderComplex.Show(
  AParent: TWinControl;
  AZoom: byte;
  const APolygon: IGeometryLonLatPolygon
);
var
  VFrame: IRegionProcessComplexFrame;
begin
  if FFrame = nil then begin
    FFrame := CreateFrame;
    Assert(Supports(FFrame, IRegionProcessComplexFrame));
  end;
  if FFrame <> nil then begin
    FFrame.Parent := AParent;
    if not FFrame.Visible then begin
      FFrame.Show;
    end;
    VFrame := Frame;
    if VFrame <> nil then begin
      VFrame.Init(AZoom, APolygon);
    end;
  end;
end;

procedure TRegionProcessProviderComplex.StartProcess(
  const APolygon: IGeometryLonLatPolygon
);
var
  VFrame: IRegionProcessComplexFrame;
  VExportProvider: IRegionProcessProvider;
begin
  VFrame := Frame;
  if Assigned(VFrame) then begin
    VExportProvider := VFrame.ActiveProvider;
    if Assigned(VExportProvider) then begin
      VExportProvider.StartProcess(APolygon);
    end;
  end;
end;

function TRegionProcessProviderComplex.Validate(
  const APolygon: IGeometryLonLatPolygon
): Boolean;
var
  VFrame: IRegionProcessComplexFrame;
  VExportProvider: IRegionProcessProvider;
begin
  Result := False;
  VFrame := Frame;
  if Assigned(VFrame) then begin
    VExportProvider := VFrame.ActiveProvider;
    if Assigned(VExportProvider) then begin
      Result := VExportProvider.Validate(APolygon);
    end;
  end;
end;

end.
