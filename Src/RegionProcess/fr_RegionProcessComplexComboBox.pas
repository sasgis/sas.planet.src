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

unit fr_RegionProcessComplexComboBox;

interface

uses
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  i_LanguageManager,
  i_GeometryLonLat,
  i_RegionProcessProvider,
  i_InterfaceListStatic,
  i_RegionProcessParamsFrame,
  u_CommonFormAndFrameParents;

type
  TfrRegionProcessComplexComboBox = class(TFrame, IRegionProcessComplexFrame)
    pnlTop: TPanel;
    pnlOutputFormat: TPanel;
    lblOutputFormat: TLabel;
    cbbOutputFormat: TComboBox;
    pnlContent: TPanel;
    procedure cbbOutputFormatChange(Sender: TObject);
  private
    FProviders: IInterfaceListStatic;
    FHeader: string;
    FLabel: string;
    FZoom: Byte;
    FPolygon: IGeometryLonLatPolygon;
  protected
    procedure OnShow(const AIsFirstTime: Boolean); override;
    procedure OnHide; override;
  private
    { IRegionProcessComplexFrame }
    procedure Init(
      const AZoom: Byte;
      const APolygon: IGeometryLonLatPolygon
    );
    function GetActiveProvider: IRegionProcessProvider; inline;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AProviders: IInterfaceListStatic;
      const AHeader: string;
      const ALabel: string
    ); reintroduce;
    procedure RefreshTranslation; override;
  end;

implementation

uses
  gnugettext;

{$R *.dfm}

{ TfrRegionProcessComplexComboBox }

constructor TfrRegionProcessComplexComboBox.Create(
  const ALanguageManager: ILanguageManager;
  const AProviders: IInterfaceListStatic;
  const AHeader: string;
  const ALabel: string
);
var
  I: Integer;
  VExportProvider: IRegionProcessProvider;
begin
  Assert(Assigned(AProviders));
  TP_Ignore(Self, 'cbbOutputFormat.Items');

  inherited Create(ALanguageManager);

  FProviders := AProviders;
  FHeader := AHeader;
  FLabel := ALabel;

  if AHeader <> '' then begin
    pnlTop.Caption := _(FHeader);
  end else begin
    pnlTop.Visible := False;
  end;
  lblOutputFormat.Caption := _(FLabel);

  cbbOutputFormat.Items.BeginUpdate;
  try
    for I := 0 to FProviders.Count - 1 do begin
      VExportProvider := IRegionProcessProvider(FProviders.Items[I]);
      cbbOutputFormat.Items.Add(VExportProvider.GetCaption);
    end;
    cbbOutputFormat.DropDownCount := cbbOutputFormat.Items.Count;
  finally
    cbbOutputFormat.Items.EndUpdate;
  end;
  Assert(cbbOutputFormat.Items.Count = FProviders.Count);

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

procedure TfrRegionProcessComplexComboBox.OnShow(const AIsFirstTime: Boolean);
begin
  inherited;
  if AIsFirstTime then begin
    if cbbOutputFormat.ItemIndex < 0 then begin
      cbbOutputFormat.ItemIndex := 0;
    end;
  end;
end;

procedure TfrRegionProcessComplexComboBox.OnHide;
begin
  inherited;
  GetActiveProvider.Hide;
end;

function TfrRegionProcessComplexComboBox.GetActiveProvider: IRegionProcessProvider;
begin
  Result := IRegionProcessProvider(FProviders.Items[cbbOutputFormat.ItemIndex]);
end;

procedure TfrRegionProcessComplexComboBox.cbbOutputFormatChange(Sender: TObject);
var
  I: Integer;
  VExportProvider: IRegionProcessProvider;
begin
  for I := 0 to FProviders.Count - 1 do begin
    VExportProvider := IRegionProcessProvider(FProviders.Items[I]);
    if (VExportProvider <> nil) and ( I <> cbbOutputFormat.ItemIndex) then begin
      VExportProvider.Hide;
    end;
  end;
  GetActiveProvider.Show(pnlContent, FZoom, FPolygon);
end;

procedure TfrRegionProcessComplexComboBox.RefreshTranslation;
var
  I: Integer;
  VIndex: Integer;
  VProvider: IRegionProcessProvider;
begin
  inherited;

  pnlTop.Caption := _(FHeader);
  lblOutputFormat.Caption := _(FLabel);

  cbbOutputFormat.Items.BeginUpdate;
  try
    VIndex := cbbOutputFormat.ItemIndex;
    for I := 0 to FProviders.Count - 1 do begin
      VProvider := IRegionProcessProvider(FProviders.Items[I]);
      cbbOutputFormat.Items[I] := VProvider.GetCaption;
    end;
    cbbOutputFormat.ItemIndex := VIndex;
  finally
    cbbOutputFormat.Items.EndUpdate;
  end;
end;

procedure TfrRegionProcessComplexComboBox.Init(
  const AZoom: Byte;
  const APolygon: IGeometryLonLatPolygon
);
begin
  FZoom := AZoom;
  FPolygon := APolygon;
  GetActiveProvider.Show(pnlContent, AZoom, APolygon);
end;

end.
