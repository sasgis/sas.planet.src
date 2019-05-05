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
    FZoom: byte;
    FPolygon: IGeometryLonLatPolygon;
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: IGeometryLonLatPolygon
    );
    function GetActiveProvider: IRegionProcessProvider;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AProviders: IInterfaceListStatic;
      const AHeader: string;
      const ALabel: string
    ); reintroduce;
    destructor Destroy; override;
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
  i: Integer;
  VExportProvider: IRegionProcessProvider;
begin
  Assert(Assigned(AProviders));
  TP_Ignore(Self, 'cbbOutputFormat.Items');
  inherited Create(ALanguageManager);
  FHeader := AHeader;
  FLabel := ALabel;
  if AHeader <> '' then begin
    pnlTop.Caption := _(FHeader);
  end else begin
    pnlTop.Visible := False;
  end;
  lblOutputFormat.Caption := _(FLabel);

  FProviders := AProviders;

  cbbOutputFormat.Items.BeginUpdate;
  try
    for i := 0 to FProviders.Count - 1 do begin
      VExportProvider := IRegionProcessProvider(FProviders.Items[i]);
      cbbOutputFormat.Items.Add(VExportProvider.GetCaption);
    end;
    cbbOutputFormat.ItemIndex := 0;
    cbbOutputFormat.DropDownCount := cbbOutputFormat.Items.Count;
  finally
    cbbOutputFormat.Items.EndUpdate;
  end;
  Assert(cbbOutputFormat.Items.Count = FProviders.Count);
end;

destructor TfrRegionProcessComplexComboBox.Destroy;
begin
  FProviders := nil;
  inherited;
end;

function TfrRegionProcessComplexComboBox.GetActiveProvider: IRegionProcessProvider;
begin
  Result := IRegionProcessProvider(FProviders.Items[cbbOutputFormat.ItemIndex]);
end;

procedure TfrRegionProcessComplexComboBox.cbbOutputFormatChange(Sender: TObject);
var
  VExportProvider: IRegionProcessProvider;
  i: Integer;
begin
  for i := 0 to FProviders.Count - 1 do begin
    VExportProvider := IRegionProcessProvider(FProviders.Items[i]);
    if VExportProvider <> nil then begin
      if i = cbbOutputFormat.ItemIndex then begin
        VExportProvider.Show(pnlContent, FZoom, FPolygon);
      end else begin
        VExportProvider.Hide;
      end;
    end;
  end;
end;

procedure TfrRegionProcessComplexComboBox.RefreshTranslation;
var
  i: Integer;
  VProvider: IRegionProcessProvider;
  VIndex: Integer;
begin
  inherited;
  pnlTop.Caption := _(FHeader);
  lblOutputFormat.Caption := _(FLabel);

  cbbOutputFormat.Items.BeginUpdate;
  try
    VIndex := cbbOutputFormat.ItemIndex;
    for i := 0 to FProviders.Count - 1 do begin
      VProvider := IRegionProcessProvider(FProviders.Items[i]);
      cbbOutputFormat.Items[i] := VProvider.GetCaption;
    end;
    cbbOutputFormat.ItemIndex := VIndex;
  finally
    cbbOutputFormat.Items.EndUpdate;
  end;
end;

procedure TfrRegionProcessComplexComboBox.Init(
  const AZoom: byte;
  const APolygon: IGeometryLonLatPolygon
);
var
  VIndex: integer;
  VExportProvider: IRegionProcessProvider;
begin
  FZoom := AZoom;
  FPolygon := APolygon;
  VIndex := cbbOutputFormat.ItemIndex;
  VExportProvider := IRegionProcessProvider(FProviders.Items[VIndex]);
  VExportProvider.Show(pnlContent, AZoom, APolygon);
end;

end.
