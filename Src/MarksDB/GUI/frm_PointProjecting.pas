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

unit frm_PointProjecting;

interface

uses
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  i_GeoCalc,
  i_LanguageManager,
  i_InterfaceListStatic,
  i_GeometryLonLatFactory,
  i_LocalCoordConverterChangeable,
  u_MarkDbGUIHelper,
  u_CommonFormAndFrameParents;

type
  TfrmPointProjecting = class(TFormWitghLanguageManager)
    pnlDist: TPanel;
    lblDist: TLabel;
    edtDist: TEdit;
    pnlAzimuth: TPanel;
    lblAzimuth: TLabel;
    edtAzimuth: TEdit;
    rgSourcePointType: TRadioGroup;
    cbbAllMarks: TComboBox;
    pnlBottom: TPanel;
    btnCreatePoint: TButton;
    procedure btnCreatePointClick(Sender: TObject);
    procedure cbbAllMarksDropDown(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure rgSourcePointTypeClick(Sender: TObject);
  private
    FGeoCalc: IGeoCalcChangeable;
    FMarkDBGUI: TMarkDbGUIHelper;
    FPosition: ILocalCoordConverterChangeable;
    FGeometryLonLatFactory: IGeometryLonLatFactory;
    FMarksList: IInterfaceListStatic;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AGeometryLonLatFactory: IGeometryLonLatFactory;
      const AMarkDBGUI: TMarkDbGUIHelper;
      const APosition: ILocalCoordConverterChangeable;
      const AGeoCalc: IGeoCalcChangeable
    ); reintroduce;
  end;

implementation

uses
  t_GeoTypes,
  i_MarkId,
  i_GeometryLonLat,
  i_VectorDataItemSimple,
  u_GeoToStrFunc;

{$R *.dfm}

constructor TfrmPointProjecting.Create(
  const ALanguageManager: ILanguageManager;
  const AGeometryLonLatFactory: IGeometryLonLatFactory;
  const AMarkDBGUI: TMarkDbGUIHelper;
  const APosition: ILocalCoordConverterChangeable;
  const AGeoCalc: IGeoCalcChangeable
);
begin
  Assert(Assigned(AGeometryLonLatFactory));
  Assert(Assigned(AMarkDBGUI));
  Assert(Assigned(APosition));
  inherited Create(ALanguageManager);
  FGeometryLonLatFactory := AGeometryLonLatFactory;
  FMarkDBGUI := AMarkDBGUI;
  FPosition := APosition;
  FGeoCalc := AGeoCalc;
end;

procedure MarksListToStrings(
  const AList: IInterfaceListStatic;
  AStrings: TStrings
);
var
  I: Integer;
  VMarkId: IMarkId;
begin
  AStrings.BeginUpdate;
  try
    AStrings.Clear;
    if Assigned(AList) then begin
      for I := 0 to AList.Count - 1 do begin
        VMarkId := IMarkId(AList[I]);
        AStrings.AddObject(VMarkId.Name, Pointer(VMarkId));
      end;
    end;
  finally
    AStrings.EndUpdate;
  end;
end;

procedure TfrmPointProjecting.btnCreatePointClick(Sender: TObject);
var
  VDistText: string;
  VAzimuthText: string;
  VSourceLonLat: TDoublePoint;
  VIndex: Integer;
  VMarkId: IMarkId;
  VMark: IVectorDataItem;
  VDist: Double;
  VAzimuth: Double;
  VLonLat: TDoublePoint;
  VPoint: IGeometryLonLatPoint;
begin
  VDistText := edtDist.Text;
  if VDistText = '' then begin
    Exit;
  end;
  VAzimuthText := edtAzimuth.Text;
  if VAzimuthText = '' then begin
    Exit;
  end;
  VDist := str2r(VDistText);
  VAzimuth := str2r(VAzimuthText);
  if VDist <= 0 then begin
    Exit;
  end;

  if rgSourcePointType.ItemIndex = 0 then begin
    VSourceLonLat := FPosition.GetStatic.GetCenterLonLat;
  end else begin
    VIndex := cbbAllMarks.ItemIndex;
    if VIndex >= 0 then begin
      VMarkId := IMarkId(Pointer(cbbAllMarks.Items.Objects[VIndex]));
      VMark := FMarkDBGUI.MarksDb.MarkDb.GetMarkByID(VMarkId);
      if Assigned(VMark) then begin
        VSourceLonLat := VMark.Geometry.GetGoToPoint;
      end else begin
        Exit;
      end;
    end else begin
      Exit;
    end;
  end;

  VLonLat := FGeoCalc.Datum.CalcFinishPosition(VSourceLonLat, VAzimuth, VDist);
  VPoint := FGeometryLonLatFactory.CreateLonLatPoint(VLonLat);
  FMarkDBGUI.SaveMarkModal(nil, VPoint);
end;

procedure TfrmPointProjecting.cbbAllMarksDropDown(Sender: TObject);
begin
  if cbbAllMarks.Items.Count = 0 then begin
    FMarksList := FMarkDBGUI.MarksDb.MarkDb.GetAllMarkIdList;
    MarksListToStrings(FMarksList, cbbAllMarks.Items);
  end;
end;

procedure TfrmPointProjecting.FormHide(Sender: TObject);
begin
  cbbAllMarks.Clear;
  FMarksList := nil;
end;

procedure TfrmPointProjecting.rgSourcePointTypeClick(Sender: TObject);
begin
  cbbAllMarks.Enabled := rgSourcePointType.ItemIndex > 0;
  if not cbbAllMarks.Enabled then begin
    cbbAllMarks.Clear;
    FMarksList := nil;
  end;
end;

end.
