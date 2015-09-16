unit frm_PointProjecting;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
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
    FMarkDBGUI: TMarkDbGUIHelper;
    FPosition: ILocalCoordConverterChangeable;
    FGeometryLonLatFactory: IGeometryLonLatFactory;
    FMarksList: IInterfaceListStatic;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AGeometryLonLatFactory: IGeometryLonLatFactory;
      const AMarkDBGUI: TMarkDbGUIHelper;
      const APosition: ILocalCoordConverterChangeable
    ); reintroduce;
  end;

implementation

uses
  t_GeoTypes,
  i_MarkId,
  i_Datum,
  i_GeometryLonLat,
  i_VectorDataItemSimple,
  u_GeoToStrFunc;

{$R *.dfm}

constructor TfrmPointProjecting.Create(
  const ALanguageManager: ILanguageManager;
  const AGeometryLonLatFactory: IGeometryLonLatFactory;
  const AMarkDBGUI: TMarkDbGUIHelper;
  const APosition: ILocalCoordConverterChangeable
);
begin
  Assert(Assigned(AGeometryLonLatFactory));
  Assert(Assigned(AMarkDBGUI));
  Assert(Assigned(APosition));
  inherited Create(ALanguageManager);
  FGeometryLonLatFactory := AGeometryLonLatFactory;
  FMarkDBGUI := AMarkDBGUI;
  FPosition := APosition;
end;

procedure MarksListToStrings(
  const AList: IInterfaceListStatic;
  AStrings: TStrings
);
var
  i: Integer;
  VMarkId: IMarkId;
begin
  AStrings.BeginUpdate;
  try
    AStrings.Clear;
    for i := 0 to AList.Count - 1 do begin
      VMarkId := IMarkId(AList[i]);
      AStrings.AddObject(VMarkId.Name, Pointer(VMarkId));
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
  VDatum: IDatum;
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

  VDatum := FPosition.GetStatic.Projection.ProjectionType.Datum;
  VLonLat := VDatum.CalcFinishPosition(VSourceLonLat, VAzimuth, VDist * 1000);
  VPoint := FGeometryLonLatFactory.CreateLonLatPoint(VLonLat);
  FMarkDBGUI.SaveMarkModal(nil, VPoint);
end;

procedure TfrmPointProjecting.cbbAllMarksDropDown(Sender: TObject);
begin
  if cbbAllMarks.Items.Count=0 then begin
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
