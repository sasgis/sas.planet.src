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

unit frm_GoTo;

interface

uses
  Windows,
  SysUtils,
  Forms,
  Classes,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  Controls,
  t_GeoTypes,
  i_LanguageManager,
  i_InterfaceListStatic,
  i_MarkDb,
  i_MainGeoCoderConfig,
  i_LocalCoordConverterChangeable,
  i_VectorDataItemSimple,
  i_VectorItemSubsetBuilder,
  i_ValueToStringConverter,
  i_GeoCoder,
  u_CommonFormAndFrameParents,
  fr_LonLat;

type

  TfrmGoTo = class(TFormWitghLanguageManager)
    btnGoTo: TButton;
    btnCancel: TButton;
    pnlBottomButtons: TPanel;
    cbbGeoCode: TComboBox;
    pgcSearchType: TPageControl;
    tsPlaceMarks: TTabSheet;
    tsSearch: TTabSheet;
    tsCoordinates: TTabSheet;
    cbbAllMarks: TComboBox;
    cbbSearcherType: TComboBox;
    procedure btnGoToClick(Sender: TObject);
    procedure cbbAllMarksDropDown(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FMarksDb: IMarkDb;
    FMainGeoCoderConfig: IMainGeoCoderConfig;
    FGeoCodePlacemarkFactory:IGeoCodePlacemarkFactory;
    FViewPortState: ILocalCoordConverterChangeable;
    FValueToStringConverter: IValueToStringConverterChangeable;
    FResult: IGeoCodeResult;
    frLonLatPoint: TfrLonLat;
    FMarksList: IInterfaceListStatic;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    function GeocodeResultFromVectorItem(
      const ASearch: WideString;
      const AItem: IVectorDataItemSimple
    ): IGeoCodeResult;
    function GeocodeResultFromLonLat(
      const ASearch: WideString;
      const ALonLat: TDoublePoint;
      const AMessage: WideString
    ): IGeoCodeResult;
    procedure InitHistory;
    procedure InitGeoCoders;
    procedure EmptyGeoCoders;
    procedure MarksListToStrings(
      const AList: IInterfaceListStatic;
      AStrings: TStrings
    );
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AGeoCodePlacemarkFactory:IGeoCodePlacemarkFactory;
      const AMarksDb: IMarkDb;
      const AMainGeoCoderConfig: IMainGeoCoderConfig;
      const AViewPortState: ILocalCoordConverterChangeable;
      const AValueToStringConverter: IValueToStringConverterChangeable
    ); reintroduce;
    destructor Destroy; override;
    function ShowGeocodeModal(): IGeoCodeResult;
  end;

implementation

uses
  ActiveX,
  i_GeoCoderList,
  i_MarkId,
  i_LocalCoordConverter,
  i_NotifierOperation,
  u_Notifier,
  u_NotifierOperation,
  u_GeoCodeResult;

{$R *.dfm}

function TfrmGoTo.GeocodeResultFromLonLat(
  const ASearch: WideString;
  const ALonLat: TDoublePoint;
  const AMessage: WideString
): IGeoCodeResult;
var
  VPlace: IVectorDataItemPoint;
  VSubsetBuilder: IVectorItemSubsetBuilder;
begin
  VPlace := FGeoCodePlacemarkFactory.Build(ALonLat, AMessage, '', '', 4);
  VSubsetBuilder := FVectorItemSubsetBuilderFactory.Build;
  VSubsetBuilder.Add(VPlace);
  Result := TGeoCodeResult.Create(ASearch, 203, '', VSubsetBuilder.MakeStaticAndClear);
end;

function TfrmGoTo.GeocodeResultFromVectorItem(
  const ASearch: WideString;
  const AItem: IVectorDataItemSimple
): IGeoCodeResult;
var
  VSubsetBuilder: IVectorItemSubsetBuilder;
begin
  VSubsetBuilder := FVectorItemSubsetBuilderFactory.Build;
  VSubsetBuilder.Add(AItem);
  Result := TGeoCodeResult.Create(ASearch, 203, '', VSubsetBuilder.MakeStaticAndClear);
end;

procedure TfrmGoTo.InitGeoCoders;
var
  VEnum: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
  VItem: IGeoCoderListEntity;
  VIndex: Integer;
  VActiveGUID: TGUID;
  VActiveIndex: Integer;
begin
  VEnum := FMainGeoCoderConfig.GetList.GetGUIDEnum;
  VActiveGUID := FMainGeoCoderConfig.ActiveGeoCoderGUID;
  VActiveIndex := -1;
  while VEnum.Next(1, VGUID, i) = S_OK do begin
    VItem := FMainGeoCoderConfig.GetList.Get(VGUID);
    VItem._AddRef;
    VIndex := cbbSearcherType.Items.AddObject(VItem.GetCaption, Pointer(VItem));
    if IsEqualGUID(VGUID, VActiveGUID) then begin
      VActiveIndex := VIndex;
    end;
  end;
  if VActiveIndex < 0 then begin
    VActiveIndex := 0;
  end;
  cbbSearcherType.ItemIndex := VActiveIndex;
end;

procedure TfrmGoTo.InitHistory;
var
  i: Integer;
begin
  with FMainGeoCoderConfig.SearchHistory do begin
    LockRead;
    try
      if (Count>0) then begin
        cbbGeoCode.Items.BeginUpdate;
        try
          for i := 0 to Count-1 do begin
            cbbGeoCode.Items.Add(GetItem(i));
          end;
        finally
          cbbGeoCode.Items.EndUpdate;
        end;
      end;
    finally
      UnlockRead;
    end;
  end;
end;

procedure TfrmGoTo.MarksListToStrings(
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

procedure TfrmGoTo.btnGoToClick(Sender: TObject);
var
  textsrch:String;
  VIndex: Integer;
  VMarkId: IMarkId;
  VMark: IVectorDataItemSimple;
  VLonLat: TDoublePoint;
  VGeoCoderItem: IGeoCoderListEntity;
  VLocalConverter: ILocalCoordConverter;
  VNotifier: INotifierOperation;
begin
  VLocalConverter := FViewPortState.GetStatic;
  if pgcSearchType.ActivePage = tsPlaceMarks then begin
    VIndex := cbbAllMarks.ItemIndex;
    if VIndex >= 0 then begin
      VMarkId := IMarkId(Pointer(cbbAllMarks.Items.Objects[VIndex]));
      VMark := FMarksDb.GetMarkByID(VMarkId);
      if Assigned(VMark) then begin
        FResult := GeocodeResultFromVectorItem(cbbAllMarks.Text, VMark);
        ModalResult := mrOk;
      end else begin
        ModalResult := mrCancel;
      end;
    end else begin
      ModalResult := mrCancel;
    end;
  end else if pgcSearchType.ActivePage = tsCoordinates then begin
    if frLonLatPoint.Validate then begin
      VLonLat := frLonLatPoint.LonLat;
      textsrch := FValueToStringConverter.GetStatic.LonLatConvert(VLonLat);
      FResult := GeocodeResultFromLonLat(textsrch, VLonLat, textsrch);
      ModalResult := mrOk;
    end;
  end else if pgcSearchType.ActivePage = tsSearch then begin
    textsrch:= Trim(cbbGeoCode.Text);
    VGeoCoderItem := nil;
    VIndex := cbbSearcherType.ItemIndex;
    if VIndex >= 0 then begin
      VGeoCoderItem := IGeoCoderListEntity(Pointer(cbbSearcherType.Items.Objects[VIndex]));
    end;
    if VGeoCoderItem <> nil then begin
      VNotifier := TNotifierOperation.Create(TNotifierBase.Create);
      FResult := VGeoCoderItem.GetGeoCoder.GetLocations(VNotifier, VNotifier.CurrentOperation, textsrch, VLocalConverter);
      FMainGeoCoderConfig.SearchHistory.AddItem(textsrch);
      FMainGeoCoderConfig.ActiveGeoCoderGUID := VGeoCoderItem.GetGUID;
      ModalResult := mrOk;
    end else begin
      ModalResult := mrCancel;
    end;
  end;
end;

function TfrmGoTo.ShowGeocodeModal: IGeoCodeResult;
var
  VLocalConverter: ILocalCoordConverter;
begin
  frLonLatPoint.Parent := tsCoordinates;
  VLocalConverter := FViewPortState.GetStatic;
  frLonLatPoint.LonLat := VLocalConverter.GetCenterLonLat;
  InitGeoCoders;
  InitHistory;
  try
    if ShowModal = mrOk then begin
      Result := FResult;
    end else begin
      Result := nil;
    end;
  finally
    EmptyGeoCoders;
  end;
  cbbAllMarks.Clear;
  FMarksList:=nil;
  cbbGeoCode.Clear;
end;

procedure TfrmGoTo.cbbAllMarksDropDown(Sender: TObject);
begin
  if cbbAllMarks.Items.Count=0 then begin
    FMarksList := FMarksDb.GetAllMarkIdList;
    MarksListToStrings(FMarksList, cbbAllMarks.Items);
  end;
end;

constructor TfrmGoTo.Create(
  const ALanguageManager: ILanguageManager;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AGeoCodePlacemarkFactory:IGeoCodePlacemarkFactory;
  const AMarksDb: IMarkDb;
  const AMainGeoCoderConfig: IMainGeoCoderConfig;
  const AViewPortState: ILocalCoordConverterChangeable;
  const AValueToStringConverter: IValueToStringConverterChangeable
);
begin
  inherited Create(ALanguageManager);
  FMarksDb := AMarksDb;
  FGeoCodePlacemarkFactory := AGeoCodePlacemarkFactory;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FMainGeoCoderConfig := AMainGeoCoderConfig;
  FViewPortState := AViewPortState;
  FValueToStringConverter := AValueToStringConverter;
  frLonLatPoint := TfrLonLat.Create(ALanguageManager, FViewPortState, FValueToStringConverter, tssCenter);
  frLonLatPoint.Width:= tsCoordinates.Width;
  frLonLatPoint.Height:= tsCoordinates.Height;
end;

destructor TfrmGoTo.Destroy;
begin
  FreeAndNil(frLonLatPoint);
  inherited;
end;

procedure TfrmGoTo.EmptyGeoCoders;
var
  VObj: IInterface;
  i: Integer;
begin
  for i := 0 to cbbSearcherType.Items.Count - 1 do begin
    VObj := IInterface(Pointer(cbbSearcherType.Items.Objects[i]));
    VObj._Release;
  end;
  cbbSearcherType.Clear;
end;

procedure TfrmGoTo.FormShow(Sender: TObject);
begin
  if pgcSearchType.ActivePage = tsPlaceMarks then begin
    cbbAllMarks.SetFocus;
  end else if pgcSearchType.ActivePage = tsCoordinates then begin
    frLonLatPoint.SetFocus;
  end else if pgcSearchType.ActivePage = tsSearch then begin
    cbbGeoCode.SetFocus;
  end;
end;

end.
