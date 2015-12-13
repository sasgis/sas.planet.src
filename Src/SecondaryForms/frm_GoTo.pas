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
  SysUtils,
  Forms,
  Classes,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  Controls,
  t_GeoTypes,
  TB2Item,
  TB2Dock,
  TB2Toolbar,
  TBX,
  i_LanguageManager,
  i_InterfaceListStatic,
  i_MarkDb,
  i_StringHistory,
  i_GeoCoderList,
  i_MainGeoCoderConfig,
  i_ProjectionSetChangeable,
  i_LocalCoordConverterChangeable,
  i_VectorDataItemSimple,
  i_VectorItemSubsetBuilder,
  i_CoordToStringConverter,
  i_SearchResultPresenter,
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
    TBXDontClose: TTBXToolbar;
    tbtmDontClose: TTBItem;
    procedure btnGoToClick(Sender: TObject);
    procedure cbbAllMarksDropDown(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FMarksDb: IMarkDb;
    FMainGeoCoderConfig: IMainGeoCoderConfig;
    FSearchHistory: IStringHistory;
    FGeoCoderList: IGeoCoderListStatic;
    FGeoCodePlacemarkFactory: IGeoCodePlacemarkFactory;
    FViewPortState: ILocalCoordConverterChangeable;
    FCoordToStringConverter: ICoordToStringConverterChangeable;
    FResult: IGeoCodeResult;
    frLonLatPoint: TfrLonLat;
    FMarksList: IInterfaceListStatic;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FSearchPresenter: ISearchResultPresenter;
    function GeocodeResultFromVectorItem(
      const ASearch: string;
      const AItem: IVectorDataItem
    ): IGeoCodeResult;
    function GeocodeResultFromLonLat(
      const ASearch: string;
      const ALonLat: TDoublePoint;
      const AMessage: string
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
      const AProjectionSet: IProjectionSetChangeable;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AGeoCodePlacemarkFactory: IGeoCodePlacemarkFactory;
      const AMarksDb: IMarkDb;
      const AGeoCoderList: IGeoCoderListStatic;
      const AFSearchHistory: IStringHistory;
      const AMainGeoCoderConfig: IMainGeoCoderConfig;
      const AViewPortState: ILocalCoordConverterChangeable;
      const ACoordToStringConverter: ICoordToStringConverterChangeable;
      const ASearchPresenter: ISearchResultPresenter
    ); reintroduce;
    destructor Destroy; override;
    procedure ShowGotoDialog();
  end;

implementation

uses
  ActiveX,
  i_MarkId,
  i_LocalCoordConverter,
  i_NotifierOperation,
  u_NotifierOperation,
  u_GeoCodeResult;

{$R *.dfm}

function TfrmGoTo.GeocodeResultFromLonLat(
  const ASearch: string;
  const ALonLat: TDoublePoint;
  const AMessage: string
): IGeoCodeResult;
var
  VPlace: IVectorDataItem;
  VSubsetBuilder: IVectorItemSubsetBuilder;
begin
  VPlace := FGeoCodePlacemarkFactory.Build(ALonLat, AMessage, '', '', 4);
  VSubsetBuilder := FVectorItemSubsetBuilderFactory.Build;
  VSubsetBuilder.Add(VPlace);
  Result := TGeoCodeResult.Create(ASearch, 203, '', VSubsetBuilder.MakeStaticAndClear);
end;

function TfrmGoTo.GeocodeResultFromVectorItem(
  const ASearch: string;
  const AItem: IVectorDataItem
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
  i: Integer;
  VItem: IGeoCoderListEntity;
  VIndex: Integer;
  VActiveGUID: TGUID;
  VActiveIndex: Integer;
begin
  VActiveGUID := FMainGeoCoderConfig.ActiveGeoCoderGUID;
  VActiveIndex := -1;

  cbbSearcherType.Items.BeginUpdate;
  try
    for i := 0 to FGeoCoderList.Count - 1 do begin
      VItem := FGeoCoderList.Items[i];
      VItem._AddRef;
      VIndex := cbbSearcherType.Items.AddObject(VItem.GetCaption, Pointer(VItem));
      if IsEqualGUID(VItem.GUID, VActiveGUID) then begin
        VActiveIndex := VIndex;
      end;
    end;
    if VActiveIndex < 0 then begin
      VActiveIndex := 0;
    end;
    cbbSearcherType.ItemIndex := VActiveIndex;
  finally
    cbbSearcherType.Items.EndUpdate;
  end;
end;

procedure TfrmGoTo.InitHistory;
var
  i: Integer;
begin
  FSearchHistory.LockRead;
  try
    if (FSearchHistory.Count > 0) then begin
      cbbGeoCode.Items.BeginUpdate;
      try
        for i := 0 to FSearchHistory.Count - 1 do begin
          cbbGeoCode.Items.Add(FSearchHistory.GetItem(i));
        end;
      finally
        cbbGeoCode.Items.EndUpdate;
      end;
    end;
  finally
    FSearchHistory.UnlockRead;
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
    if Assigned(AList) then begin
      for i := 0 to AList.Count - 1 do begin
        VMarkId := IMarkId(AList[i]);
        AStrings.AddObject(VMarkId.Name, Pointer(VMarkId));
      end;
    end;
  finally
    AStrings.EndUpdate;
  end;
end;

procedure TfrmGoTo.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmGoTo.btnGoToClick(Sender: TObject);
var
  textsrch: String;
  VIndex: Integer;
  VMarkId: IMarkId;
  VMark: IVectorDataItem;
  VLonLat: TDoublePoint;
  VGeoCoderItem: IGeoCoderListEntity;
  VLocalConverter: ILocalCoordConverter;
  VNotifier: INotifierOperation;
begin
  try
    VLocalConverter := FViewPortState.GetStatic;
    if pgcSearchType.ActivePage = tsPlaceMarks then begin
      VIndex := cbbAllMarks.ItemIndex;
      if VIndex >= 0 then begin
        VMarkId := IMarkId(Pointer(cbbAllMarks.Items.Objects[VIndex]));
        VMark := FMarksDb.GetMarkByID(VMarkId);
        if Assigned(VMark) then begin
          FResult := GeocodeResultFromVectorItem(cbbAllMarks.Text, VMark);
        end;
      end;
    end else if pgcSearchType.ActivePage = tsCoordinates then begin
      if frLonLatPoint.Validate then begin
        VLonLat := frLonLatPoint.LonLat;
        textsrch := FCoordToStringConverter.GetStatic.LonLatConvert(VLonLat);
        FResult := GeocodeResultFromLonLat(textsrch, VLonLat, textsrch);
      end;
    end else if pgcSearchType.ActivePage = tsSearch then begin
      textsrch := Trim(cbbGeoCode.Text);

      VGeoCoderItem := nil;
      VIndex := cbbSearcherType.ItemIndex;
      if VIndex >= 0 then begin
        VGeoCoderItem := IGeoCoderListEntity(Pointer(cbbSearcherType.Items.Objects[VIndex]));
      end;
      if VGeoCoderItem <> nil then begin
        VNotifier := TNotifierOperationFake.Create;
        FResult := VGeoCoderItem.GetGeoCoder.GetLocations(VNotifier, VNotifier.CurrentOperation, textsrch, VLocalConverter);
        FSearchHistory.AddItem(textsrch);
        FMainGeoCoderConfig.ActiveGeoCoderGUID := VGeoCoderItem.GetGUID;
      end;
    end;
  except
   FResult := nil;
  end;

  if Assigned(FResult) then begin
    FSearchPresenter.ShowSearchResults(FResult);
  end;
  if not tbtmDontClose.Checked then begin
    close;
  end;
end;

procedure TfrmGoTo.ShowGotoDialog();
var
  VLocalConverter: ILocalCoordConverter;
begin
  if not self.Visible then
  begin
    frLonLatPoint.Parent := tsCoordinates;
    VLocalConverter := FViewPortState.GetStatic;
    frLonLatPoint.LonLat := VLocalConverter.GetCenterLonLat;
    InitGeoCoders;
    InitHistory;
  end;
  Self.Show;
end;

procedure TfrmGoTo.cbbAllMarksDropDown(Sender: TObject);
begin
  if cbbAllMarks.Items.Count = 0 then begin
    FMarksList := FMarksDb.GetAllMarkIdList;
    MarksListToStrings(FMarksList, cbbAllMarks.Items);
  end;
end;

constructor TfrmGoTo.Create(
  const ALanguageManager: ILanguageManager;
  const AProjectionSet: IProjectionSetChangeable;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AGeoCodePlacemarkFactory: IGeoCodePlacemarkFactory;
  const AMarksDb: IMarkDb;
  const AGeoCoderList: IGeoCoderListStatic;
  const AFSearchHistory: IStringHistory;
  const AMainGeoCoderConfig: IMainGeoCoderConfig;
  const AViewPortState: ILocalCoordConverterChangeable;
  const ACoordToStringConverter: ICoordToStringConverterChangeable;
  const ASearchPresenter: ISearchResultPresenter
);
begin
  Assert(ALanguageManager <> nil);
  Assert(AProjectionSet <> nil);
  Assert(AVectorItemSubsetBuilderFactory <> nil);
  Assert(AGeoCodePlacemarkFactory <> nil);
  Assert(AMarksDb <> nil);
  Assert(AGeoCoderList <> nil);
  Assert(AFSearchHistory <> nil);
  Assert(AMainGeoCoderConfig <> nil);
  Assert(AViewPortState <> nil);
  Assert(ACoordToStringConverter <> nil);
  Assert(ASearchPresenter <> nil);

  inherited Create(ALanguageManager);
  FMarksDb := AMarksDb;
  FGeoCodePlacemarkFactory := AGeoCodePlacemarkFactory;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FGeoCoderList := AGeoCoderList;
  FSearchHistory := AFSearchHistory;
  FMainGeoCoderConfig := AMainGeoCoderConfig;
  FViewPortState := AViewPortState;
  FCoordToStringConverter := ACoordToStringConverter;
  FSearchPresenter := ASearchPresenter;
  frLonLatPoint :=
    TfrLonLat.Create(
      ALanguageManager,
      AProjectionSet,
      FViewPortState,
      FCoordToStringConverter,
      tssCenter
    );
  frLonLatPoint.Width := tsCoordinates.Width;
  frLonLatPoint.Height := tsCoordinates.Height;
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

procedure TfrmGoTo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  cbbAllMarks.Clear;
  cbbGeoCode.Clear;
  FMarksList := nil;
  EmptyGeoCoders;
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
