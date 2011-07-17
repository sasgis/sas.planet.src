unit u_SensorViewListGeneratorStuped;

interface

uses
  Classes,
  ImgList,
  TB2Item,
  TB2Dock,
  i_JclNotify,
  i_GUIDList,
  i_SensorList,
  i_SensorViewListGenerator;

type
  TSensorViewListGeneratorStuped = class(TInterfacedObject, ISensorViewListGenerator)
  private
    FTimerNoifier: IJclNotifier;
    FOwner: TComponent;
    FDefaultDoc: TTBDock;
    FParentMenu: TTBCustomItem;
    FImages: TCustomImageList;
    FImageIndexReset: TImageIndex;
    procedure AddSensor(ASensor: ISensorListEntity; AResult: IGUIDInterfaceList);
    procedure AddSensorsInFixedOrder(ASensorList: ISensorList; AResult: IGUIDInterfaceList);
  protected
    function CreateSensorViewList(ASensorList: ISensorList): IGUIDInterfaceList;
  public
    constructor Create(
      ATimerNoifier: IJclNotifier;
      AOwner: TComponent;
      ADefaultDoc: TTBDock;
      AParentMenu: TTBCustomItem;
      AImages: TCustomImageList;
      AImageIndexReset: TImageIndex
    );
  end;

implementation

uses
  ActiveX,
  SysUtils,
  u_GUIDInterfaceList,
  c_SensorsGUIDSimple,
  i_Sensor,
  u_SensorViewTextTBXPanel,
  u_SensorViewConfigSimple;

{ TSensorViewListGeneratorStuped }

procedure TSensorViewListGeneratorStuped.AddSensor(ASensor: ISensorListEntity;
  AResult: IGUIDInterfaceList);
var
  VSensorViewConfig: ISensorViewConfig;
  VSensorView: ISensorView;
  VGUID: TGUID;
begin
  if ASensor <> nil then begin
    if IsEqualGUID(ASensor.GetSensorTypeIID, ISensorText) then begin
      VGUID := ASensor.GUID;
      if not AResult.IsExists(VGUID) then begin
        VSensorViewConfig := TSensorViewConfigSimple.Create;
        VSensorView :=
          TSensorViewTextTBXPanel.Create(
            ASensor,
            VSensorViewConfig,
            FTimerNoifier,
            FOwner,
            FDefaultDoc,
            FParentMenu,
            FImages,
            FImageIndexReset
          );
        AResult.Add(VGUID, VSensorView);
      end;
    end;
  end;
end;

procedure TSensorViewListGeneratorStuped.AddSensorsInFixedOrder(
  ASensorList: ISensorList; AResult: IGUIDInterfaceList);
var
  VSensor: ISensorListEntity;
begin
  VSensor := ASensorList.Get(CSensorLastSpeedGUID);
  AddSensor(VSensor, AResult);
  VSensor := ASensorList.Get(CSensorAvgSpeedGUID);
  AddSensor(VSensor, AResult);
  VSensor := ASensorList.Get(CSensorMaxSpeedGUID);
  AddSensor(VSensor, AResult);
  VSensor := ASensorList.Get(CSensorDistGUID);
  AddSensor(VSensor, AResult);
  VSensor := ASensorList.Get(CSensorOdometer1GUID);
  AddSensor(VSensor, AResult);
  VSensor := ASensorList.Get(CSensorOdometer2GUID);
  AddSensor(VSensor, AResult);
  VSensor := ASensorList.Get(CSensorDistToMarkGUID);
  AddSensor(VSensor, AResult);
  VSensor := ASensorList.Get(CSensorLastAltitudeGUID);
  AddSensor(VSensor, AResult);
  VSensor := ASensorList.Get(CSensorBatteryGUID);
  AddSensor(VSensor, AResult);
  VSensor := ASensorList.Get(CSensorHeadingGUID);
  AddSensor(VSensor, AResult);
end;

constructor TSensorViewListGeneratorStuped.Create(
  ATimerNoifier: IJclNotifier;
  AOwner: TComponent;
  ADefaultDoc: TTBDock;
  AParentMenu: TTBCustomItem;
  AImages: TCustomImageList;
  AImageIndexReset: TImageIndex
);
begin
  FTimerNoifier := ATimerNoifier;
  FOwner := AOwner;
  FDefaultDoc := ADefaultDoc;
  FParentMenu := AParentMenu;
  FImages := AImages;
  FImageIndexReset := AImageIndexReset;
end;

function TSensorViewListGeneratorStuped.CreateSensorViewList(
  ASensorList: ISensorList): IGUIDInterfaceList;
var
  VGUID: TGUID;
  i: Cardinal;
  VSensor: ISensorListEntity;
  VEnum: IEnumGUID;
begin
  FDefaultDoc.BeginUpdate;
  try
    Result := TGUIDInterfaceList.Create;
    ASensorList.LockRead;
    try
      AddSensorsInFixedOrder(ASensorList, Result);
      VEnum := ASensorList.GetGUIDEnum;
      while VEnum.Next(1, VGUID, i) = S_OK do begin
        VSensor := ASensorList.Get(VGUID);
        AddSensor(VSensor, Result);
      end;
    finally
      ASensorList.UnlockRead;
    end;
  finally
    FDefaultDoc.EndUpdate;
    FDefaultDoc.CommitNewPositions := True;
    FDefaultDoc.ArrangeToolbars;
  end;
end;

end.
