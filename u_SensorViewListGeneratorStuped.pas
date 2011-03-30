unit u_SensorViewListGeneratorStuped;

interface

uses
  TB2Item,
  TB2Dock,
  i_JclNotify,
  i_GUIDList,
  i_SensorViewListGenerator;

type
  TSensorViewListGeneratorStuped = class(TInterfacedObject, ISensorViewListGenerator)
  private
    FTimerNoifier: IJclNotifier;
    FDefaultDoc: TTBDock;
    FParentMenu: TTBCustomItem;
  protected
    function CreateSensorViewList(ASensorList: IGUIDInterfaceList): IGUIDInterfaceList;
  public
    constructor Create(
      ATimerNoifier: IJclNotifier;
      ADefaultDoc: TTBDock;
      AParentMenu: TTBCustomItem
    );
  end;

implementation

uses
  ActiveX,
  SysUtils,
  u_GUIDInterfaceList,
  i_Sensor,
  u_SensorViewTextTBXPanel,
  u_SensorViewConfigSimple;

{ TSensorViewListGeneratorStuped }

constructor TSensorViewListGeneratorStuped.Create(ATimerNoifier: IJclNotifier;
  ADefaultDoc: TTBDock; AParentMenu: TTBCustomItem);
begin
  FTimerNoifier := ATimerNoifier;
  FDefaultDoc := ADefaultDoc;
  FParentMenu := AParentMenu;
end;

function TSensorViewListGeneratorStuped.CreateSensorViewList(
  ASensorList: IGUIDInterfaceList): IGUIDInterfaceList;
var
  VEnumGUID: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
  VSensor: ISensor;
  VSensorText: ISensorText;
  VSensorViewConfig: ISensorViewConfig;
  VSensorView: ISensorView;
begin
  Result := TGUIDInterfaceList.Create;

  VEnumGUID := ASensorList.GetGUIDEnum;
  while VEnumGUID.Next(1, VGUID, i) = S_OK do begin
    VSensor := ISensor(ASensorList.GetByGUID(VGUID));
    if IsEqualGUID(VSensor.GetSensorTypeIID, ISensorText) then begin
      VSensorText := VSensor as ISensorText;
      VSensorViewConfig := TSensorViewConfigSimple.Create;
      VSensorView := TSensorViewTextTBXPanel.Create(VSensorText, VSensorViewConfig, FTimerNoifier, FDefaultDoc, FParentMenu);
      Result.Add(VGUID, VSensorView);
    end;
  end;
end;

end.
