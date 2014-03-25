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

unit u_SensorViewListGeneratorStuped;

interface

uses
  Classes,
  ImgList,
  TB2Item,
  TB2Dock,
  i_NotifierTime,
  i_GUIDSet,
  i_ValueToStringConverter,
  i_LanguageManager,
  i_SensorList,
  i_SensorViewListGenerator,
  u_BaseInterfacedObject;

type
  TSensorViewListGeneratorStuped = class(TBaseInterfacedObject, ISensorViewListGenerator)
  private
    FTimerNoifier: INotifierTime;
    FValueConverterConfig: IValueToStringConverterConfig;
    FLanguageManager: ILanguageManager;
    FOwner: TComponent;
    FDefaultDoc: TTBDock;
    FParentMenu: TTBCustomItem;
    FImages: TCustomImageList;
    FImageIndexReset: TImageIndex;
    procedure AddSensor(
      const ASensor: ISensorListEntity;
      const AResult: IGUIDInterfaceSet
    );
    procedure AddSensorsInFixedOrder(
      const ASensorList: ISensorList;
      const AResult: IGUIDInterfaceSet
    );
  private
    function CreateSensorViewList(const ASensorList: ISensorList): IGUIDInterfaceSet;
  public
    constructor Create(
      const ATimerNoifier: INotifierTime;
      const AValueConverterConfig: IValueToStringConverterConfig;
      const ALanguageManager: ILanguageManager;
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
  u_GUIDInterfaceSet,
  c_SensorsGUIDSimple,
  i_Sensor,
  u_SatellitesInViewMapDrawSimple,
  u_SensorViewTextTBXPanel,
  u_SensorViewConfigSimple;

{ TSensorViewListGeneratorStuped }

procedure TSensorViewListGeneratorStuped.AddSensor(
  const ASensor: ISensorListEntity;
  const AResult: IGUIDInterfaceSet
);
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
    end else if IsEqualGUID(ASensor.GetSensorTypeIID, ISensorSpeed) then begin
      VGUID := ASensor.GUID;
      if not AResult.IsExists(VGUID) then begin
        VSensorViewConfig := TSensorViewConfigSimple.Create;
        VSensorView :=
          TSensorViewSpeedTBXPanel.Create(
            ASensor,
            VSensorViewConfig,
            FTimerNoifier,
            FValueConverterConfig,
            FOwner,
            FDefaultDoc,
            FParentMenu,
            FImages,
            FImageIndexReset
          );
        AResult.Add(VGUID, VSensorView);
      end;
    end else if IsEqualGUID(ASensor.GetSensorTypeIID, ISensorDistance) then begin
      VGUID := ASensor.GUID;
      if not AResult.IsExists(VGUID) then begin
        VSensorViewConfig := TSensorViewConfigSimple.Create;
        VSensorView :=
          TSensorViewLengthTBXPanel.Create(
            ASensor,
            VSensorViewConfig,
            FTimerNoifier,
            FValueConverterConfig,
            FOwner,
            FDefaultDoc,
            FParentMenu,
            FImages,
            FImageIndexReset
          );
        AResult.Add(VGUID, VSensorView);
      end;
    end else if IsEqualGUID(ASensor.GetSensorTypeIID, ISensorBatteryLifePercent) then begin
      VGUID := ASensor.GUID;
      if not AResult.IsExists(VGUID) then begin
        VSensorViewConfig := TSensorViewConfigSimple.Create;
        VSensorView :=
          TSensorViewBatteryLifePercentTBXPanel.Create(
            ASensor,
            VSensorViewConfig,
            FTimerNoifier,
            FLanguageManager,
            FOwner,
            FDefaultDoc,
            FParentMenu,
            FImages,
            FImageIndexReset
          );
        AResult.Add(VGUID, VSensorView);
      end;
    end else if IsEqualGUID(ASensor.GetSensorTypeIID, ISensorDegrees) then begin
      VGUID := ASensor.GUID;
      if not AResult.IsExists(VGUID) then begin
        VSensorViewConfig := TSensorViewConfigSimple.Create;
        VSensorView :=
          TSensorViewDegreesTBXPanel.Create(
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
    end else if IsEqualGUID(ASensor.GetSensorTypeIID, ISensorDouble) then begin
      VGUID := ASensor.GUID;
      if not AResult.IsExists(VGUID) then begin
        VSensorViewConfig := TSensorViewConfigSimple.Create;
        VSensorView :=
          TSensorViewDoubleTBXPanel.Create(
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
    end else if IsEqualGUID(ASensor.GetSensorTypeIID, ISensorTime) then begin
      VGUID := ASensor.GUID;
      if not AResult.IsExists(VGUID) then begin
        VSensorViewConfig := TSensorViewConfigSimple.Create;
        VSensorView :=
          TSensorViewTimeTBXPanel.Create(
            ASensor,
            VSensorViewConfig,
            FTimerNoifier,
            FValueConverterConfig,
            FOwner,
            FDefaultDoc,
            FParentMenu,
            FImages,
            FImageIndexReset
          );
        AResult.Add(VGUID, VSensorView);
      end;
    end else if IsEqualGUID(ASensor.GetSensorTypeIID, ISensorPosition) then begin
      VGUID := ASensor.GUID;
      if not AResult.IsExists(VGUID) then begin
        VSensorViewConfig := TSensorViewConfigSimple.Create;
        VSensorView :=
          TSensorViewPositionTBXPanel.Create(
            ASensor,
            VSensorViewConfig,
            FTimerNoifier,
            FValueConverterConfig,
            FOwner,
            FDefaultDoc,
            FParentMenu,
            FImages,
            FImageIndexReset
          );
        AResult.Add(VGUID, VSensorView);
      end;
    end else if IsEqualGUID(ASensor.GetSensorTypeIID, ISensorGPSSatellites) then begin
      VGUID := ASensor.GUID;
      if not AResult.IsExists(VGUID) then begin
        VSensorViewConfig := TSensorViewConfigSimple.Create;
        VSensorView :=
          TSensorViewGPSSatellitesTBXPanel.Create(
            ASensor,
            VSensorViewConfig,
            FTimerNoifier,
            TSatellitesInViewMapDrawSimple.Create,
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
  const ASensorList: ISensorList;
  const AResult: IGUIDInterfaceSet
);
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

  VSensor := ASensorList.Get(CSensorHDOPGUID);
  AddSensor(VSensor, AResult);
  VSensor := ASensorList.Get(CSensorVDOPGUID);
  AddSensor(VSensor, AResult);
  VSensor := ASensorList.Get(CSensorUTCTimeGUID);
  AddSensor(VSensor, AResult);
  VSensor := ASensorList.Get(CSensorLocalTimeGUID);
  AddSensor(VSensor, AResult);
  VSensor := ASensorList.Get(CSensorDGPSGUID);
  AddSensor(VSensor, AResult);
  VSensor := ASensorList.Get(CSensorGPSUnitInfoGUID);
  AddSensor(VSensor, AResult);
end;

constructor TSensorViewListGeneratorStuped.Create(
  const ATimerNoifier: INotifierTime;
  const AValueConverterConfig: IValueToStringConverterConfig;
  const ALanguageManager: ILanguageManager;
  AOwner: TComponent;
  ADefaultDoc: TTBDock;
  AParentMenu: TTBCustomItem;
  AImages: TCustomImageList;
  AImageIndexReset: TImageIndex
);
begin
  inherited Create;
  FTimerNoifier := ATimerNoifier;
  FValueConverterConfig := AValueConverterConfig;
  FLanguageManager := ALanguageManager;
  FOwner := AOwner;
  FDefaultDoc := ADefaultDoc;
  FParentMenu := AParentMenu;
  FImages := AImages;
  FImageIndexReset := AImageIndexReset;
end;

function TSensorViewListGeneratorStuped.CreateSensorViewList(
  const ASensorList: ISensorList
): IGUIDInterfaceSet;
var
  VGUID: TGUID;
  i: Cardinal;
  VSensor: ISensorListEntity;
  VEnum: IEnumGUID;
begin
  FDefaultDoc.BeginUpdate;
  try
    Result := TGUIDInterfaceSet.Create;
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
