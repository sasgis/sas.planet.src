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
    FValueConverter: IValueToStringConverterChangeable;
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
      const AValueConverter: IValueToStringConverterChangeable;
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
  u_SensorViewTextTBXPanel;

{ TSensorViewListGeneratorStuped }

procedure TSensorViewListGeneratorStuped.AddSensor(
  const ASensor: ISensorListEntity;
  const AResult: IGUIDInterfaceSet
);
var
  VSensorView: IInterface;
  VGUID: TGUID;
begin
  if ASensor <> nil then begin
    if IsEqualGUID(ASensor.GetSensorTypeIID, ISensorText) then begin
      VGUID := ASensor.GUID;
      if not AResult.IsExists(VGUID) then begin
        VSensorView :=
          TSensorViewTextTBXPanel.Create(
            ASensor,
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
        VSensorView :=
          TSensorViewSpeedTBXPanel.Create(
            ASensor,
            FTimerNoifier,
            FValueConverter,
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
        VSensorView :=
          TSensorViewLengthTBXPanel.Create(
            ASensor,
            FTimerNoifier,
            FValueConverter,
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
        VSensorView :=
          TSensorViewBatteryLifePercentTBXPanel.Create(
            ASensor,
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
        VSensorView :=
          TSensorViewDegreesTBXPanel.Create(
            ASensor,
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
        VSensorView :=
          TSensorViewDoubleTBXPanel.Create(
            ASensor,
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
        VSensorView :=
          TSensorViewTimeTBXPanel.Create(
            ASensor,
            FTimerNoifier,
            FValueConverter,
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
        VSensorView :=
          TSensorViewPositionTBXPanel.Create(
            ASensor,
            FTimerNoifier,
            FValueConverter,
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
        VSensorView :=
          TSensorViewGPSSatellitesTBXPanel.Create(
            ASensor,
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
  const AValueConverter: IValueToStringConverterChangeable;
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
  FValueConverter := AValueConverter;
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
    AddSensorsInFixedOrder(ASensorList, Result);
    VEnum := ASensorList.GetGUIDEnum;
    while VEnum.Next(1, VGUID, i) = S_OK do begin
      VSensor := ASensorList.Get(VGUID);
      AddSensor(VSensor, Result);
    end;
  finally
    FDefaultDoc.EndUpdate;
    FDefaultDoc.CommitNewPositions := True;
    FDefaultDoc.ArrangeToolbars;
  end;
end;

end.
