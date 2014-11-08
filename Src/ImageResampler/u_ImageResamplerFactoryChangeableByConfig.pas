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

unit u_ImageResamplerFactoryChangeableByConfig;

interface

uses
  i_ImageResamplerFactory,
  i_ImageResamplerConfig,
  i_ImageResamplerFactoryChangeable,
  i_Listener,
  u_ConfigDataElementBase;

type
  TImageResamplerFactoryChangeableByConfig = class(TConfigDataElementWithStaticBaseEmptySaveLoad, IImageResamplerFactoryChangeable)
  private
    FDefGUID: TGUID;
    FList: IImageResamplerFactoryList;
    FConfig: IImageResamplerConfig;
    FConfigChangeListener: IListener;
    FDefItem: IImageResamplerFactory;
    procedure OnConfigChange;
  protected
    function CreateStatic: IInterface; override;
  private
    function GetStatic: IImageResamplerFactory;
  public
    constructor Create(
      const AConfig: IImageResamplerConfig;
      const AList: IImageResamplerFactoryList
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  c_ZeroGUID,
  u_ListenerByEvent;

{ TImageResamplerFactoryChangeableByConfig }

constructor TImageResamplerFactoryChangeableByConfig.Create(
  const AConfig: IImageResamplerConfig;
  const AList: IImageResamplerFactoryList
);
var
  VIndex: Integer;
begin
  Assert(Assigned(AConfig));
  Assert(Assigned(AList));
  Assert(AList.Count > 0);
  inherited Create;
  FList := AList;
  FConfig := AConfig;
  FDefGUID := FConfig.DefaultGUID;

  VIndex := FList.GetIndexByGUID(FDefGUID);
  if VIndex < 0 then begin
    VIndex := 0;
    FDefGUID := FList.GUIDs[0];
  end;
  FDefItem := FList.Items[VIndex];
  Assert(Assigned(FDefItem));

  FConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FConfig.ChangeNotifier.Add(FConfigChangeListener);
end;

destructor TImageResamplerFactoryChangeableByConfig.Destroy;
begin
  if Assigned(FConfig) and Assigned(FConfigChangeListener) then begin
    FConfig.ChangeNotifier.Remove(FConfigChangeListener);
    FConfigChangeListener := nil;
  end;
  inherited;
end;

function TImageResamplerFactoryChangeableByConfig.CreateStatic: IInterface;
var
  VGUID: TGUID;
  VIndex: Integer;
begin
  VGUID := FConfig.ActiveGUID;
  if IsEqualGUID(VGUID, CGUID_Zero) then begin
    Result := FDefItem;
  end else begin
    VIndex := FList.GetIndexByGUID(VGUID);
    if VIndex < 0 then begin
      Result := FDefItem;
    end else begin
      Result := FList.Items[VIndex];
    end;
  end;
end;

function TImageResamplerFactoryChangeableByConfig.GetStatic: IImageResamplerFactory;
begin
  Result := IImageResamplerFactory(GetStaticInternal);
end;

procedure TImageResamplerFactoryChangeableByConfig.OnConfigChange;
begin
  if FList.GetIndexByGUID(FConfig.ActiveGUID) < 0 then begin
    FConfig.ActiveGUID := FDefGUID;
  end;

  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
