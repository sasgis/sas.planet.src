{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_GPSModuleByVSAGPS;

{$I vsagps_defines.inc}

interface

uses
  Windows,
  SysUtils,
  i_GPSPositionFactory,
  i_GPSModuleByCOMPortSettings,
  i_GPSModuleByCOM,
  i_GPSConfig,
  u_GPSModuleAbstract,
{$if defined(VSAGPS_AS_DLL)}
  vsagps_public_dll,
{$else}
  vsagps_memory,
  vsagps_object,
  vsagps_track_writer,
{$ifend}
  vsagps_public_base,
  vsagps_public_types,
  vsagps_public_events,
  vsagps_public_tracks,
  vsagps_public_device,
  vsagps_public_position,
  vsagps_public_trackpoint,
  vsagps_public_sysutils,
  vsagps_public_debugstring,
  vsagps_public_unit_info;

type
  TGPSModuleByVSAGPS = class(TGPSModuleAbstract, IGPSModuleByCOM)
  private
{$if defined(VSAGPS_AS_DLL)}
    FVSAGPS_HANDLE: TVSAGPS_HANDLE;
    FVSAGPS_LOGGER: TVSAGPS_HANDLE;
{$else}
    FVSAGPS_Object: Tvsagps_object;
    FVSAGPS_Logger: Tvsagps_track_writer;
{$ifend}

    FGPSGPS_DevParams: TVSAGPS_ALL_DEVICE_PARAMS;
    FVSAGPS_LOG_WRITER_PARAMS: TVSAGPS_LOG_WRITER_PARAMS;
    FVSAGPS_GPX_WRITER_PARAMS: TVSAGPS_GPX_WRITER_PARAMS;

    // unit info (TODO: array of )
    FVSAGPS_UNIT_INFO: TVSAGPS_UNIT_INFO;

    FLoggerCS: TRTLCriticalSection;
    FConnectCS: TRTLCriticalSection;
    FUnitInfoCS: TRTLCriticalSection;

    FConnectState: Tvsagps_GPSState;
    FConnectedUnitindex: Byte;
    FRecvTimeoutOccured: Boolean;
    FNotify_on_GGA: Boolean;
    FNotify_on_GLL: Boolean;
    FNotify_on_GSA: Boolean;
    FNotify_on_RMC: Boolean;
    FGPSUnitInfo: AnsiString;
  private
    procedure GPSRecv_NMEA_GGA(const AUnitIndex: Byte; const pGGA: PNMEA_GGA);
    procedure GPSRecv_NMEA_GLL(const AUnitIndex: Byte; const pGLL: PNMEA_GLL);
    procedure GPSRecv_NMEA_GSA(const AUnitIndex: Byte; const pGSA: PNMEA_GSA);
    procedure GPSRecv_NMEA_GSV(const AUnitIndex: Byte; const pGSV: PNMEA_GSV);
    procedure GPSRecv_NMEA_RMC(const AUnitIndex: Byte; const pRMC: PNMEA_RMC);
{$if defined(USE_NMEA_VTG)}
    procedure GPSRecv_NMEA_VTG(const AUnitIndex: Byte; const pVTG: PNMEA_VTG);
{$ifend}
    procedure GPSRecv_GARMIN_D800(const AUnitIndex: Byte; const pData: PD800_Pvt_Data_Type);
    procedure GPSRecv_GARMIN_MEAS(const AUnitIndex: Byte; const pData: Pcpo_all_sat_data);

    procedure GPSRecv_LowLevel(const AUnitIndex: Byte;
                               const ADevType: DWORD;
                               const APacket: Pointer);

    procedure GPSRecv_TrackPoint(const AUnitIndex: Byte;
                                 const ADevType: DWORD;
                                 const pData: PSingleTrackPointData);
  private
    procedure ReGenerateGPSUnitInfo(const AUnitIndex: Byte);
    procedure InternalApplyCalcStatsFlag(const AUnitIndex: Byte; const AAllowCalc: Boolean);
    procedure InternalPrepareLoggerParams;

    procedure InternalStopLogger;
    procedure InternalResumeLogger;
    procedure InternalStartLogger(const AConfig: IGPSModuleByCOMPortSettings;
                                  const ALogConfig: IGPSConfig);
    function InternalGetLoggerState(const AConfig: IGPSModuleByCOMPortSettings;
                                    const ALogConfig: IGPSConfig;
                                    out ATrackTypes: TVSAGPS_TrackTypes): Boolean;
    // to fill track log params
    procedure Do_VSAGPS_LOGGER_TRACKPARAMS_EVENT(const pLogger: Pointer;
                                                 const pATP: Pvsagps_AddTrackPoint;
                                                 const AParamsOut: PVSAGPS_LOGGER_GETVALUES_CALLBACK_PARAMS);
    // change logger state
    procedure Do_VSAGPS_LOGGER_STATECHANGED(const pLogger: Pointer;
                                            const AState: Tvsagps_track_writer_state;
                                            const AFatal: LongBool);

    procedure InternalClearUnitInfo(const AUnitIndex: Byte);
    procedure InternalSetUnitInfo(const AUnitIndex: Byte;
                                  const AKind: TVSAGPS_UNIT_INFO_Kind;
                                  const AValue: AnsiString);

    procedure LockUnitInfo(const AUnitIndex: Byte);
    procedure UnlockUnitInfo(const AUnitIndex: Byte);
  private
    procedure GPSDoStateChanged(const AUnitIndex: Byte;
                                const ANewState: Tvsagps_GPSState);
    procedure GPSDoTimeout(const AUnitIndex: Byte);

    // called by device object when unit info changed
    procedure DoGPSUnitInfoChanged(const AUnitIndex: Byte;
                                   const AKind: TVSAGPS_UNIT_INFO_Kind;
                                   const AValue: PAnsiChar);
  protected
    procedure Connect(const AConfig: IGPSModuleByCOMPortSettings;
                      const ALogConfig: IGPSConfig); safecall;
    procedure Disconnect; safecall;
    function GetIsReadyToConnect: Boolean; safecall;
    function GetGPSUnitInfo: String; override;
    function ExecuteGPSCommand(
      const AUnitIndex: Byte;
      const ACommand: LongInt;
      const APointer: Pointer
    ): AnsiString; override;
  protected
    procedure LockLogger;
    procedure UnlockLogger;
    procedure LockConnect;
    procedure UnlockConnect;
    procedure DoAddPointToLogWriter(const AUnitIndex: Byte); override;
  public
    constructor Create(const AGPSPositionFactory: IGPSPositionFactory);
    destructor Destroy; override;
  end;
implementation

uses
  DateUtils,
  Math,
  Classes,
  u_ResStrings,
  t_GeoTypes;


function rVSAGPS_GARMIN_D800_HANDLER(const pUserPointer: Pointer;
                                     const btUnitIndex: Byte;
                                     const dwPacketType: DWORD;
                                     const dwData_Size: DWORD;
                                     const pData: PD800_Pvt_Data_Type): DWORD; stdcall;
begin
  Result:=0;
  if (nil<>pUserPointer) then
  with TGPSModuleByVSAGPS(pUserPointer) do begin
    InternalApplyCalcStatsFlag(btUnitIndex, (0<>(dwPacketType and vgpt_Allow_Stats)));
    GPSRecv_GARMIN_D800(btUnitIndex,pData);
  end;
end;

function rVSAGPS_GARMIN_MEAS_HANDLER(const pUserPointer: Pointer;
                                     const btUnitIndex: Byte;
                                     const dwPacketType: DWORD;
                                     const dwData_Size: DWORD;
                                     const pData: Pcpo_all_sat_data): DWORD; stdcall;
begin
  Result:=0;
  if (nil<>pUserPointer) then
  with TGPSModuleByVSAGPS(pUserPointer) do begin
    InternalApplyCalcStatsFlag(btUnitIndex, (0<>(dwPacketType and vgpt_Allow_Stats)));
    GPSRecv_GARMIN_MEAS(btUnitIndex,pData);
  end;
end;

function rVSAGPS_NMEA_GGA_HANDLER(const pUserPointer: Pointer;
                                  const btUnitIndex: Byte;
                                  const dwPacketType: DWORD;
                                  const pNmeaData: PNMEA_GGA): DWORD; stdcall;
begin
  Result:=0;
  if (nil<>pUserPointer) then
  with TGPSModuleByVSAGPS(pUserPointer) do begin
    InternalApplyCalcStatsFlag(btUnitIndex, (0<>(dwPacketType and vgpt_Allow_Stats)));
    GPSRecv_NMEA_GGA(btUnitIndex,pNmeaData);
  end;
end;

function rVSAGPS_NMEA_GLL_HANDLER(const pUserPointer: Pointer;
                                  const btUnitIndex: Byte;
                                  const dwPacketType: DWORD;
                                  const pNmeaData: PNMEA_GLL): DWORD; stdcall;
begin
  Result:=0;
  if (nil<>pUserPointer) then
  with TGPSModuleByVSAGPS(pUserPointer) do begin
    InternalApplyCalcStatsFlag(btUnitIndex, (0<>(dwPacketType and vgpt_Allow_Stats)));
    GPSRecv_NMEA_GLL(btUnitIndex,pNmeaData);
  end;
end;

function rVSAGPS_NMEA_GSA_HANDLER(const pUserPointer: Pointer;
                                  const btUnitIndex: Byte;
                                  const dwPacketType: DWORD;
                                  const pNmeaData: PNMEA_GSA): DWORD; stdcall;
begin
  Result:=0;
  if (nil<>pUserPointer) then
  with TGPSModuleByVSAGPS(pUserPointer) do begin
    InternalApplyCalcStatsFlag(btUnitIndex, (0<>(dwPacketType and vgpt_Allow_Stats)));
    GPSRecv_NMEA_GSA(btUnitIndex,pNmeaData);
  end;
end;

function rVSAGPS_NMEA_GSV_HANDLER(const pUserPointer: Pointer;
                                  const btUnitIndex: Byte;
                                  const dwPacketType: DWORD;
                                  const pNmeaData: PNMEA_GSV): DWORD; stdcall;
begin
  Result:=0;
  if (nil<>pUserPointer) then
  with TGPSModuleByVSAGPS(pUserPointer) do begin
    InternalApplyCalcStatsFlag(btUnitIndex, (0<>(dwPacketType and vgpt_Allow_Stats)));
    GPSRecv_NMEA_GSV(btUnitIndex,pNmeaData);
  end;
end;

function rVSAGPS_NMEA_RMC_HANDLER(const pUserPointer: Pointer;
                                  const btUnitIndex: Byte;
                                  const dwPacketType: DWORD;
                                  const pNmeaData: PNMEA_RMC): DWORD; stdcall;
begin
  Result:=0;
  if (nil<>pUserPointer) then
  with TGPSModuleByVSAGPS(pUserPointer) do begin
    InternalApplyCalcStatsFlag(btUnitIndex, (0<>(dwPacketType and vgpt_Allow_Stats)));
    GPSRecv_NMEA_RMC(btUnitIndex,pNmeaData);
  end;
end;

{$if defined(USE_NMEA_VTG)}
function rVSAGPS_NMEA_VTG_HANDLER(const pUserPointer: Pointer;
                                  const btUnitIndex: Byte;
                                  const dwPacketType: DWORD;
                                  const pNmeaData: PNMEA_VTG): DWORD; stdcall;
begin
  Result:=0;
  if (nil<>pUserPointer) then
  with TGPSModuleByVSAGPS(pUserPointer) do begin
    InternalApplyCalcStatsFlag(btUnitIndex, (0<>(dwPacketType and vgpt_Allow_Stats)));
    GPSRecv_NMEA_VTG(btUnitIndex,pNmeaData);
  end;
end;
{$ifend}

function rLowLevelHandler(const pUserPointer: Pointer;
                          const btUnitIndex: Byte;
                          const dwDeviceType: DWORD;
                          const pPacket: Pointer): DWORD; stdcall;
begin
  Result:=0;
  if (nil<>pUserPointer) then
  with TGPSModuleByVSAGPS(pUserPointer) do begin
    GPSRecv_LowLevel(btUnitIndex, dwDeviceType, pPacket);
  end;
end;

function rVSAGPS_TRACKPOINT_HANDLER(const pUserPointer: Pointer;
                                    const btUnitIndex: Byte;
                                    const dwDeviceType: DWORD;
                                    const dwPacketType: DWORD;
                                    const pPacket: PSingleTrackPointData): DWORD; stdcall;
begin
  Result:=0;
  if (nil<>pUserPointer) then
  with TGPSModuleByVSAGPS(pUserPointer) do begin
    InternalApplyCalcStatsFlag(btUnitIndex, (0<>(dwPacketType and vgpt_Allow_Stats)));
    GPSRecv_TrackPoint(btUnitIndex, dwDeviceType, pPacket);
  end;
end;

procedure rVSAGPS_UNIT_INFO_DLL_Proc(const pUserPointer: Pointer;
                                     const btUnitIndex: Byte;
                                     const dwGPSDevType: DWORD;
                                     const eKind: TVSAGPS_UNIT_INFO_Kind;
                                     const szValue: PAnsiChar); stdcall;
begin
  if (nil<>pUserPointer) then
    TGPSModuleByVSAGPS(pUserPointer).DoGPSUnitInfoChanged(btUnitIndex, eKind, szValue);
end;

procedure rVSAGPS_GPSState_DLL_Proc(const pUserPointer: Pointer;
                                    const btUnitIndex: Byte;
                                    const dwGPSDevType: DWORD;
                                    const eNewState: Tvsagps_GPSState); stdcall;
begin
  if (nil<>pUserPointer) then
    TGPSModuleByVSAGPS(pUserPointer).GPSDoStateChanged(btUnitIndex, eNewState);
end;

procedure rVSAGPS_GPSTimeout_DLL_Proc(const pUserPointer: Pointer;
                                      const btUnitIndex: Byte;
                                      const dwGPSDevType: DWORD;
                                      const pdwRezerved: PDWORD); stdcall;
begin
  if (nil<>pUserPointer) then
    TGPSModuleByVSAGPS(pUserPointer).GPSDoTimeout(btUnitIndex);
end;

procedure rVSAGPS_LOGGER_TRACKPARAMS_EVENT(const pUserPointer: Pointer;
                                           const pLogger: Pointer;
                                           const pATP: Pvsagps_AddTrackPoint;
                                           const AParamsOut: PVSAGPS_LOGGER_GETVALUES_CALLBACK_PARAMS); stdcall;
begin
  if (nil<>pUserPointer) then
    TGPSModuleByVSAGPS(pUserPointer).Do_VSAGPS_LOGGER_TRACKPARAMS_EVENT(pLogger, pATP, AParamsOut);
end;

procedure rTVSAGPS_LOGGER_STATECHANGED_PROC(const pUserPointer: Pointer;
                                            const pLogger: Pointer;
                                            const AState: Tvsagps_track_writer_state;
                                            const AFatal: LongBool); stdcall;
begin
  if (nil<>pUserPointer) then
    TGPSModuleByVSAGPS(pUserPointer).Do_VSAGPS_LOGGER_STATECHANGED(pLogger, AState, AFatal);
end;

{ TGPSModuleByZylGPS }

constructor TGPSModuleByVSAGPS.Create(const AGPSPositionFactory: IGPSPositionFactory);
begin
  inherited Create(AGPSPositionFactory);

  InitializeCriticalSection(FConnectCS);
  InitializeCriticalSection(FLoggerCS);
  InitializeCriticalSection(FUnitInfoCS);

  Clear_TVSAGPS_UNIT_INFO(@FVSAGPS_UNIT_INFO);

  FConnectedUnitindex:=0;
  FRecvTimeoutOccured:=FALSE;

  // notify if gps data changes
  FNotify_on_GGA:=FALSE;
  FNotify_on_GLL:=FALSE;
  FNotify_on_GSA:=FALSE;
  FNotify_on_RMC:=TRUE;

  ZeroMemory(@FGPSGPS_DevParams, sizeof(FGPSGPS_DevParams));
  FGPSGPS_DevParams.wSize:=Sizeof(FGPSGPS_DevParams);
  //FGPSGPS_DevParams.dwDeviceFlagsIn:=(dpdfi_ConnectingFromConnect or );
  //FGPSGPS_DevParams.pLowLevelHandler:=nil;
  //FGPSGPS_DevParams.pTrackPointHandler:=nil;
  FGPSGPS_DevParams.pGARMIN_D800_HANDLER:=rVSAGPS_GARMIN_D800_HANDLER;
  FGPSGPS_DevParams.pGARMIN_MEAS_HANDLER:=rVSAGPS_GARMIN_MEAS_HANDLER;
  FGPSGPS_DevParams.pNMEA_GGA_HANDLER:=rVSAGPS_NMEA_GGA_HANDLER;
  FGPSGPS_DevParams.pNMEA_GLL_HANDLER:=rVSAGPS_NMEA_GLL_HANDLER;
  FGPSGPS_DevParams.pNMEA_GSA_HANDLER:=rVSAGPS_NMEA_GSA_HANDLER;
  FGPSGPS_DevParams.pNMEA_GSV_HANDLER:=rVSAGPS_NMEA_GSV_HANDLER;
  FGPSGPS_DevParams.pNMEA_RMC_HANDLER:=rVSAGPS_NMEA_RMC_HANDLER;
{$if defined(USE_NMEA_VTG)}
  FGPSGPS_DevParams.pNMEA_VTG_HANDLER:=rVSAGPS_NMEA_VTG_HANDLER;
{$ifend}

{$if defined(VSAGPS_AS_DLL)}
  FVSAGPS_HANDLE:=VSAGPS_Create(Self, @rVSAGPS_GPSState_DLL_Proc, @rVSAGPS_GPSTimeout_DLL_Proc);
  FVSAGPS_LOGGER:=nil;
{$else}
  FVSAGPS_Object:=Tvsagps_object.Create;
  FVSAGPS_Object.ALLDeviceUserPointer:=Self;
  FVSAGPS_Object.OnGPSStateChanged:=rVSAGPS_GPSState_DLL_Proc;
  FVSAGPS_Object.OnGPSTimeout:=rVSAGPS_GPSTimeout_DLL_Proc;
  FVSAGPS_Logger:=nil;
{$ifend}

  InternalPrepareLoggerParams;

  FConnectState := gs_DoneDisconnected;
end;

destructor TGPSModuleByVSAGPS.Destroy;
begin
{$if defined(VSAGPS_USE_DEBUG_STRING)}
  VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.Destroy: begin');
{$ifend}

  Disconnect;

{$if defined(VSAGPS_USE_DEBUG_STRING)}
  VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.Destroy: kill object');
{$ifend}
  
{$if defined(VSAGPS_AS_DLL)}
  if (nil<>FVSAGPS_HANDLE) then begin
    VSAGPS_Destroy(FVSAGPS_HANDLE);
    FVSAGPS_HANDLE:=nil;
  end;
{$else}
  FreeAndNil(FVSAGPS_Object);
{$ifend}

{$if defined(VSAGPS_USE_DEBUG_STRING)}
  VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.Destroy: stop logger');
{$ifend}

  InternalStopLogger;

{$if defined(VSAGPS_USE_DEBUG_STRING)}
  VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.Destroy: delete sections');
{$ifend}

  DeleteCriticalSection(FLoggerCS);
  DeleteCriticalSection(FConnectCS);
  DeleteCriticalSection(FUnitInfoCS);

{$if defined(VSAGPS_USE_DEBUG_STRING)}
  VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.Destroy: inherited');
{$ifend}

  inherited;

{$if defined(VSAGPS_USE_DEBUG_STRING)}
  VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.Destroy: end');
{$ifend}
end;

procedure TGPSModuleByVSAGPS.Connect(const AConfig: IGPSModuleByCOMPortSettings;
                                     const ALogConfig: IGPSConfig);
const
  FlyOnTrackCFG='vsagps_fly-on-track.cfg';
var
  FGPSPortName: AnsiString;
{$if defined(VSAGPS_AS_DLL)}
  FszGPSPortName: PAnsiChar;
{$ifend}
  FGPSDevType: DWORD;
  VTimeout: DWORD;
  VFlyOnTrackSource: WideString;
  VTrackTypes: TVSAGPS_TrackTypes;

  function _IsFlyOnTrackMode: Boolean;
  begin
    Result:=(0<Length(VFlyOnTrackSource));
  end;

  procedure _LoadFlyOnTrackSource;
  var sl: TStringList;
  begin
    if FileExists(FlyOnTrackCFG) then begin
      sl:=TStringList.Create;
      try
        sl.LoadFromFile(FlyOnTrackCFG);
        VFlyOnTrackSource:=sl.Text;
      finally
        sl.Free;
      end;
    end;
  end;

begin
  FRecvTimeoutOccured:=FALSE;
  LockConnect;
  try
    if FConnectState <> gs_DoneDisconnected then
      Exit;

{$if defined(VSAGPS_USE_DEBUG_STRING)}
    VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.Connect: begin');
{$ifend}

    VFlyOnTrackSource:='';
    _LoadFlyOnTrackSource;

{$if defined(VSAGPS_AS_DLL)}
    FszGPSPortName:=nil;
{$ifend}

    // logger (create suspended)
    if (not _IsFlyOnTrackMode) then
      InternalStartLogger(AConfig, ALogConfig);

    // timeouts
    VTimeout:=AConfig.ConnectionTimeout;
    if (VTimeout>$FFFF) then
      VTimeout:=$FFFF;
    FGPSGPS_DevParams.wConnectionTimeoutSec:=VTimeout;
    if (VTimeout>$FF) then
      VTimeout:=$FF;
    FGPSGPS_DevParams.btReceiveGPSTimeoutSec:=VTimeout;

    // delay
    VTimeout:=AConfig.Delay;
    if (VTimeout>$FFFF) then
      VTimeout:=$FFFF;
    FGPSGPS_DevParams.wWorkerThreadTimeoutMSec:=VTimeout;
    
    FGPSGPS_DevParams.iBaudRate:=AConfig.BaudRate;

    // upper limit
    if FGPSGPS_DevParams.wWorkerThreadTimeoutMSec>cWorkingThread_MaxDelay_Msec then
      FGPSGPS_DevParams.wWorkerThreadTimeoutMSec:=cWorkingThread_MaxDelay_Msec;

    if _IsFlyOnTrackMode then
      FGPSGPS_DevParams.pLowLevelHandler:=nil
    else if InternalGetLoggerState(AConfig, ALogConfig, VTrackTypes) then
      FGPSGPS_DevParams.pLowLevelHandler:=rLowLevelHandler
    else
      FGPSGPS_DevParams.pLowLevelHandler:=nil;

    // reserved for fly-on-track mode
    if _IsFlyOnTrackMode then
      FGPSGPS_DevParams.pTrackPointHandler:=rVSAGPS_TRACKPOINT_HANDLER
    else
      FGPSGPS_DevParams.pTrackPointHandler:=nil;

    if _IsFlyOnTrackMode then begin
      FGPSDevType:=gdt_FILE_Track;
{$if not defined(VSAGPS_AS_DLL)}
      FGPSPortName:='';
{$ifend}
      FGPSGPS_DevParams.btAutodetectOnConnect:=0; // no autodetect
      FGPSGPS_DevParams.dwAutodetectFlags:=0;
    end else if AConfig.USBGarmin then begin
      // USB Garmin - always autodetect (check attached usb devices by guid)
      FGPSDevType:=gdt_USB_Garmin;
{$if not defined(VSAGPS_AS_DLL)}
      FGPSPortName:='';
{$ifend}
      FGPSGPS_DevParams.btAutodetectOnConnect:=0; // always autodetect by internal algoritm
      FGPSGPS_DevParams.dwAutodetectFlags:=0;
    end else begin
      // NMEA via COMx port
      FGPSDevType:=gdt_COM_NMEA0183;
      FGPSPortName:='COM' + IntToStr(AConfig.Port);
{$if defined(VSAGPS_AS_DLL)}
      FGPSPortName:=FGPSPortName+#0;
      FszGPSPortName:=PAnsiChar(FGPSPortName);
{$ifend}
      FGPSGPS_DevParams.btAutodetectOnConnect:=Ord(AConfig.AutodetectCOMOnConnect<>FALSE);
      FGPSGPS_DevParams.dwAutodetectFlags:=AConfig.AutodetectCOMFlags;
    end;

{$if defined(VSAGPS_USE_DEBUG_STRING)}
    VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.Connect: before GPSConnect');
{$ifend}

    try
{$if defined(VSAGPS_AS_DLL)}
      if (not VSAGPS_Connect(FVSAGPS_HANDLE,
                             FGPSDevType,
                             FszGPSPortName,
                             PWideChar(VFlyOnTrackSource),
                             @FGPSGPS_DevParams,
                             nil,
                             rVSAGPS_UNIT_INFO_DLL_Proc,
                             @FConnectedUnitindex,
                             nil)) then
{$else}
      if (not FVSAGPS_Object.GPSConnect(FGPSDevType,
                                        FGPSPortName,
                                        PWideChar(VFlyOnTrackSource),
                                        @FGPSGPS_DevParams,
                                        nil,
                                        rVSAGPS_UNIT_INFO_DLL_Proc,
                                        @FConnectedUnitindex,
                                        nil)) then
{$ifend}
      begin

{$if defined(VSAGPS_USE_DEBUG_STRING)}
        VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.Connect: not connected');
{$ifend}

        // not connected
        raise Exception.Create(SAS_MSG_NoGPSdetected);
      end;
    except
{$if defined(VSAGPS_AS_DLL)}
      VSAGPS_Disconnect(FVSAGPS_HANDLE);
{$else}
      FVSAGPS_Object.GPSDisconnect;
{$ifend}
      raise;
    end;
  finally
    UnLockConnect;
  end;
end;

procedure TGPSModuleByVSAGPS.GPSDoStateChanged(const AUnitIndex: Byte;
                                               const ANewState: Tvsagps_GPSState);
begin
  // check only global state
  if (cUnitIndex_ALL<>AUnitIndex) then
    Exit;
  
  if (FConnectState<>ANewState) or (gs_DoneDisconnected=ANewState) then begin
    FConnectState:=ANewState;

    case FConnectState of
    gs_DoneDisconnected: begin
      _UpdateToEmptyPosition;
      if FRecvTimeoutOccured then
        TimeOutNotifier.Notify(nil);
      DisconnectedNotifier.Notify(nil);
      DoGPSUnitInfoChanged(AUnitIndex, guik_ClearALL, ''); // reset unit info
    end;
    gs_ProcessConnecting:
      ConnectingNotifier.Notify(nil);
    gs_DoneConnected: begin
      ConnectedNotifier.Notify(nil);
      InternalResumeLogger;
    end;
    gs_ProcessDisconnecting:
      DisconnectingNotifier.Notify(nil);
    end;
  end;
end;

procedure TGPSModuleByVSAGPS.GPSDoTimeout(const AUnitIndex: Byte);
begin
  FRecvTimeoutOccured:=TRUE;
end;

procedure TGPSModuleByVSAGPS.Disconnect;
begin
{$if defined(VSAGPS_USE_DEBUG_STRING)}
  VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.Disconnect: begin');
{$ifend}

  inherited;

  LockConnect;
  try
{$if defined(VSAGPS_USE_DEBUG_STRING)}
    VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.Disconnect: check');
{$ifend}

    if FConnectState<>gs_DoneDisconnected then begin
{$if defined(VSAGPS_USE_DEBUG_STRING)}
      VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.Disconnect: GPSDisconnect');
{$ifend}
    
{$if defined(VSAGPS_AS_DLL)}
      VSAGPS_Disconnect(FVSAGPS_HANDLE);
{$else}
      FVSAGPS_Object.GPSDisconnect;
{$ifend}
    end;

{$if defined(VSAGPS_USE_DEBUG_STRING)}
    VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.Disconnect: stop logger');
{$ifend}

    // stop low-level logging
    InternalStopLogger;
  finally
    UnLockConnect;
  end;
  
{$if defined(VSAGPS_USE_DEBUG_STRING)}
  VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.Disconnect: end');
{$ifend}
end;

procedure TGPSModuleByVSAGPS.DoAddPointToLogWriter(const AUnitIndex: Byte);
var
  tATP: Tvsagps_AddTrackPoint;
  s: AnsiString;
begin
  //inherited;
  LockLogger;
  try
    if (nil<>FVSAGPS_Logger) then begin
      ZeroMemory(@tATP, sizeof(tATP));
      tATP.pPos:=@FSingleGPSData;
      tATP.pSatFixAll:=@FFixSatsALL;
      tATP.btUnitIndex:=AUnitIndex;
      try
        // get sats info (if requested)
        if (0<>FVSAGPS_GPX_WRITER_PARAMS.btWrite_Sasx_Sats_Info) then begin
          s:=SerializeSatsInfo;
          if (0<Length(s)) then begin
            tATP.szSatsInfo := VSAGPS_AllocPCharByString(s, TRUE);
          end;
        end;
        // run
{$if defined(VSAGPS_AS_DLL)}
        VSAGPS_AddLoggerTrackPoint(FVSAGPS_LOGGER, @tATP);
{$else}
        FVSAGPS_Logger.AddTrackPoint(@tATP);
{$ifend}
      finally
        VSAGPS_FreeAndNil_PChar(tATP.szSatsInfo);
      end;
    end;
  finally
    UnlockLogger;
  end;
end;

function TGPSModuleByVSAGPS.ExecuteGPSCommand(
  const AUnitIndex: Byte;
  const ACommand: LongInt;
  const APointer: Pointer
): AnsiString;
begin
  // some special commands

  if (gpsc_Refresh_GPSUnitInfo=ACommand) then begin
    LockUnitInfo(AUnitIndex);
    try
      FGPSUnitInfo := VSAGPS_Generate_GPSUnitInfo(@FVSAGPS_UNIT_INFO);
      Result := FGPSUnitInfo;
    finally
      UnlockUnitInfo(AUnitIndex);
    end;
    Exit;
  end;

  if (gpsc_LocalTimeChanged=ACommand) or
     (gpsc_RestartTrackLogs=ACommand) or
     (
       (0<>FVSAGPS_GPX_WRITER_PARAMS.btUse_Predefined_Extensions[geSASGIS]) and
       ( // some commands only for sasx
         (gpsc_WriteFileLinkToLog=ACommand) or
         (gpsc_CreateStandaloneWpt=ACommand)
       )
     ) then begin
    LockLogger;
    try
      if (nil<>FVSAGPS_Logger) then begin
{$if defined(VSAGPS_AS_DLL)}
        VSAGPS_ExecuteLoggerCommand(FVSAGPS_LOGGER, AUnitIndex, ACommand, APointer);
{$else}
        FVSAGPS_Logger.ExecuteLoggerCommand(AUnitIndex, ACommand, APointer);
{$ifend}
      end;
    finally
      UnlockLogger;
    end;
    Exit;
  end;

  LockConnect;
  try
    if (gs_DoneConnected =
{$if defined(VSAGPS_AS_DLL)}
    VSAGPS_GPSState(FVSAGPS_HANDLE)
{$else}
    FVSAGPS_Object.GPSState
{$ifend}
    ) then begin
{$if defined(VSAGPS_AS_DLL)}
      VSAGPS_ExecuteGPSCommand_OnUnit(FVSAGPS_HANDLE, FConnectedUnitindex, ACommand, APointer);
{$else}
      FVSAGPS_Object.Execute_GPSCommand_OnUnit(FConnectedUnitindex, ACommand, APointer);
{$ifend}
    end;
  finally
    UnLockConnect;
  end;
end;

procedure TGPSModuleByVSAGPS.DoGPSUnitInfoChanged(
  const AUnitIndex: Byte;
  const AKind: TVSAGPS_UNIT_INFO_Kind;
  const AValue: PAnsiChar);
var s: AnsiString;
begin
  // save to info
  if (guik_ClearALL=AKind) then
    InternalClearUnitInfo(AUnitIndex)
  else begin
    SafeSetStringP(s, AValue);
    InternalSetUnitInfo(AUnitIndex, AKind, s);
  end;
  ReGenerateGPSUnitInfo(AUnitIndex);
end;

function TGPSModuleByVSAGPS.GetGPSUnitInfo: String;
begin
  LockUnitInfo(cUnitIndex_Reserved);
  try
    Result := FGPSUnitInfo;
  finally
    UnlockUnitInfo(cUnitIndex_Reserved);
  end;
end;

function TGPSModuleByVSAGPS.GetIsReadyToConnect: Boolean;
begin
{$if defined(VSAGPS_AS_DLL)}
  Result:=(nil=FVSAGPS_HANDLE) or (gs_DoneDisconnected=VSAGPS_GPSState(FVSAGPS_HANDLE))
{$else}
  Result:=(nil=FVSAGPS_Object) or (gs_DoneDisconnected=FVSAGPS_Object.GPSState);
{$ifend}  
end;

procedure TGPSModuleByVSAGPS.GPSRecv_NMEA_GSV(const AUnitIndex: Byte; const pGSV: PNMEA_GSV);
var
  VFixIndex: ShortInt;
  VStatus: Byte;
  VTalkerID: AnsiString;
begin
  if (nil=pGSV) or (sizeof(pGSV^)<>pGSV^.dwSize) then
    Exit;
  LockGPSData;
  try
    // 'GP' = for GPS and SBAS
    // 'GL' = for GLONASS
    VTalkerID:=NMEA_TalkerID_to_String(@(pGSV^.chTalkerID));

    _UpdateSatsInView(VTalkerID, pGSV^.sats_in_view);
    
    if (0<pGSV^.sats_in_view) then begin
      VStatus:=cSat_Status_Unavailable;
      if (not GetSatNumberIndexEx(Select_PVSAGPS_FIX_SATS_from_ALL(@FFixSatsALL,VTalkerID), @(pGSV^.info.sat_info), VFixIndex)) then
        VFixIndex:=-1;

      if (pGSV^.info.sat_info.svid>0) and (pGSV^.info.snr>0) then begin
        Inc(VStatus); // really 1 or 2 - i.e. >0
        if (0<=VFixIndex) then
          Inc(VStatus);
      end;

      _UpdateSattelite(
          VTalkerID,
          pGSV^.global_index,
          pGSV^.info.sat_info,
          pGSV^.info.sat_ele,
          pGSV^.info.azimuth,
          pGSV^.info.snr,
          VStatus,
          0);
    end;
  finally
    UnLockGPSData(AUnitIndex, FALSE, TRUE);
  end;
end;

procedure TGPSModuleByVSAGPS.GPSRecv_NMEA_RMC(const AUnitIndex: Byte; const pRMC: PNMEA_RMC);
var
  VPoint: TDoublePoint;
  VPositionOK: Boolean;
  VUTCDateOK: Boolean;
  VUTCDate: TDateTime;
  VUTCTimeOK: Boolean;
  VUTCTime: TDateTime;
begin
  if (nil=pRMC) or (sizeof(pRMC^)<>pRMC^.dwSize) then
    Exit;
  LockGPSData;
  try
    VPositionOK:=Nmea_Coord_To_Double(@(pRMC^.lon), VPoint.X) and
                 Nmea_Coord_To_Double(@(pRMC^.lat), VPoint.Y);

    VUTCDateOK:=Nmea_Date_To_DateTime(@(pRMC^.date), VUTCDate);
    VUTCTimeOK:=Nmea_Time_To_DateTime(@(pRMC^.time), VUTCTime);

    _UpdatePosTime(
      VPositionOK,
      VPoint,
      VUTCTimeOK,
      VUTCTime);

    _UpdateSpeedHeading(
      pRMC^.speed_in_knots*cNmea_knot_to_kmph,
      pRMC^.course_in_degrees,
      0,
      FALSE);

    _UpdateUTCDate(
      VUTCDateOK,
      VUTCDate);

    _UpdateMagVar(
      pRMC^.magvar_deg,
      pRMC^.magvar_sym
    );
      
    _UpdateNavMode(pRMC^.status); // A (valid) or V (not valid)
    _UpdateNmea23Mode(pRMC^.nmea23_mode, TRUE, FALSE);
  finally
    UnLockGPSData(AUnitIndex, FNotify_on_RMC, FALSE);
  end;
end;

procedure TGPSModuleByVSAGPS.GPSRecv_TrackPoint(const AUnitIndex: Byte;
                                                const ADevType: DWORD;
                                                const pData: PSingleTrackPointData);
begin
  if (nil=pData) then
    Exit;
  LockGPSData;
  try
    _UpdateFromTrackPoint(pData);
  finally
    UnLockGPSData(AUnitIndex, TRUE, FALSE);
  end;
end;

procedure TGPSModuleByVSAGPS.Do_VSAGPS_LOGGER_STATECHANGED(const pLogger: Pointer;
                                                           const AState: Tvsagps_track_writer_state;
                                                           const AFatal: LongBool);
begin

end;

procedure TGPSModuleByVSAGPS.Do_VSAGPS_LOGGER_TRACKPARAMS_EVENT(const pLogger: Pointer;
                                                                const pATP: Pvsagps_AddTrackPoint;
                                                                const AParamsOut: PVSAGPS_LOGGER_GETVALUES_CALLBACK_PARAMS);
  procedure _AllocA(const s: AnsiString);
  var dwLen: DWORD;
  begin
    if (nil<>AParamsOut.AStrResult) then begin
      VSAGPS_FreeMem(AParamsOut.AStrResult);
      AParamsOut.AStrResult:=nil;
    end;
    dwLen:=Length(s);
    AParamsOut.AStrResult:=VSAGPS_GetMem(dwLen+1);
    CopyMemory(AParamsOut.AStrResult, PAnsiChar(s), dwLen);
    AParamsOut.AStrResult[dwLen]:=#0;
  end;

var
  st: AnsiString;
begin
  if (ttGPX=pATP^.eTrackType) then
  case pATP^.eTrackParam of
    tpTrkSrc: begin
      // source of track
      LockUnitInfo(pATP^.btUnitIndex);
      try
        st := VSAGPS_Generate_GPSUnitInfo(@FVSAGPS_UNIT_INFO);
      finally
        UnlockUnitInfo(pATP^.btUnitIndex);
      end;
      if (0<Length(st)) then begin
        AParamsOut^.UsePredefined:=FALSE;
        AParamsOut^.UseResult:=TRUE;
        _AllocA(st);
      end;
    end;
  end;
end;

{$if defined(USE_NMEA_VTG)}
procedure TGPSModuleByVSAGPS.GPSRecv_NMEA_VTG(const AUnitIndex: Byte; const pVTG: PNMEA_VTG);
var
  Vspeed: Double;
  VHeading: Double;
begin
  if (nil=pVTG) or (sizeof(pVTG^)<>pVTG^.dwSize) then
    Exit;
  LockGPSData;
  try
    InternalApplyCalcStatsFlag(AUnitIndex);

    VHeading:=cGps_Float32_no_data;
    if ('T'=pVTG^.trk_sym) then
      VHeading:=pVTG^.trk_deg
    else if ('T'=pVTG^.mag_sym) then
      VHeading:=pVTG^.mag_deg;

    Vspeed:=cGps_Float32_no_data;
    if ('K'=pVTG^.kmph_sym) then
      Vspeed:=pVTG^.kmph_speed
    else if ('K'=pVTG^.knots_sym) then
      Vspeed:=pVTG^.knots_speed
    else if ('N'=pVTG^.knots_sym) then
      Vspeed:=pVTG^.knots_speed*cNmea_knot_to_kmph
    else if ('N'=pVTG^.kmph_sym) then
      Vspeed:=pVTG^.kmph_speed*cNmea_knot_to_kmph;

    _UpdateSpeedHeading(Vspeed,
                        VHeading,
                        0,
                        FALSE);
                        
    _UpdateNmea23Mode(pVTG^.nmea23_mode, TRUE, FALSE);
  finally
    UnLockGPSData(FALSE, FALSE);
  end;
end;
{$ifend}

procedure TGPSModuleByVSAGPS.InternalApplyCalcStatsFlag(const AUnitIndex: Byte; const AAllowCalc: Boolean);
begin
  // TODO: multiple flags
  FSingleGPSData.AllowCalcStats:=AAllowCalc;
end;

procedure TGPSModuleByVSAGPS.InternalClearUnitInfo(const AUnitIndex: Byte);
begin
  LockUnitInfo(AUnitIndex);
  try
    Clear_TVSAGPS_UNIT_INFO(@FVSAGPS_UNIT_INFO);
  finally
    UnlockUnitInfo(AUnitIndex);
  end;
end;

function TGPSModuleByVSAGPS.InternalGetLoggerState(
  const AConfig: IGPSModuleByCOMPortSettings;
  const ALogConfig: IGPSConfig;
  out ATrackTypes: TVSAGPS_TrackTypes): Boolean;
begin
  if Assigned(ALogConfig) then
    Result := ALogConfig.AllowWriteLog(ATrackTypes) and (ATrackTypes<>[])
  else begin
    Result := FALSE;
    ATrackTypes := [];
  end;
  if not Result then
    Result := Assigned(AConfig) and AConfig.NMEALog;
end;

procedure TGPSModuleByVSAGPS.InternalPrepareLoggerParams;
begin
  ZeroMemory(@FVSAGPS_LOG_WRITER_PARAMS, sizeof(FVSAGPS_LOG_WRITER_PARAMS));
  FVSAGPS_LOG_WRITER_PARAMS.wSize:=sizeof(FVSAGPS_LOG_WRITER_PARAMS);
  FVSAGPS_LOG_WRITER_PARAMS.DelEmptyFileOnClose:=TRUE;
  FVSAGPS_LOG_WRITER_PARAMS.pLoggerStateChangedProc:=rTVSAGPS_LOGGER_STATECHANGED_PROC;
  // FVSAGPS_LOG_WRITER_PARAMS.RestartLogAfterPoints[ttGPX]:=10000; // restart after 10000 points
  // FVSAGPS_LOG_WRITER_PARAMS.RestartLogAfterBytes[ttGarmin]:=1024*1024*10; // restart if bigger than 10MByte

  ZeroMemory(@FVSAGPS_GPX_WRITER_PARAMS, sizeof(FVSAGPS_GPX_WRITER_PARAMS));
  FVSAGPS_GPX_WRITER_PARAMS.wSize:=sizeof(FVSAGPS_GPX_WRITER_PARAMS);
  FVSAGPS_GPX_WRITER_PARAMS.dwLoggerFlags:=lwpf_SkipNoneFix; // do not log without fix
  FVSAGPS_GPX_WRITER_PARAMS.btWrite_Undocumented_Speed:=1; // for gpsmapedit support
  FVSAGPS_GPX_WRITER_PARAMS.btWrite_Sasx_LocalTime:=1;
  FVSAGPS_GPX_WRITER_PARAMS.btWrite_Sasx_TimeShift:=1;
  FVSAGPS_GPX_WRITER_PARAMS.btWrite_Sasx_Sats_Info:=1;
  FVSAGPS_GPX_WRITER_PARAMS.btUse_Predefined_Extensions[geSASGIS]:=1; // for sasgis extensions
end;

procedure TGPSModuleByVSAGPS.InternalResumeLogger;
begin
  LockLogger;
  try
    if Assigned(FVSAGPS_Logger) then begin
{$if defined(VSAGPS_AS_DLL)}
      VSAGPS_SetPausedLogger(FVSAGPS_LOGGER, FALSE);
{$else}
      FVSAGPS_Logger.SetPaused(FALSE);
{$ifend}
    end;
  finally
    UnlockLogger;
  end;
end;

procedure TGPSModuleByVSAGPS.InternalSetUnitInfo(
  const AUnitIndex: Byte;
  const AKind: TVSAGPS_UNIT_INFO_Kind;
  const AValue: AnsiString
);
begin
  LockUnitInfo(AUnitIndex);
  try
    FVSAGPS_UNIT_INFO[AKind]:=AValue;
  finally
    UnlockUnitInfo(AUnitIndex);
  end;
end;

procedure TGPSModuleByVSAGPS.InternalStartLogger(const AConfig: IGPSModuleByCOMPortSettings;
                                                 const ALogConfig: IGPSConfig);
var
  VTrackTypes: TVSAGPS_TrackTypes;
  VLoggerPath: WideString;
  tCallbackFilter: TVSAGPS_LOGGER_GETVALUES_CALLBACK_FILTER;
begin
  if InternalGetLoggerState(AConfig, ALogConfig, VTrackTypes) then begin
    LockLogger;
    try
      // create or stop running
      if (nil=FVSAGPS_Logger) then begin
        // set path for new logger
        VLoggerPath:=IncludeTrailingPathDelimiter(AConfig.LogPath);
        tCallbackFilter.seCallCallbackOnParams:=[tpTrkSrc];
        // create new
{$if defined(VSAGPS_AS_DLL)}
        FVSAGPS_Logger:=VSAGPS_MakeLogger(Self,
                                          @FVSAGPS_LOG_WRITER_PARAMS,
                                          @FVSAGPS_GPX_WRITER_PARAMS,
                                          PWideChar(VLoggerPath),
                                          rVSAGPS_LOGGER_TRACKPARAMS_EVENT,
                                          @tCallbackFilter);
{$else}
        FVSAGPS_Logger:=Tvsagps_track_writer.Create;
        FVSAGPS_Logger.SetALLLoggerParams(Self,
                                          @FVSAGPS_LOG_WRITER_PARAMS,
                                          @FVSAGPS_GPX_WRITER_PARAMS,
                                          PWideChar(VLoggerPath),
                                          rVSAGPS_LOGGER_TRACKPARAMS_EVENT,
                                          @tCallbackFilter);
{$ifend}
      end else begin
        // close tracks (if opened)
{$if defined(VSAGPS_AS_DLL)}
        VSAGPS_CloseLogger(FVSAGPS_LOGGER);
{$else}
        FVSAGPS_Logger.CloseALL;
{$ifend}
      end;

      // define log type(s)
      if Assigned(ALogConfig) then begin
        // get types
        System.Exclude(VTrackTypes,ttGarmin);
        System.Exclude(VTrackTypes,ttNMEA);
      end else begin
        // no log
        VTrackTypes:=[];
      end;

      if AConfig.NMEALog then begin
        if AConfig.USBGarmin then
          System.Include(VTrackTypes,ttGarmin)
        else
          System.Include(VTrackTypes,ttNMEA);
      end;

      // create suspended
{$if defined(VSAGPS_AS_DLL)}
      VSAGPS_StartLogger(FVSAGPS_LOGGER, Byte(VTrackTypes), TRUE, nil);
{$else}
      FVSAGPS_Logger.StartTracksEx(VTrackTypes, TRUE, nil);
{$ifend}
    finally
      UnlockLogger;
    end;
  end else begin
    // close if exists
    InternalStopLogger;
  end;
end;

procedure TGPSModuleByVSAGPS.InternalStopLogger;
begin
  LockLogger;
  try
{$if defined(VSAGPS_AS_DLL)}
    if (nil<>FVSAGPS_LOGGER) then begin
      VSAGPS_CloseLogger(FVSAGPS_LOGGER);
      VSAGPS_Destroy(FVSAGPS_LOGGER);
      FVSAGPS_LOGGER:=nil;
    end;
{$else}
    if (nil<>FVSAGPS_Logger) then begin
      FVSAGPS_Logger.CloseALL;
      FreeAndNil(FVSAGPS_Logger);
    end;
{$ifend}
  finally
    UnlockLogger;
  end;
end;

procedure TGPSModuleByVSAGPS.LockConnect;
begin
{$if defined(VSAGPS_USE_DEBUG_STRING)}
  VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.LockConnect: in');
{$ifend}
  EnterCriticalSection(FConnectCS);
{$if defined(VSAGPS_USE_DEBUG_STRING)}
  VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.LockConnect: ok');
{$ifend}
end;

procedure TGPSModuleByVSAGPS.LockLogger;
begin
{$if defined(VSAGPS_USE_DEBUG_STRING)}
  VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.LockLogger: in');
{$ifend}
  EnterCriticalSection(FLoggerCS);
{$if defined(VSAGPS_USE_DEBUG_STRING)}
  VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.LockLogger: ok');
{$ifend}
end;

procedure TGPSModuleByVSAGPS.LockUnitInfo(const AUnitIndex: Byte);
begin
{$if defined(VSAGPS_USE_DEBUG_STRING)}
  VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.LockUnitInfo: in');
{$ifend}
  EnterCriticalSection(FUnitInfoCS);
{$if defined(VSAGPS_USE_DEBUG_STRING)}
  VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.LockUnitInfo: ok');
{$ifend}
end;

procedure TGPSModuleByVSAGPS.ReGenerateGPSUnitInfo(const AUnitIndex: Byte);
begin
  LockUnitInfo(AUnitIndex);
  try
    ExecuteGPSCommand(AUnitIndex, gpsc_Refresh_GPSUnitInfo, nil);
  finally
    UnlockUnitInfo(AUnitIndex);
  end;
end;

procedure TGPSModuleByVSAGPS.UnlockConnect;
begin
{$if defined(VSAGPS_USE_DEBUG_STRING)}
  VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.UnlockConnect: in');
{$ifend}
  LeaveCriticalSection(FConnectCS);
{$if defined(VSAGPS_USE_DEBUG_STRING)}
  VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.UnlockConnect: ok');
{$ifend}
end;

procedure TGPSModuleByVSAGPS.UnlockLogger;
begin
{$if defined(VSAGPS_USE_DEBUG_STRING)}
  VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.UnlockLogger: in');
{$ifend}
  LeaveCriticalSection(FLoggerCS);
{$if defined(VSAGPS_USE_DEBUG_STRING)}
  VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.UnlockLogger: ok');
{$ifend}
end;

procedure TGPSModuleByVSAGPS.UnlockUnitInfo(const AUnitIndex: Byte);
begin
{$if defined(VSAGPS_USE_DEBUG_STRING)}
  VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.UnlockUnitInfo: in');
{$ifend}
  LeaveCriticalSection(FUnitInfoCS);
{$if defined(VSAGPS_USE_DEBUG_STRING)}
  VSAGPS_DebugAnsiString('TGPSModuleByVSAGPS.UnlockUnitInfo: ok');
{$ifend}
end;

procedure TGPSModuleByVSAGPS.GPSRecv_GARMIN_D800(const AUnitIndex: Byte; const pData: PD800_Pvt_Data_Type);
var
  VPoint: TDoublePoint;
  VUTCTime: TDateTime;
  VUTCDate: TDateTime;
  VTemp: Byte;
  VNmea23Mode: AnsiChar;
begin
  if (nil=pData) then
    Exit;
  LockGPSData;
  try
    // altitude above WGS 84 ellipsoid + height of WGS84 ellipsoid above MSL
    _UpdateAlt(pData^.alt+pData^.msl_hght);
    _UpdateGeoidHeight(-pData^.msl_hght); // here we need: Height (in meters) of geoid (mean sea level) above WGS84 earth ellipsoid

    // 2 sigma
    _UpdateDOP(pData^.eph/2,
               pData^.epv/2,
               pData^.epe/2);

    // dimentions
    VTemp:=pData^.fix;
    if (VTemp>=Type1_fix_2D_diff) then
      VTemp:=VTemp-Type1_fix_2D_diff+2;
    _UpdateDimentions(VTemp);

    // fixstatus
    VTemp:=pData^.fix;
    if (VTemp>Type1_fix_2D) then
      VTemp:=Type1_fix_2D; // 2
    _UpdateFixStatus(VTemp);

    // dgps
    VNmea23Mode:=#0;
    if (pData^.fix >= Type1_fix_2D_diff) then
      VNmea23Mode:='D'
    else if (pData^.fix >= Type1_fix_2D) then
      VNmea23Mode:='A';
    _UpdateNmea23Mode(VNmea23Mode, FALSE, FALSE);

    VPoint.X:=pData^.posn.lon*180/PI;
    VPoint.Y:=pData^.posn.lat*180/PI;

    VUTCTime:=Get_UTCDateTime_From_D800(pData);

    VUTCDate:=DateOf(VUTCTime);
    VUTCTime:=TimeOf(VUTCTime);


    _UpdatePosTime(
      TRUE,
      VPoint,
      TRUE,
      VUTCTime);

    _UpdateSpeedHeading(
      sqrt(pData^.east*pData^.east+pData^.north*pData^.north+pData^.up*pData^.up)*3.6, // in km/h
      ArcTan2(pData^.north,pData^.east)*180/PI,
      pData^.up, // in m/s
      TRUE);

    _UpdateUTCDate(
      TRUE,
      VUTCDate);
  finally
    UnLockGPSData(AUnitIndex, TRUE, FALSE);
  end;
end;

procedure TGPSModuleByVSAGPS.GPSRecv_GARMIN_MEAS(const AUnitIndex: Byte; const pData: Pcpo_all_sat_data);
var
  i:Byte;
  VSatCount: Byte;
  Vsnr: SInt16;
  VStatus: Byte;
  VFixedStatus: Byte;
  VFixedSats: TVSAGPS_FIX_SATS;
begin
  if (nil=pData) then
    Exit;
  LockGPSData;
  try
    VSatCount:=0;
    for i:=0 to cpo_all_sat_data_count-1 do begin
      // params for sat
      VStatus:=(pData^.sv[i].status and cGarmin_Flag_Fixed_Mask);

      VFixedStatus:=cSat_Status_Unavailable;

      if (0<pData^.sv[i].svid) and (0<>VStatus) then begin // sat number ok, with some bits
        // 001 (1) - returns 1 = cSat_Status_Visible
        // 100 (4) - returns 2 = cSat_Status_InSolution
        // 101 (5) - returns 3 = cSat_Status_FixWithEphe
        if (cGarmin_Flag_Fixed_Mask = (VStatus and cGarmin_Flag_Fixed_Mask)) then
          VFixedStatus:=cSat_Status_Fixed
        else
          VFixedStatus:=cSat_Status_Visible;
      end;     

      Vsnr:=pData^.sv[i].snr div snr_to_procents_divider;

      if SatAvailableForShow(pData^.sv[i].svid, Vsnr, VFixedStatus) then
        Inc(VSatCount);

      _UpdateSattelite(nmea_ti_GPS,
                       i,
                       Make_TVSAGPS_FIX_SAT(pData^.sv[i].svid, 0),
                       pData^.sv[i].elev,
                       pData^.sv[i].azmth,
                       Vsnr,
                       VFixedStatus,
                       pData^.sv[i].status);

      if (VFixedStatus>=cSat_Status_Fixed) then
        VFixedSats.sats[i].svid:=pData^.sv[i].svid
      else
        VFixedSats.sats[i].svid:=cGPS_Invalid_SatNumber;
        
      // no constellation_flag for garmin
      VFixedSats.sats[i].constellation_flag:=0;
    end;
    
    _UpdateSatsInView(nmea_ti_GPS, VSatCount);

    VFixedSats.fix_count:=VSatCount;
    VFixedSats.all_count:=VSatCount;
    _UpdateFixedSats(nmea_ti_GPS, @VFixedSats);
  finally
    UnLockGPSData(AUnitIndex, FALSE, TRUE);
  end;
end;

procedure TGPSModuleByVSAGPS.GPSRecv_LowLevel(const AUnitIndex: Byte;
                                              const ADevType: DWORD;
                                              const APacket: Pointer);

  function _Active: LongBool;
  begin
{$if defined(VSAGPS_AS_DLL)}
    Result := VSAGPS_ActiveLogger(FVSAGPS_LOGGER);
{$else}
    Result := FVSAGPS_Logger.Active;
{$ifend}  
  end;

  procedure AddLoggerPacket(const p: PAnsiChar);
  begin
{$if defined(VSAGPS_AS_DLL)}
    VSAGPS_AddLoggerPacket(FVSAGPS_LOGGER, p, StrLen(p), nil);
{$else}
    FVSAGPS_Logger.AddPacket(p, StrLen(p), nil);
{$ifend}
  end;

  procedure InternalDumpByDevice;
  var p: PAnsiChar;
  begin
{$if defined(VSAGPS_AS_DLL)}
    p:=VSAGPS_SerializePacket(FVSAGPS_HANDLE, AUnitIndex, APacket, nil);
{$else}
    p:=FVSAGPS_Object.SerializePacket(AUnitIndex, APacket);
{$ifend}
    try
      AddLoggerPacket(p);
    finally
      VSAGPS_FreeMem(p);
    end;
  end;

begin
  if (nil<>FVSAGPS_LOGGER) and (nil<>APacket) then
  if _Active then begin
    // get packet for logging
    if (gdt_USB_Garmin=(ADevType and gdt_USB_Garmin)) then begin
      // write full dump of binary garmin packets (with header about packet type)
      InternalDumpByDevice;
    end else if (gdt_COM_NMEA0183=(ADevType and gdt_COM_NMEA0183)) then begin
      // write full nmea packets like a strings
      AddLoggerPacket(PAnsiChar(APacket));
    end;
  end;
end;

procedure TGPSModuleByVSAGPS.GPSRecv_NMEA_GGA(const AUnitIndex: Byte; const pGGA: PNMEA_GGA);
var
  VPoint: TDoublePoint;
  VPositionOK: Boolean;
  VUTCTimeOK: Boolean;
  VUTCTime: TDateTime;
  VNmea23Mode: AnsiChar;
begin
  if (nil=pGGA) or (sizeof(pGGA^)<>pGGA^.dwSize) then
    Exit;
  LockGPSData;
  try
    // vsa: disabled because:
    // a) sometimes returns 9 instead of 12 in GSV (less)
    // b) for GN (GP+GL) shows number of all sats
    //_UpdateSatsInView(pGGA^.sats_in_view);

    VPositionOK:=Nmea_Coord_To_Double(@(pGGA^.lon), VPoint.X) and
                 Nmea_Coord_To_Double(@(pGGA^.lat), VPoint.Y);

    VUTCTimeOK:=Nmea_Time_To_DateTime(@(pGGA^.time), VUTCTime);

    _UpdatePosTime(
      VPositionOK,
      VPoint,
      VUTCTimeOK,
      VUTCTime
     );

    _UpdateFixStatus(2*Ord(pGGA^.quality>0)); // set 0 or 2

    VNmea23Mode:=#0;
    case pGGA^.quality of
    {cNmea_Autonomous_Mode:
      VNmea23Mode:='A';
    cNmea_PPS_Mode:
      VNmea23Mode:='P';}
    cNmea_DGPS_SPS_Mode:
      VNmea23Mode:='D';
    cNmea_Dead_Reckoning_Mode:
      VNmea23Mode:='E';
    end;
    _UpdateNmea23Mode(VNmea23Mode, FALSE, TRUE);
    
    _UpdateHDOP(pGGA^.hdop);

    _UpdateAlt(pGGA^.alt_from_msl);
    _UpdateGeoidHeight(pGGA^.msl_above_ellipsoid);

    _UpdateDGPSParams(pGGA^.dgps_station_id, pGGA^.dgps_age_second);
  finally
    UnLockGPSData(AUnitIndex, FNotify_on_GGA, FALSE);
  end;
end;

procedure TGPSModuleByVSAGPS.GPSRecv_NMEA_GLL(const AUnitIndex: Byte; const pGLL: PNMEA_GLL);
var
  VPoint: TDoublePoint;
  VPositionOK: Boolean;
  VUTCTimeOK: Boolean;
  VUTCTime: TDateTime;
begin
  if (nil=pGLL) or (sizeof(pGLL^)<>pGLL^.dwSize) then
    Exit;
  LockGPSData;
  try
    VPositionOK:=Nmea_Coord_To_Double(@(pGLL^.lon), VPoint.X) and
                 Nmea_Coord_To_Double(@(pGLL^.lat), VPoint.Y);

    VUTCTimeOK:=Nmea_Time_To_DateTime(@(pGLL^.time), VUTCTime);

    _UpdatePosTime(
      VPositionOK,
      VPoint,
      VUTCTimeOK,
      VUTCTime);

    _UpdateNavMode(pGLL^.status);
    _UpdateNmea23Mode(pGLL^.nmea23_mode, TRUE, FALSE);
  finally
    UnLockGPSData(AUnitIndex, FNotify_on_GLL, FALSE);
  end;
end;

procedure TGPSModuleByVSAGPS.GPSRecv_NMEA_GSA(const AUnitIndex: Byte; const pGSA: PNMEA_GSA);
var
  VSourceTalkerID: AnsiString;
begin
  if (nil=pGSA) or (sizeof(pGSA^)<>pGSA^.dwSize) then
    Exit;
  LockGPSData;
  try
    VSourceTalkerID:=NMEA_TalkerID_to_String(@(pGSA^.chCorrectedTalkerID));

    // if QZSS - skip (no data)
    if SameText(VSourceTalkerID, nmea_ti_QZSS) then
      Exit;

    // if no corrected talker_id - skip data
    if (0=Length(VSourceTalkerID)) then
      Exit;

    _UpdateDOP(pGSA^.hdop, pGSA^.vdop, pGSA^.pdop);
    _UpdateFixedSats(VSourceTalkerID, @(pGSA^.sat_fix));
    // Dimentions
    _UpdateDimentions(pGSA^.fix_mode); // 0 - 1 - 2 - 3
    // and fixstatus
    _UpdateFixStatus(2*Ord(pGSA^.fix_mode>1)); // 0 or 2
  finally
    UnLockGPSData(AUnitIndex, FNotify_on_GSA, TRUE);
  end;
end;

end.
