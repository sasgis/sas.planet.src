{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_GPSModuleAbstract;

interface

uses
  ActiveX,
  Classes,
  SyncObjs,
  i_JclNotify,
  t_GeoTypes,
  i_GPS,
  i_GPSPositionFactory,
  i_GPSModule;

type
  TSatellitesInternalList = class
  private
    FList: TList;
    function Get(Index: Integer): IGPSSatelliteInfo;
    procedure Put(Index: Integer; Item: IGPSSatelliteInfo);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetList: PUnknownList;
  public
    constructor Create();
    destructor Destroy; override;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: IGPSSatelliteInfo read Get write Put; default;
    property List: PUnknownList read GetList;
  end;

  TGPSModuleAbstract = class(TInterfacedObject, IGPSModule)
  private
    FGPSPositionFactory: IGPSPositionFactory;
    FCS: TCriticalSection;
    FPosChanged: Boolean;
    FSatellitesChanged: Boolean;
    FLastStaticPosition: IGPSPosition;

    FPosition: TDoublePoint;
    FAltitude: Double;
    FSpeed_KMH: Double;
    FHeading: Double;
    FUTCDateTime: TDateTime;
    FLocalDateTime: TDateTime;
    FIsFix: Word;
    FHDOP: Double;
    FVDOP: Double;
    FPDOP: Double;
    FFixCount: Integer;
    FSatellites: TSatellitesInternalList;

    FDataReciveNotifier: IJclNotifier;

    FConnectingNotifier: IJclNotifier;
    FConnectedNotifier: IJclNotifier;
    FDisconnectingNotifier: IJclNotifier;
    FDisconnectedNotifier: IJclNotifier;

    FConnectErrorNotifier: IJclNotifier;
    FTimeOutNotifier: IJclNotifier;
  protected
    procedure _UpdatePosition(
      APosition: TDoublePoint;
      AAltitude: Double;
      ASpeed_KMH: Double;
      AHeading: Double;
      AUTCDateTime: TDateTime;
      ALocalDateTime: TDateTime;
      AIsFix: Word;
      AHDOP: Double;
      AVDOP: Double;
      APDOP: Double
    );
    procedure _UpdateSatellitesCount(
      AFixCount: Integer;
      ACount: Integer
    );
    procedure _UpdateSattelite(
      AIndex: Integer;
      APseudoRandomCode: Integer;
      AElevation: Integer;
      AAzimuth: Integer;
      ASignalToNoiseRatio: Integer;
      AIsFix: Boolean
    );
    procedure _UpdateToEmptyPosition;
    procedure Lock;
    procedure UnLock;
  protected
    function GetPosition: IGPSPosition; virtual; safecall;

    function GetDataReciveNotifier: IJclNotifier; virtual; safecall;

    function GetConnectingNotifier: IJclNotifier; virtual; safecall;
    function GetConnectedNotifier: IJclNotifier; virtual; safecall;
    function GetDisconnectingNotifier: IJclNotifier; virtual; safecall;
    function GetDisconnectedNotifier: IJclNotifier; virtual; safecall;

    function GetConnectErrorNotifier: IJclNotifier; virtual; safecall;
    function GetTimeOutNotifier: IJclNotifier; virtual; safecall;
  public
    constructor Create(AGPSPositionFactory: IGPSPositionFactory);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_JclNotify;

{ TGPSPositionUpdatable }

constructor TGPSModuleAbstract.Create(AGPSPositionFactory: IGPSPositionFactory);
var
  VPoint: TDoublePoint;
begin
  FGPSPositionFactory := AGPSPositionFactory;

  FCS := TCriticalSection.Create;

  FSatellites := TSatellitesInternalList.Create;
  FSatellites.Capacity := 32;
  FPosChanged := True;
  FSatellitesChanged := True;
  FLastStaticPosition := nil;
  FConnectErrorNotifier := TJclBaseNotifier.Create;

  FConnectingNotifier := TJclBaseNotifier.Create;
  FConnectedNotifier := TJclBaseNotifier.Create;
  FDisconnectingNotifier := TJclBaseNotifier.Create;
  FDisconnectedNotifier := TJclBaseNotifier.Create;

  FDataReciveNotifier := TJclBaseNotifier.Create;
  FTimeOutNotifier := TJclBaseNotifier.Create;
  VPoint.X := 0;
  VPoint.Y := 0;
end;

destructor TGPSModuleAbstract.Destroy;
begin
  FreeAndNil(FCS);
  FreeAndNil(FSatellites);
  FLastStaticPosition := nil;
  FGPSPositionFactory := nil;

  FConnectingNotifier := nil;
  FConnectedNotifier := nil;
  FDisconnectingNotifier := nil;
  FDisconnectedNotifier := nil;

  FConnectErrorNotifier := nil;
  FDataReciveNotifier := nil;
  FTimeOutNotifier := nil;
  inherited;
end;

function TGPSModuleAbstract.GetConnectErrorNotifier: IJclNotifier;
begin
  Result := FConnectErrorNotifier;
end;

function TGPSModuleAbstract.GetConnectingNotifier: IJclNotifier;
begin
  Result := FConnectingNotifier;
end;

function TGPSModuleAbstract.GetConnectedNotifier: IJclNotifier;
begin
  Result := FConnectedNotifier;
end;

function TGPSModuleAbstract.GetDisconnectingNotifier: IJclNotifier;
begin
  Result := FDisconnectingNotifier;
end;

function TGPSModuleAbstract.GetDisconnectedNotifier: IJclNotifier;
begin
  Result := FDisconnectedNotifier;
end;

function TGPSModuleAbstract.GetDataReciveNotifier: IJclNotifier;
begin
  Result := FDataReciveNotifier;
end;

function TGPSModuleAbstract.GetPosition: IGPSPosition;
begin
  Lock;
  try
    if FSatellitesChanged or FPosChanged then begin
      if not FSatellitesChanged then begin
        Result := FGPSPositionFactory.BuildPosition(
          FPosition,
          FAltitude,
          FSpeed_KMH,
          FHeading,
          FUTCDateTime,
          FLocalDateTime,
          FIsFix,
          FHDOP,
          FVDOP,
          FPDOP,
          FLastStaticPosition.Satellites
        );
      end else begin
        Result := FGPSPositionFactory.BuildPosition(
          FPosition,
          FAltitude,
          FSpeed_KMH,
          FHeading,
          FUTCDateTime,
          FLocalDateTime,
          FIsFix,
          FHDOP,
          FVDOP,
          FPDOP,
          FGPSPositionFactory.BuildSatellitesInView(
            FFixCount,
            FSatellites.Count,
            FSatellites.List
          )
        );
      end;
      FLastStaticPosition := Result;
    end else begin
      Result := FLastStaticPosition;
    end;
  finally
    UnLock;
  end;
end;

function TGPSModuleAbstract.GetTimeOutNotifier: IJclNotifier;
begin
  Result := FTimeOutNotifier;
end;

procedure TGPSModuleAbstract.Lock;
begin
  FCS.Acquire;
end;

procedure TGPSModuleAbstract.UnLock;
begin
  FCS.Release;
end;

procedure TGPSModuleAbstract._UpdatePosition(APosition: TDoublePoint;
  AAltitude, ASpeed_KMH, AHeading: Double; AUTCDateTime,
  ALocalDateTime: TDateTime; AIsFix: Word; AHDOP, AVDOP, APDOP: Double);
begin
  if FPosition.X <> APosition.X then begin
    FPosition.X := APosition.X;
    FPosChanged := True;
  end;
  if FPosition.Y <> APosition.Y then begin
    FPosition.Y := APosition.Y;
    FPosChanged := True;
  end;

  if FAltitude <> AAltitude then begin
    FAltitude := AAltitude;
    FPosChanged := True;
  end;

  if FSpeed_KMH <> ASpeed_KMH then begin
    FSpeed_KMH := ASpeed_KMH;
    FPosChanged := True;
  end;

  if FHeading <> AHeading then begin
    FHeading := AHeading;
    FPosChanged := True;
  end;

  if FUTCDateTime <> AUTCDateTime then begin
    FUTCDateTime := AUTCDateTime;
    FPosChanged := True;
  end;

  if FLocalDateTime <> ALocalDateTime then begin
    FLocalDateTime := ALocalDateTime;
    FPosChanged := True;
  end;

  if FIsFix <> AIsFix then begin
    FIsFix := AIsFix;
    FPosChanged := True;
  end;

  if FHDOP <> AHDOP then begin
    FHDOP := AHDOP;
    FPosChanged := True;
  end;

  if FVDOP <> AVDOP then begin
    FVDOP := AVDOP;
    FPosChanged := True;
  end;

  if FPDOP <> APDOP then begin
    FPDOP := APDOP;
    FPosChanged := True;
  end;
end;

procedure TGPSModuleAbstract._UpdateSatellitesCount(AFixCount,
  ACount: Integer);
begin
  if FFixCount <> AFixCount then begin
    FFixCount := AFixCount;
    FSatellitesChanged := True;
  end;
  if FSatellites.Count <> ACount then begin
    FSatellites.Count := ACount;
    FSatellitesChanged := True;
  end;
end;

procedure TGPSModuleAbstract._UpdateSattelite(AIndex, APseudoRandomCode,
  AElevation, AAzimuth, ASignalToNoiseRatio: Integer; AIsFix: Boolean);
var
  VSattelite: IGPSSatelliteInfo;
  VSatteliteChanged: Boolean;
begin
  VSattelite := FSatellites[AIndex];
  VSatteliteChanged := False;
  if VSattelite <> nil then begin
    if VSattelite.PseudoRandomCode <> APseudoRandomCode then begin
      VSatteliteChanged := True;
    end;
    if VSattelite.Elevation <> AElevation then begin
      VSatteliteChanged := True;
    end;
    if VSattelite.Azimuth <> AAzimuth then begin
      VSatteliteChanged := True;
    end;
    if VSattelite.SignalToNoiseRatio <> ASignalToNoiseRatio then begin
      VSatteliteChanged := True;
    end;
    if VSattelite.IsFix <> AIsFix then begin
      VSatteliteChanged := True;
    end;
  end else begin
    VSatteliteChanged := True;
  end;

  if VSatteliteChanged then begin
    VSattelite := FGPSPositionFactory.BuildSatelliteInfo(
      APseudoRandomCode,
      AElevation,
      AAzimuth,
      ASignalToNoiseRatio,
      AIsFix
    );
    FSatellites[AIndex] := VSattelite;
    FSatellitesChanged := True;
  end;
end;

procedure TGPSModuleAbstract._UpdateToEmptyPosition;
begin
  FPosChanged := False;
  FSatellitesChanged := False;
  FLastStaticPosition := FGPSPositionFactory.BuildPositionEmpty;
end;

{ TSatellitesInternalList }

constructor TSatellitesInternalList.Create;
begin
  FList := TList.Create;;
end;

destructor TSatellitesInternalList.Destroy;
begin
  SetCount(0);
  FreeAndNil(FList);
  inherited;
end;

function TSatellitesInternalList.Get(Index: Integer): IGPSSatelliteInfo;
begin
  Result := IGPSSatelliteInfo(FList.Items[Index]);
end;

function TSatellitesInternalList.GetCapacity: Integer;
begin
  Result := FList.Capacity;
end;

function TSatellitesInternalList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TSatellitesInternalList.GetList: PUnknownList;
begin
  Result := PUnknownList(FList.List);
end;

procedure TSatellitesInternalList.Put(Index: Integer; Item: IGPSSatelliteInfo);
begin
  if (Index >= 0) and (Index < FList.Count) then begin
    IInterface(FList.List[Index]) := Item;
  end;
end;

procedure TSatellitesInternalList.SetCapacity(NewCapacity: Integer);
var
  i: Integer;
begin
  if FList.Count > NewCapacity then begin
    for i := NewCapacity to FList.Count - 1 do begin
      IInterface(FList.List[i]) := nil;
    end;
  end;
  FList.Capacity := NewCapacity;
end;

procedure TSatellitesInternalList.SetCount(NewCount: Integer);
var
  i: Integer;
begin
  if FList.Count > NewCount then begin
    for i := NewCount to FList.Count - 1 do begin
      IInterface(FList.List[i]) := nil;
    end;
  end;
  FList.Count := NewCount;
end;

end.
