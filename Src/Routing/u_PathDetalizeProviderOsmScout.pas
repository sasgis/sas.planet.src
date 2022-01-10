{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_PathDetalizeProviderOsmScout;

interface

uses
  Math,
  SysUtils,
  libosmscout_route,
  t_GeoTypes,
  i_NotifierTime,
  i_NotifierOperation,
  i_PathDetalizeConfig,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_DoublePointsAggregator,
  u_PathDetalizeProviderBase;

type
  IOsmScoutRouteContext = interface
    ['{60163DE0-6A10-488B-86E5-5ED82F462F6D}']
    function Acquire: Pointer;
    procedure Release;
  end;

  TPathDetalizeProviderOsmScout = class(TPathDetalizeProviderBase)
  private
    FOpt: Pointer;
    FCtx: Pointer;
    FProfile: TRouteProfile;
    FOsmScoutRouteContext: IOsmScoutRouteContext;

    {$IF CompilerVersion >= 33}
    FExceptionMask: TArithmeticExceptionMask;
    {$ELSE}
    FExceptionMask: TFPUExceptionMask;
    {$IFEND}
  protected
    function ProcessSingleLine(
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer;
      const ASource: IGeometryLonLatSingleLine;
      const APointsAggregator: IDoublePointsAggregator;
      const ABuilder: IGeometryLonLatLineBuilder;
      out AErrorMessage: string
    ): Boolean; override;
    procedure OnBeforeGetRoute; override;
    procedure OnAfterGetRoute; override;
  public
    constructor Create(
      const AProfile: TRouteProfile;
      const AOsmScoutRouteContext: IOsmScoutRouteContext;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
    );
  end;

  EPathDetalizeProviderOsmScout = class(Exception);

function NewOsmScoutRouteContext(
  const AGCNotifier: INotifierTime;
  const APathDetalizeConfig: IPathDetalizeConfig;
  const ADatabasePath: string
): IOsmScoutRouteContext;

implementation

uses
  SyncObjs,
  i_ListenerTime,
  u_ListenerTime,
  u_StrFunc,
  u_BaseInterfacedObject;

resourcestring
  rsNoDataStart = 'There is no data in database for start location!';
  rsNoDataTarget = 'There is no data in database for target location!';

  rsNoDataRoute = 'The route cannot be built for the given profile.' + #13#10 +
                  'Not enough data in the database!';

type
  TOsmScoutRouteContext = class(TBaseInterfacedObject, IOsmScoutRouteContext)
  private
    FOpt: Pointer;
    FCtx: Pointer;
    FLock: TCriticalSection;
    FDatabasePath: AnsiString;
    FIsCtxInitialized: Boolean;
    FIsOptInitialized: Boolean;

    FNotifier: INotifierTime;
    FListener: IListenerTimeWithUsedFlag;
    FDeleteCtxByTimeOut: Boolean;

    procedure OnGarbageCollectionNotify;

    procedure ClearCtx; inline;
    procedure DeleteCtx; inline;

    procedure DeleteOpt; inline;
  private
    { IOsmScoutRouteContext }
    function Acquire: Pointer;
    procedure Release;
  public
    constructor Create(
      const AGCNotifier: INotifierTime;
      const APathDetalizeConfig: IPathDetalizeConfig;
      const ADatabasePath: string
    );
    destructor Destroy; override;
  end;

function NewOsmScoutRouteContext(
  const AGCNotifier: INotifierTime;
  const APathDetalizeConfig: IPathDetalizeConfig;
  const ADatabasePath: string
): IOsmScoutRouteContext;
begin
  Result := TOsmScoutRouteContext.Create(
    AGCNotifier,
    APathDetalizeConfig,
    ADatabasePath
  );
end;

{ TPathDetalizeProviderOSMScout }

constructor TPathDetalizeProviderOsmScout.Create(
  const AProfile: TRouteProfile;
  const AOsmScoutRouteContext: IOsmScoutRouteContext;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
);
begin
  inherited Create('', nil, nil, AVectorGeometryLonLatFactory);

  FOpt := nil;
  FCtx := nil;
  FProfile := AProfile;
  FOsmScoutRouteContext := AOsmScoutRouteContext;
end;

procedure TPathDetalizeProviderOsmScout.OnBeforeGetRoute;
begin
  FCtx := FOsmScoutRouteContext.Acquire;
  FExceptionMask := Math.SetExceptionMask(
    [exInvalidOp, exDenormalized, exUnderflow, exPrecision]
  );
end;

procedure TPathDetalizeProviderOsmScout.OnAfterGetRoute;
begin
  FCtx := nil;
  Math.SetExceptionMask(FExceptionMask);
  FOsmScoutRouteContext.Release;
end;

function TPathDetalizeProviderOsmScout.ProcessSingleLine(
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Integer;
  const ASource: IGeometryLonLatSingleLine;
  const APointsAggregator: IDoublePointsAggregator;
  const ABuilder: IGeometryLonLatLineBuilder;
  out AErrorMessage: string
): Boolean;
var
  I: Integer;
  VCount: uint32_t;
  VRoutePoint: ppoint_t;
  VResult: TRouterResult;
  VPoints: PDoublePointArray;
begin
  Assert(FCtx <> nil);
  Result := False;
  VPoints := ASource.Points;

  for I := 1 to ASource.Count - 1 do begin

    VResult :=
      router.calc(
        FCtx,
        FProfile,
        ppoint_t(@VPoints[I-1]),
        ppoint_t(@VPoints[I]),
        VCount,
        VRoutePoint
      );

    case VResult of
      ROUTER_RESULT_OK: begin
        APointsAggregator.AddPoints(PDoublePointArray(VRoutePoint), VCount);
      end;

      ROUTER_RESULT_NODATA_START: begin
        AErrorMessage := rsNoDataStart;
        Exit;
      end;

      ROUTER_RESULT_NODATA_TARGET: begin
        AErrorMessage := rsNoDataTarget;
        Exit;
      end;

      ROUTER_RESULT_NODATA_ROUTE: begin
        AErrorMessage := rsNoDataRoute;
        Exit;
      end;

      ROUTER_RESULT_ERROR: begin
        RiseLibOsmScoutError(FCtx, 'calc');
      end;
    else
      raise EPathDetalizeProviderOsmScout.CreateFmt(
        'Function "router_calc" returns unexpected result: %d', [Integer(VResult)]
      );
    end;
  end;

  if APointsAggregator.Count > 0 then begin
    ABuilder.AddLine(APointsAggregator.MakeStaticAndClear);
  end;

  Result := True;
end;

{ TOsmScoutRouteContext }

constructor TOsmScoutRouteContext.Create(
  const AGCNotifier: INotifierTime;
  const APathDetalizeConfig: IPathDetalizeConfig;
  const ADatabasePath: string
);
var
  VTimeOut: Integer;
begin
  inherited Create;

  if SizeOf(TDoublePoint) <> SizeOf(libosmscout_route.point_t) then begin
    raise EPathDetalizeProviderOsmScout.Create('Point types mismatch!');
  end;

  FDatabasePath := StringToAnsiSafe(IncludeTrailingPathDelimiter(ADatabasePath));

  FOpt := nil;
  FCtx := nil;

  FIsOptInitialized := False;
  FIsCtxInitialized := False;

  FLock := TCriticalSection.Create;

  VTimeOut := APathDetalizeConfig.GarbageCollectionTimeOut;
  if VTimeOut > 0 then begin
    FListener := TListenerTTLCheck.Create(Self.OnGarbageCollectionNotify, VTimeOut);
    FNotifier := AGCNotifier;
    FNotifier.Add(FListener);
  end else begin
    FNotifier := nil;
    FListener := nil;
  end;
  FDeleteCtxByTimeOut := False;
end;

destructor TOsmScoutRouteContext.Destroy;
begin
  if Assigned(FNotifier) and Assigned(FListener) then begin
    FNotifier.Remove(FListener);
  end;

  DeleteCtx;
  DeleteOpt;

  FreeAndNil(FLock);

  inherited Destroy;
end;

procedure TOsmScoutRouteContext.ClearCtx;
begin
  if FCtx <> nil then begin
    router.clear(FCtx);
  end;
end;

procedure TOsmScoutRouteContext.DeleteCtx;
begin
  if FCtx <> nil then begin
    router.del(FCtx);
    FCtx := nil;
  end;
end;

procedure TOsmScoutRouteContext.DeleteOpt;
begin
  if FOpt <> nil then begin
    router.opt_del(FOpt);
    FOpt := nil;
  end;
end;

function TOsmScoutRouteContext.Acquire: Pointer;

  procedure OptInitialize;
  var
    I: Integer;
    VDataBasesCount: Integer;
    VDataBases: array of AnsiString;
    VDataBasesArr: array of PAnsiChar;
    VSearchRec: TSearchRec;
    VResult: TRouterResult;
  begin
    // collect subfolders names
    VDataBasesCount := 0;
    if FindFirst(string(FDatabasePath) + '*.*', faAnyFile, VSearchRec) = 0 then
    try
      repeat
        if (VSearchRec.Attr and faDirectory) = 0 then begin
          // not a folder, ignore it
          Continue;
        end;

        if Pos('.', VSearchRec.Name) = 1 then begin
          // ignore folders beginnig with dot (include "." and "..")
          Continue;
        end;

        SetLength(VDataBases, VDataBasesCount + 1);
        VDataBases[VDataBasesCount] := FDatabasePath + StringToAnsiSafe(VSearchRec.Name);

        Inc(VDataBasesCount);
      until FindNext(VSearchRec) <> 0;
    finally
      FindClose(VSearchRec);
    end;

    if VDataBasesCount = 0 then begin
      // no subfolders found, use root folder as a database
      SetLength(VDataBases, 1);
      VDataBases[0] := FDatabasePath;
      Inc(VDataBasesCount);
    end;

    SetLength(VDataBasesArr, VDataBasesCount);
    for I := 0 to VDataBasesCount - 1 do begin
      VDataBasesArr[I] := PAnsiChar(VDataBases[I]);
    end;

    // initilize options
    VResult := router.opt_new(FOpt);
    if VResult <> ROUTER_RESULT_OK then begin
      try
        RiseLibOsmScoutError(nil, 'opt_new');
      finally
        DeleteOpt;
      end;
    end;

    VResult := router.opt_set_dbpath(FOpt, @VDataBasesArr[0], VDataBasesCount);
    if VResult <> ROUTER_RESULT_OK then begin
      try
        RiseLibOsmScoutError(nil, 'opt_set_dbpath');
      finally
        DeleteOpt;
      end;
    end;

    FIsOptInitialized := True;
  end;

begin
  FLock.Acquire;
  try
    if not FIsOptInitialized then begin
      OptInitialize;
    end;

    if not FIsCtxInitialized then begin
      if router.new(FCtx, FOpt) = ROUTER_RESULT_OK then begin
        FIsCtxInitialized := True;
      end else begin
        try
          RiseLibOsmScoutError(FCtx, 'new');
        finally
          DeleteCtx;
        end;
      end;
    end;

    Result := FCtx;
  except
    FLock.Release;
    raise;
  end;
end;

procedure TOsmScoutRouteContext.Release;
begin
  try
    if Assigned(FListener) then begin
      FListener.UpdateUseTime;
      FDeleteCtxByTimeOut := False;
    end else begin
      ClearCtx;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TOsmScoutRouteContext.OnGarbageCollectionNotify;
begin
  if FLock.TryEnter then
  try
    ClearCtx;
    if FDeleteCtxByTimeOut then begin
      DeleteCtx;
      FIsCtxInitialized := False;
    end else begin
      FListener.UpdateUseTime;
      FDeleteCtxByTimeOut := True;
    end;
  finally
    FLock.Release;
  end;
end;

end.
