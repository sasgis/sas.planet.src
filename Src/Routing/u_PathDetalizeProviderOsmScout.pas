{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
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
      const ABuilder: IGeometryLonLatLineBuilder
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

type
  TOsmScoutRouteContext = class(TBaseInterfacedObject, IOsmScoutRouteContext)
  private
    FCtx: Pointer;
    FLock: TCriticalSection;
    FDatabasePath: AnsiString;
    FIsInitialized: Boolean;

    FNotifier: INotifierTime;
    FListener: IListenerTimeWithUsedFlag;
    FDeleteCtxByTimeOut: Boolean;

    procedure OnGarbageCollectionNotify;

    procedure ClearCtx; inline;
    procedure DeleteCtx; inline;
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
  const ABuilder: IGeometryLonLatLineBuilder
): Boolean;
var
  I: Integer;
  VCount: uint32_t;
  VRoutePoint: ppoint_t;
  VCalcResult: TRouteCalcResult;
  VPoints: PDoublePointArray;
begin
  Assert(FCtx <> nil);
  VPoints := ASource.Points;

  for I := 1 to ASource.Count - 1 do begin

    VCalcResult :=
      router.calc(
        FCtx,
        FProfile,
        ppoint_t(@VPoints[I-1]),
        ppoint_t(@VPoints[I]),
        VCount,
        VRoutePoint
      );

    case VCalcResult of
      CALC_RESULT_OK: begin
        APointsAggregator.AddPoints(PDoublePointArray(VRoutePoint), VCount);
      end;
      CALC_RESULT_NODATA: begin
        // There is no data in database for this location
        Break;
      end;
      CALC_RESULT_ERROR: begin
        RiseLibOsmScoutError(FCtx, 'calc');
      end;
    else
      raise EPathDetalizeProviderOsmScout.CreateFmt(
        '"router_calc" returns unexpected value: %d', [Integer(VCalcResult)]
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

  FCtx := nil;
  FIsInitialized := False;
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

function TOsmScoutRouteContext.Acquire: Pointer;
var
  I: Integer;
  VDataBases: array of AnsiString;
  VDataBasesArr: array of PAnsiChar;
  VSearchRec: TSearchRec;
begin
  FLock.Acquire;

  if not FIsInitialized then begin
    FIsInitialized := True;

    // collect db names (folder == db)
    I := 0;
    if FindFirst(string(FDatabasePath) + '*.*', faAnyFile, VSearchRec) = 0 then
    try
      repeat
        if (VSearchRec.Attr and faDirectory) = 0 then begin
          // ignore all except folders
          Continue;
        end;

        if Pos('.', VSearchRec.Name) = 1 then begin
          // ignore folders beginnig with dot (include "." and "..")
          Continue;
        end;

        SetLength(VDataBases, I+1);
        SetLength(VDataBasesArr, I+1);

        VDataBases[I] := FDatabasePath + StringToAnsiSafe(VSearchRec.Name);
        VDataBasesArr[I] := PAnsiChar(VDataBases[I]);

        Inc(I);
      until FindNext(VSearchRec) <> 0;
    finally
      FindClose(VSearchRec);
    end;

    if I = 0 then begin
      // db is a root folder
      SetLength(VDataBases, 1);
      VDataBases[I] := FDatabasePath;
      Inc(I);
    end;

    // open
    if I = 1 then begin
      if not router.new(FCtx, PAnsiChar(VDataBases[0])) then begin
        try
          RiseLibOsmScoutError(FCtx, 'new');
        finally
          DeleteCtx;
        end;
      end;
    end else begin
      if not router.new_multi(FCtx, @VDataBasesArr[0], I) then begin
        try
          RiseLibOsmScoutError(FCtx, 'new_multi');
        finally
          DeleteCtx;
        end;
      end;
    end;
  end;

  if FCtx = nil then begin
    raise EPathDetalizeProviderOsmScout.Create('Context is not assigned!');
  end;

  Result := FCtx;
end;

procedure TOsmScoutRouteContext.Release;
begin
  if Assigned(FListener) then begin
    FListener.UpdateUseTime;
    FDeleteCtxByTimeOut := False;
  end else begin
    ClearCtx;
  end;

  FLock.Release;
end;

procedure TOsmScoutRouteContext.OnGarbageCollectionNotify;
begin
  if FLock.TryEnter then
  try
    ClearCtx;
    if FDeleteCtxByTimeOut then begin
      DeleteCtx;
      FIsInitialized := False;
    end else begin
      FListener.UpdateUseTime;
      FDeleteCtxByTimeOut := True;
    end;
  finally
    FLock.Release;
  end;
end;

end.
