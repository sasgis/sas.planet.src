{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2020, SAS.Planet development team.                      *}
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
  i_NotifierOperation,
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
    function ProcessSinglePath(
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer;
      const ASource: IGeometryLonLatSingleLine;
      const APointsAggregator: IDoublePointsAggregator;
      const ABuilder: IGeometryLonLatLineBuilder
    ): Boolean; override;
    procedure OnBeforeGetPath; override;
    procedure OnAfterGetPath; override;
  public
    constructor Create(
      const AProfile: TRouteProfile;
      const AOsmScoutRouteContext: IOsmScoutRouteContext;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
    );
  end;

  EPathDetalizeProviderOsmScout = class(Exception);

function NewOsmScoutRouteContext(
  const ADatabasePath: string
): IOsmScoutRouteContext;

implementation

uses
  SyncObjs,
  u_StrFunc,
  u_BaseInterfacedObject;

type
  TOsmScoutRouteContext = class(TBaseInterfacedObject, IOsmScoutRouteContext)
  private
    FCtx: Pointer;
    FLock: TCriticalSection;
    FDatabasePath: AnsiString;
    FIsInitialized: Boolean;
    procedure DeleteCtx;
  private
    { IOsmScoutRouteContext }
    function Acquire: Pointer;
    procedure Release;
  public
    constructor Create(const ADatabasePath: string);
    destructor Destroy; override;
  end;

function NewOsmScoutRouteContext(
  const ADatabasePath: string
): IOsmScoutRouteContext;
begin
  Result := TOsmScoutRouteContext.Create(ADatabasePath);
end;

{ TPathDetalizeProviderOSMScout }

constructor TPathDetalizeProviderOsmScout.Create(
  const AProfile: TRouteProfile;
  const AOsmScoutRouteContext: IOsmScoutRouteContext;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
);
begin
  inherited Create('', nil, nil, AVectorGeometryLonLatFactory);

  FProfile := AProfile;
  FOsmScoutRouteContext := AOsmScoutRouteContext;
end;

procedure TPathDetalizeProviderOsmScout.OnBeforeGetPath;
begin
  FCtx := FOsmScoutRouteContext.Acquire;
  FExceptionMask := Math.SetExceptionMask(
    [exInvalidOp, exDenormalized, exUnderflow, exPrecision]
  );
end;

procedure TPathDetalizeProviderOsmScout.OnAfterGetPath;
begin
  if FCtx <> nil then begin
    router.clear(FCtx);
  end;
  FCtx := nil;
  Math.SetExceptionMask(FExceptionMask);
  FOsmScoutRouteContext.Release;
end;

function TPathDetalizeProviderOsmScout.ProcessSinglePath(
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

constructor TOsmScoutRouteContext.Create(const ADatabasePath: string);
begin
  inherited Create;

  if SizeOf(TDoublePoint) <> SizeOf(libosmscout_route.point_t) then begin
    raise EPathDetalizeProviderOsmScout.Create('Point types mismatch!');
  end;

  FDatabasePath := StringToAnsiSafe(ADatabasePath);

  FCtx := nil;
  FIsInitialized := False;
  FLock := TCriticalSection.Create;
end;

destructor TOsmScoutRouteContext.Destroy;
begin
  DeleteCtx;
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TOsmScoutRouteContext.DeleteCtx;
begin
  if FCtx <> nil then begin
    router.del(FCtx);
    FCtx := nil;
  end;
end;

function TOsmScoutRouteContext.Acquire: Pointer;
begin
  FLock.Acquire;
  try
    if not FIsInitialized then begin
      FIsInitialized := True;
      if not router.new(FCtx, PAnsiChar(FDatabasePath)) then begin
        try
          RiseLibOsmScoutError(FCtx, 'new');
        finally
          DeleteCtx;
        end;
      end;
    end;
    if FCtx = nil then begin
      raise EPathDetalizeProviderOsmScout.Create('Context is not assigned!');
    end;
    Result := FCtx;
  except
    FLock.Release;
    Result := nil;
  end;
end;

procedure TOsmScoutRouteContext.Release;
begin
  FLock.Release;
end;

end.
