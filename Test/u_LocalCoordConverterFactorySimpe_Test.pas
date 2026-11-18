{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit u_LocalCoordConverterFactorySimpe_Test;

interface

uses
  Types,
  TestFramework,
  t_GeoTypes,
  i_Projection,
  i_ProjectionSet,
  i_ProjectionSetFactory,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe;

type
  TestLocalCoordConverterFactorySimpe = class(TTestCase)
  private
    FProjectionSet: IProjectionSet;
    FProjectionSetFactory: IProjectionSetFactory;
    FConverterFactory: ILocalCoordConverterFactorySimpe;

    procedure CheckTopLeftByConverter(
      const AConverter: ILocalCoordConverter;
      const ATopLeft: TDoublePoint;
      const AMsg: string
    );
    procedure DoCheckDragPreservesTopLeft(
      const ALocalRect: TRect
    );
  protected
    procedure SetUp; override;
  published
    // Regression: dragging the map on a window with odd size used to shift the
    // map view by a pixel on every second mouse move (the map "ran away" from
    // the cursor), because the integer converter branch rounded the half
    // integer window center (e.g. 289.5 for the height 579) and restored the
    // top left from the rounded center.
    procedure TestDragByLocalDeltaPreservesTopLeftOddHeight;
    procedure TestDragByLocalDeltaPreservesTopLeftEvenHeight;
    procedure TestDragByLocalDeltaPreservesFractionalTopLeft;
    procedure TestCreateConverterClampsCenterAtWorldEdge;
  end;

implementation

uses
  SysUtils,
  c_CoordConverter,
  i_HashFunction,
  i_DatumFactory,
  i_InternalPerformanceCounter,
  u_GeoFunc,
  u_DatumFactory,
  u_HashFunctionCityHash,
  u_HashFunctionWithCounter,
  u_InternalPerformanceCounterFake,
  u_LocalCoordConverterFactorySimpe,
  u_LocalCoordConverterFactory,
  u_ProjectionSetFactorySimple;

{ TestLocalCoordConverterFactorySimpe }

procedure TestLocalCoordConverterFactorySimpe.SetUp;
var
  VHashFunction: IHashFunction;
  VDatumFactory: IDatumFactory;
  VPerfCounterList: IInternalPerformanceCounterList;
begin
  inherited;

  VHashFunction :=
    THashFunctionWithCounter.Create(
      THashFunctionCityHash.Create,
      TInternalPerformanceCounterFake.Create
    );

  VDatumFactory := TDatumFactory.Create(VHashFunction);

  FProjectionSetFactory :=
    TProjectionSetFactorySimple.Create(
      VHashFunction,
      VDatumFactory
    );

  VPerfCounterList := TInternalPerformanceCounterFake.Create;

  FConverterFactory :=
    TLocalCoordConverterFactorySimpe.Create(
      TLocalCoordConverterFactory.Create(VPerfCounterList, VHashFunction)
    );
end;

procedure TestLocalCoordConverterFactorySimpe.CheckTopLeftByConverter(
  const AConverter: ILocalCoordConverter;
  const ATopLeft: TDoublePoint;
  const AMsg: string
);
var
  VTopLeft: TDoublePoint;
begin
  VTopLeft := AConverter.LocalPixel2MapPixelFloat(Point(0, 0));
  CheckTrue(
    (VTopLeft.X = ATopLeft.X) and (VTopLeft.Y = ATopLeft.Y),
    AMsg + ': expected (' + FloatToStr(ATopLeft.X) + ',' + FloatToStr(ATopLeft.Y) +
      '), got (' + FloatToStr(VTopLeft.X) + ',' + FloatToStr(VTopLeft.Y) + ')'
  );
end;

procedure TestLocalCoordConverterFactorySimpe.DoCheckDragPreservesTopLeft(
  const ALocalRect: TRect
);
var
  I: Integer;
  J: Integer;
  VDelta: TPoint;
  VProjection: IProjection;
  VConverter: ILocalCoordConverter;
  VTopLeft: TDoublePoint;
  VTopLeftExpected: TDoublePoint;
begin
  VProjection := FProjectionSet.Zooms[8];
  VTopLeft := DoublePoint(5001, 7003);
  for I := 0 to 3 do begin
    VConverter :=
      FConverterFactory.CreateConverter(
        ALocalRect,
        VProjection,
        1.0,
        VTopLeft
      );
    CheckTopLeftByConverter(VConverter, VTopLeft, 'Initial top left');
    for J := 0 to 5 do begin
      case J of
        0: VDelta := Point(0, -1);
        1: VDelta := Point(0, 1);
        2: VDelta := Point(0, -11);
        3: VDelta := Point(0, 11);
        4: VDelta := Point(-7, -23);
        5: VDelta := Point(13, 27);
      else
        VDelta := Point(0, 0);
      end;
      VConverter :=
        FConverterFactory.ChangeByLocalDelta(
          VConverter,
          DoublePoint(VDelta)
        );
      VTopLeftExpected.X := VTopLeft.X + VDelta.X;
      VTopLeftExpected.Y := VTopLeft.Y + VDelta.Y;
      CheckTopLeftByConverter(
        VConverter,
        VTopLeftExpected,
        'Drag by (' + IntToStr(VDelta.X) + ',' + IntToStr(VDelta.Y) + ')'
      );
      VTopLeft := VTopLeftExpected;
    end;
    // move to a new top left so that the next iteration starts from another
    // combination of even and odd coordinates
    VTopLeft.X := VTopLeft.X + 101;
    VTopLeft.Y := VTopLeft.Y + 37;
  end;
end;

procedure TestLocalCoordConverterFactorySimpe.TestDragByLocalDeltaPreservesTopLeftOddHeight;
begin
  FProjectionSet :=
    FProjectionSetFactory.GetProjectionSetByCode(
      CGoogleProjectionEPSG,
      CTileSplitQuadrate256x256
    );
  // the height 579 gives a half integer center (289.5)
  DoCheckDragPreservesTopLeft(Rect(0, 0, 1390, 579));
end;

procedure TestLocalCoordConverterFactorySimpe.TestDragByLocalDeltaPreservesTopLeftEvenHeight;
begin
  FProjectionSet :=
    FProjectionSetFactory.GetProjectionSetByCode(
      CGoogleProjectionEPSG,
      CTileSplitQuadrate256x256
    );
  DoCheckDragPreservesTopLeft(Rect(0, 0, 1024, 768));
end;

procedure TestLocalCoordConverterFactorySimpe.TestDragByLocalDeltaPreservesFractionalTopLeft;
var
  VProjection: IProjection;
  VConverter: ILocalCoordConverter;
  VConverterNew: ILocalCoordConverter;
  VTopLeft: TDoublePoint;
  VTopLeftExpected: TDoublePoint;
begin
  FProjectionSet :=
    FProjectionSetFactory.GetProjectionSetByCode(
      CGoogleProjectionEPSG,
      CTileSplitQuadrate256x256
    );
  VProjection := FProjectionSet.Zooms[8];
  VConverter :=
    FConverterFactory.CreateConverter(
      Rect(0, 0, 1390, 579),
      VProjection,
      1.0,
      DoublePoint(5000.25, 7000.75)
    );
  VConverterNew :=
    FConverterFactory.ChangeByLocalDelta(
      VConverter,
      DoublePoint(-3, 5)
    );
  VTopLeft := VConverterNew.LocalPixel2MapPixelFloat(Point(0, 0));
  VTopLeftExpected := DoublePoint(4997.25, 7005.75);
  CheckTrue(
    (VTopLeft.X = VTopLeftExpected.X) and (VTopLeft.Y = VTopLeftExpected.Y),
    'Fractional top left: expected (' + FloatToStr(VTopLeftExpected.X) + ',' +
      FloatToStr(VTopLeftExpected.Y) + '), got (' + FloatToStr(VTopLeft.X) + ',' +
      FloatToStr(VTopLeft.Y) + ')'
  );
end;

procedure TestLocalCoordConverterFactorySimpe.TestCreateConverterClampsCenterAtWorldEdge;
var
  VProjection: IProjection;
  VConverter: ILocalCoordConverter;
  VConverterNew: ILocalCoordConverter;
  VTopLeft: TDoublePoint;
begin
  // the world (256 pixels at zoom 0) is smaller than the window, the view
  // center is clamped to the world edges; the top left may still move within
  // the range where the center stays inside the world
  FProjectionSet :=
    FProjectionSetFactory.GetProjectionSetByCode(
      CGoogleProjectionEPSG,
      CTileSplitQuadrate256x256
    );
  VProjection := FProjectionSet.Zooms[0];
  VConverter :=
    FConverterFactory.CreateConverter(
      Rect(0, 0, 1390, 579),
      VProjection,
      1.0,
      DoublePoint(-439, -34)
    );
  CheckTopLeftByConverter(VConverter, DoublePoint(-439, -34), 'Initial top left at world edge');

  // dragging within the clamped range moves the view exactly by the delta
  VConverterNew :=
    FConverterFactory.ChangeByLocalDelta(
      VConverter,
      DoublePoint(-100, -100)
    );
  CheckTopLeftByConverter(VConverterNew, DoublePoint(-539, -134), 'Drag within clamped range');

  // dragging beyond the range stops the view at the world edge: the view
  // center is clamped to 0 and the top left is restored from it
  VConverterNew :=
    FConverterFactory.ChangeByLocalDelta(
      VConverterNew,
      DoublePoint(-200, -200)
    );
  VTopLeft := VConverterNew.LocalPixel2MapPixelFloat(Point(0, 0));
  // center.Y = top left Y + 289.5 is clamped to 0, so the top left Y is
  // -289.5 rounded to the even neighbor (-290); center.X = 0 gives top left
  // X = -695
  CheckTrue(
    (VTopLeft.X = -695) and (VTopLeft.Y = -290),
    'Clamped drag: expected (-695,-290), got (' + FloatToStr(VTopLeft.X) + ',' +
      FloatToStr(VTopLeft.Y) + ')'
  );
end;

initialization
  RegisterTest(TestLocalCoordConverterFactorySimpe.Suite);

end.
