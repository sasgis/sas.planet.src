{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_ExportProviderAbstract;

interface

uses
  Controls,
  Forms,
  i_LanguageManager,
  i_GeometryLonLat,
  i_TileIteratorFactory,
  i_RegionProcessTask,
  i_RegionProcessProgressInfo,
  i_RegionProcessProgressInfoInternalFactory,
  i_RegionProcessParamsFrame,
  i_RegionProcessProvider,
  fr_MapSelect,
  u_BaseInterfacedObject;

type
  TExportProviderAbstract = class(TBaseInterfacedObject, IRegionProcessProvider)
  private
    FFrame: TFrame;
    FLanguageManager: ILanguageManager;
    FTileIteratorFactory: ITileIteratorFactory;
    FMapSelectFrameBuilder: IMapSelectFrameBuilder;
    FProgressFactory: IRegionProcessProgressInfoInternalFactory;
    function GetParamsFrame: IRegionProcessParamsFrameBase;
    function GetTileIteratorFactory: ITileIteratorFactory;
  protected
    function GetCaption: string; virtual; abstract;
    procedure Show(
      const AParent: TWinControl;
      const AZoom: Byte;
      const APolygon: IGeometryLonLatPolygon
    );
    procedure Hide;
    function Validate(
      const APolygon: IGeometryLonLatPolygon
    ): Boolean; virtual;
    procedure StartProcess(
      const APolygon: IGeometryLonLatPolygon
    ); virtual; abstract;
  protected
    function CreateFrame: TFrame; virtual; abstract;
    property ParamsFrame: IRegionProcessParamsFrameBase read GetParamsFrame;
    property ProgressFactory: IRegionProcessProgressInfoInternalFactory read FProgressFactory;
    property TileIteratorFactory: ITileIteratorFactory read GetTileIteratorFactory;
    property LanguageManager: ILanguageManager read FLanguageManager;
    property MapSelectFrameBuilder: IMapSelectFrameBuilder read FMapSelectFrameBuilder;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const ATileIteratorFactory: ITileIteratorFactory
    );
    destructor Destroy; override;
  end;

  TExportProviderBase = class(TExportProviderAbstract)
  protected
    function PrepareTask(
      const APolygon: IGeometryLonLatPolygon;
      const AProgressInfo: IRegionProcessProgressInfoInternal
    ): IRegionProcessTask; virtual; abstract;
    procedure StartProcess(
      const APolygon: IGeometryLonLatPolygon
    ); override;
  end;

implementation

uses
  SysUtils,
  u_RegionProcessWorker;

{ TExportProviderAbstract }

constructor TExportProviderAbstract.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const ATileIteratorFactory: ITileIteratorFactory
);
begin
  Assert(Assigned(AProgressFactory));
  Assert(Assigned(ALanguageManager));
  Assert(Assigned(AMapSelectFrameBuilder));
  //ATileIteratorFactory can be nil here

  inherited Create;

  FProgressFactory := AProgressFactory;
  FLanguageManager := ALanguageManager;
  FMapSelectFrameBuilder := AMapSelectFrameBuilder;
  FTileIteratorFactory := ATileIteratorFactory;
end;

destructor TExportProviderAbstract.Destroy;
begin
  FFrame := nil; // the frame will be destroyed by its parent
  inherited Destroy;
end;

function TExportProviderAbstract.GetParamsFrame: IRegionProcessParamsFrameBase;
begin
  if not Supports(FFrame, IRegionProcessParamsFrameBase, Result) then begin
    Result := nil;
  end;
end;

function TExportProviderAbstract.GetTileIteratorFactory: ITileIteratorFactory;
begin
  Result := FTileIteratorFactory;
  Assert(Result <> nil);
end;

procedure TExportProviderAbstract.Hide;
begin
  if (FFrame <> nil) and FFrame.Visible then begin
    FFrame.Hide;
  end;
end;

procedure TExportProviderAbstract.Show(
  const AParent: TWinControl;
  const AZoom: Byte;
  const APolygon: IGeometryLonLatPolygon
);
var
  VFrame: IRegionProcessParamsFrameBase;
begin
  if FFrame = nil then begin
    FFrame := CreateFrame;
    if not Supports(FFrame, IRegionProcessParamsFrameBase) then begin
      FreeAndNil(FFrame);
      Assert(False);
    end;
  end;
  if FFrame <> nil then begin
    FFrame.Parent := AParent; // Parent now Owns the Frame
    if not FFrame.Visible then begin
      FFrame.Show;
    end;
    VFrame := ParamsFrame;
    if VFrame <> nil then begin
      VFrame.Init(AZoom, APolygon);
    end;
  end;
end;

function TExportProviderAbstract.Validate(
  const APolygon: IGeometryLonLatPolygon
): Boolean;
begin
  Result := GetParamsFrame.Validate;
end;

{ TExportProviderBase }

procedure TExportProviderBase.StartProcess(
  const APolygon: IGeometryLonLatPolygon
);
var
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VTask: IRegionProcessTask;
  VThread: TRegionProcessWorker;
begin
  VProgressInfo := ProgressFactory.Build(APolygon);

  VTask := PrepareTask(APolygon, VProgressInfo);
  if Assigned(VTask) then begin
    VThread :=
      TRegionProcessWorker.Create(
        VTask,
        VProgressInfo,
        ClassName
      );
    VThread.Start;
  end;
end;

end.
