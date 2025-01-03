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

unit fr_MarksProcess;

interface

uses
  Classes,
  Controls,
  Forms,
  ExtCtrls,
  StdCtrls,
  i_Category,
  i_MarkCategoryDB,
  i_LanguageManager,
  i_GeometryLonLat,
  i_RegionProcessParamsFrame,
  t_MarksProcess,
  fr_MarkCategorySelectOrAdd,
  u_CommonFormAndFrameParents;

type
  TfrMarksProcess = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameMarks
    )
    chkIncludeHidden: TCheckBox;
    pnlTop: TPanel;
    chkPlacemarks: TCheckBox;
    chkPaths: TCheckBox;
    chkPolygons: TCheckBox;
    pnlOperation: TPanel;
    cbbOperation: TComboBox;
    lblOperation: TLabel;
    pnlCategory: TPanel;
    procedure cbbOperationChange(Sender: TObject);
  private
    FfrMarkCategory: TfrMarkCategorySelectOrAdd;
    function IsOperationWithCategory: Boolean;
    function GetMarksOperation: TMarksProcessOperation;
    function GetMarksTypes: TMarksProcessTypes;
    function GetCategory: ICategory;
  private
    { IRegionProcessParamsFrameBase }
    procedure Init(
      const AZoom: Byte;
      const APolygon: IGeometryLonLatPolygon
    );
    function Validate: Boolean;
  private
    { IRegionProcessParamsFrameMarks }
    function GetTaskParams: TMarksProcessTaskParams;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ACategoryDB: IMarkCategoryDB
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  gnugettext,
  u_Dialogs;

{$R *.dfm}

{ TfrMarksProcess }

constructor TfrMarksProcess.Create(
  const ALanguageManager: ILanguageManager;
  const ACategoryDB: IMarkCategoryDB
);
begin
  inherited Create(ALanguageManager);

  FfrMarkCategory :=
    TfrMarkCategorySelectOrAdd.Create(
      ALanguageManager,
      ACategoryDB
    );

  FPropertyState := CreateComponentPropertyState(
    Self, [pnlCategory], [], True, False, True, True
  );
end;

destructor TfrMarksProcess.Destroy;
begin
  FreeAndNil(FfrMarkCategory);
  inherited Destroy;
end;

function TfrMarksProcess.IsOperationWithCategory: Boolean;
begin
  Result := cbbOperation.ItemIndex in [Integer(mpoCopy), Integer(mpoMove)];
end;

procedure TfrMarksProcess.cbbOperationChange(Sender: TObject);
begin
  pnlCategory.Visible := IsOperationWithCategory;
end;

function TfrMarksProcess.GetCategory: ICategory;
begin
  if IsOperationWithCategory then begin
    Result := FfrMarkCategory.GetCategory;
  end else begin
    Result := nil;
  end;
end;

function TfrMarksProcess.GetMarksOperation: TMarksProcessOperation;
begin
  Result := TMarksProcessOperation(cbbOperation.ItemIndex);
end;

function TfrMarksProcess.GetMarksTypes: TMarksProcessTypes;
begin
  Result := [];

  if chkPlacemarks.Checked then begin
    Include(Result, mptPlacemarks);
  end;

  if chkPaths.Checked then begin
    Include(Result, mptPaths);
  end;

  if chkPolygons.Checked then begin
    Include(Result, mptPolygons);
  end;
end;

function TfrMarksProcess.GetTaskParams: TMarksProcessTaskParams;
begin
  with Result do begin
    Operation := GetMarksOperation;
    MarksTypes := GetMarksTypes;
    IncludeHiddenMarks := chkIncludeHidden.Checked;
    Category := GetCategory;
  end;
end;

procedure TfrMarksProcess.Init(
  const AZoom: Byte;
  const APolygon: IGeometryLonLatPolygon
);
begin
  if cbbOperation.ItemIndex < 0 then begin
    cbbOperation.ItemIndex := 0;
  end;

  cbbOperationChange(nil);

  FfrMarkCategory.Parent := pnlCategory;
  FfrMarkCategory.Init(nil);
end;

function TfrMarksProcess.Validate: Boolean;
var
  VParams: TMarksProcessTaskParams;
begin
  Result := True;

  VParams := GetTaskParams;

  if VParams.MarksTypes = [] then begin
    ShowErrorMessage(_('Please select at least one type of placemark!'));
    Exit(False);
  end;

  if (VParams.Operation in [mpoCopy, mpoMove]) and (VParams.Category = nil) then begin
    ShowErrorMessage(_('Please select target category!'));
    Exit(False);
  end;
end;

end.
