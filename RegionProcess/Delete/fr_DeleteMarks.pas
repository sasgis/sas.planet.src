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

unit fr_DeleteMarks;

interface

uses
  Classes,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  i_LanguageManager,
  i_GeometryLonLat,
  i_RegionProcessParamsFrame,
  u_CommonFormAndFrameParents;

type
  TfrDeleteMarks = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameMarksState
    )
    chkDelHidden: TCheckBox;
    PnlTop: TPanel;
    chkPlacemarks: TCheckBox;
    chkPaths: TCheckBox;
    chkPolygons: TCheckBox;
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: IGeometryLonLatPolygon
    );
    function Validate: Boolean;
  private
    function MarksState: Byte;
    function DeleteHiddenMarks: Boolean;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager
    ); reintroduce;
  end;

implementation

uses
  gnugettext,
  c_MarkFlag;

{$R *.dfm}

{ TfrDeleteMarks }

constructor TfrDeleteMarks.Create(const ALanguageManager: ILanguageManager);
begin
  inherited Create(ALanguageManager);
end;

function TfrDeleteMarks.DeleteHiddenMarks: Boolean;
begin
  Result := chkDelHidden.Checked;
end;

procedure TfrDeleteMarks.Init(
  const AZoom: byte;
  const APolygon: IGeometryLonLatPolygon
);
begin
  // Do nothing
end;

function TfrDeleteMarks.MarksState: Byte;
begin
  Result := 0;
  if chkPlacemarks.Checked then  Result := Result or CPlacemarkFlag;
  if chkPaths.Checked      then  Result := Result or CPathFlag;
  if chkPolygons.Checked   then  Result := Result or CPolygonFlag;
end;

function TfrDeleteMarks.Validate: Boolean;
begin
  Result := chkPlacemarks.Checked or chkPaths.Checked or chkPolygons.Checked;
  if not Result then begin
    ShowMessage(_('Please select at one placemark type'));
  end;
end;

end.
