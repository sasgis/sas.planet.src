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

unit i_MarkFactorySmlInternal;

interface

uses
  GR32,
  t_GeoTypes,
  i_Mark;

type
  IMarkFactorySmlInternal = interface
    ['{0D5A67D8-585A-4DA8-9047-CB3CB76A600E}']
    function CreateMark(
      AID: Integer;
      const AName: string;
      AVisible: Boolean;
      const APicName: string;
      ACategoryId: Integer;
      const ADesc: string;
      const APoints: PDoublePointArray;
      APointCount: Integer;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer;
      AScale2: Integer
    ): IMark;
    function CreateInternalMark(const AMark: IMark): IMark;
  end;

const
  CNotExistMarkID = -1;

implementation

end.
