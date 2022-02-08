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

unit u_GeometryMetaJson;

interface

type
  TJsonMetaGeometryTypeId = (
    jgPoint,
    jgLine,
    jgPoly
  );

  TJsonMetaDataTypeId = (
    jdInteger,
    jdDouble
  );

  TJsonMetaKnownGpxTagsId = (
    jtEle,
    jtTime
  );

const
  CJsonMetaKnownGpxTags: array [TJsonMetaKnownGpxTagsId] of string = (
    'ele',
    'time'
  );

  CJsonMetaMagic: array [0..3] of AnsiChar = (
    'J', 'S', 'O', 'N'
  );

implementation

(*

  {
      "v" : integer,                     // file struct version
      't' : integer,                     // geometry type id (point/line/poly)
      "g" : [                            // array of geometries
          {
              "c" : integer,             // points count
              "m" : [                    // array of metadata
                  {
                      "t" : integer,     // data type id (integer/double)
                      "n" : string,      // gpx tag name
                      "d" : string       // points meta (base64)
                  }
                  ...
              ]
          }
          ...
      ]
  }

*)

end.
