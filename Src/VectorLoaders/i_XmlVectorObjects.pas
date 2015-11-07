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

unit i_XmlVectorObjects;

interface

uses
  t_GeoTypes,
  i_VectorItemSubset;

type
  TCloseMarkObjectMode = (cmom_KML, cmom_GPX_TRK, cmom_GPX_WPT);

  IXmlVectorObjects = interface
    ['{463DCF4D-096C-41FE-927B-23A2CB125A41}']
    // count of geometry objects
    function GetCount: Integer;
    property Count: Integer read GetCount;

    // result list
    function GetVectorDataItemsResult: IVectorItemSubset;
    property VectorDataItemsResult: IVectorItemSubset read GetVectorDataItemsResult;

    // open and close multigeometry object
    procedure OpenMultiGeometry;
    procedure CloseMultiGeometry;

    // open and close multitrack object
    procedure OpenMultiTrack;
    procedure CloseMultiTrack;

    // open and close track segment or track object
    procedure OpenTrackSegment;
    procedure CloseTrackSegment;

    // open and close mark object
    procedure OpenMarkObject;
    procedure CloseMarkObject(
      const AData: Pointer;
      const AMode: TCloseMarkObjectMode
    );

    // close simple line and add coordinates to array
    procedure CloseKmlLineString(const ACoordinates: string);
    // close polygon border and add coordinates to array
    procedure CloseKmlLinearRing(
      const ACoordinates: string;
      const AInner: Boolean
    );
    // add single point as single object
    procedure CloseKmlPoint(const ACoordinates: string);
    // add single point as single object
    procedure CloseGPXPoint(const APoint: TDoublePoint);
    // close polygon object
    procedure CloseKmlPolygon;

    // add single track point to array
    procedure AddTrackPoint(const APoint: TDoublePoint);
  end;

implementation

(*
case 1 - RosCosmos
<Placemark>
  <MultiGeometry>
    <Polygon>
      <outerBoundaryIs>
        <LinearRing>
          <coordinates>...</coordinates>
        </LinearRing>
      </outerBoundaryIs>
      <innerBoundaryIs>
        <LinearRing>
          <coordinates>...</coordinates>
        </LinearRing>
      </innerBoundaryIs>
    </Polygon>
  </MultiGeometry>
</Placemark>

case 2 - RosCosmos
<Placemark>
  <MultiGeometry>
    <Polygon>
      <outerBoundaryIs>
        <LinearRing>
          <coordinates>...</coordinates>
        </LinearRing>
      </outerBoundaryIs>
    </Polygon>
    <Polygon>
      <outerBoundaryIs>
        <LinearRing>
          <coordinates>...</coordinates>
        </LinearRing>
      </outerBoundaryIs>
    </Polygon>
  </MultiGeometry>
</Placemark>

case 3 - Wiki
<Folder>
  <Placemark id="wm9141615">
    <MultiGeometry>
      <LineString>
        <coordinates>...</coordinates>
      </LineString>
      <Point>
        <coordinates>...</coordinates>
      </Point>
    </MultiGeometry>
  </Placemark>
</Folder>

case 4 - GoogleEarth track
<Placemark>
  <gx:Track>
    <when>...</when>
    <when>...</when>
    <when>...</when>
    <gx:coord>...</gx:coord>
    <gx:coord>...</gx:coord>
    <gx:coord>...</gx:coord>
  </gx:Track>
</Placemark>

case 5 - ??? (several segments in track)
<Placemark>
  <Point>
    <coordinates>...</coordinates>
  </Point>
</Placemark>
<Placemark>
  <MultiGeometry>
    <LineString>
      <coordinates>...</coordinates>
    </LineString>
    <LineString>
      <coordinates>...</coordinates>
    </LineString>
    <LineString>
      <coordinates>...</coordinates>
    </LineString>
  </MultiGeometry>
</Placemark>

case 6 - ??? (multigeometry object has different types parts)
note that GeometryCollection is deprecated - use MultiGeometry instead
<Placemark>
  <MultiGeometry>
                <Point>
                        <coordinates>...</coordinates>
                </Point>
                <Polygon>
                        <outerBoundaryIs>
                                <LinearRing>
                                        <coordinates>...</coordinates>
                                </LinearRing>
                        </outerBoundaryIs>
                </Polygon>
                <Polygon>
                        <outerBoundaryIs>
                                <LinearRing>
                                        <coordinates>...</coordinates>
                                </LinearRing>
                        </outerBoundaryIs>
                </Polygon>
                <Polygon>
                        <outerBoundaryIs>
                                <LinearRing>
                                        <coordinates>...</coordinates>
                                </LinearRing>
                        </outerBoundaryIs>
                </Polygon>
                <Polygon>
                        <outerBoundaryIs>
                                <LinearRing>
                                        <coordinates>...</coordinates>
                                </LinearRing>
                        </outerBoundaryIs>
                </Polygon>
        </MultiGeometry>
</Placemark>

case 7 - KML tutorial
<Placemark>
  <LineString>
    <coordinates>...</coordinates>
  </LineString>
</Placemark>

*)

end.