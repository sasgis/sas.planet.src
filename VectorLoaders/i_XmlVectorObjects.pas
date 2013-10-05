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
    procedure CloseKmlLineString(const ACoordinates: WideString);
    // close polygon border and add coordinates to array
    procedure CloseKmlLinearRing(
      const ACoordinates: WideString;
      const AInner: Boolean
    );
    // add single point as single object
    procedure CloseKmlPoint(const ACoordinates: WideString);
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