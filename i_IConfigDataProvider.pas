unit i_IConfigDataProvider;

interface

uses
  Classes;

type
  IConfigDataProvider = interface
    ['{FB657238-6D8F-463D-B56F-3FB4C74EE352}']
    function GetSubItem(const AIdent: string): IConfigDataProvider;
    function ReadBinaryStream(const AIdent: string; AValue: TStream): Integer;
    function ReadString(const AIdent: string; const ADefault: string): string;
    function ReadInteger(const AIdent: string; ADefault: Longint): Longint;
    function ReadBool(const AIdent: string; ADefault: Boolean): Boolean;
    function ReadDate(const AIdent: string; ADefault: TDateTime): TDateTime;
    function ReadDateTime(const AIdent: string; ADefault: TDateTime): TDateTime;
    function ReadFloat(const AIdent: string; ADefault: Double): Double;
    function ReadTime(const AIdent: string; ADefault: TDateTime): TDateTime;

    procedure ReadSubItemsList(AList: TStrings);
    procedure ReadValuesList(AList: TStrings);
  end;

implementation

end.
