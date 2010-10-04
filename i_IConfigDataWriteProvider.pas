unit i_IConfigDataWriteProvider;

interface

uses
  Classes,
  i_IConfigDataProvider;

type
  IConfigDataWriteProvider = interface(IConfigDataProvider)
    ['{2AA51ACD-3056-4328-BDF9-D458E50F5734}']
    function GetOrCreateSubItem(const AIdent: string): IConfigDataWriteProvider;
    procedure DeleteSubItem(const AIdent: string);
    procedure DeleteValue(const AIdent: string);
    procedure WriteBinaryStream(const AIdent: string; AValue: TStream);
    procedure WriteString(const AIdent: string; const AValue: string);
    procedure WriteInteger(const AIdent: string; const AValue: Longint);
    procedure WriteBool(const AIdent: string; const AValue: Boolean);
    procedure WriteDate(const AIdent: string; const AValue: TDateTime);
    procedure WriteDateTime(const AIdent: string; const AValue: TDateTime);
    procedure WriteFloat(const AIdent: string; const AValue: Double);
    procedure WriteTime(const AIdent: string; const AValue: TDateTime);
  end;

implementation

end.
