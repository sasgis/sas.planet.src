unit i_IConfigDataWriteProvider;

interface

uses
  Classes,
  i_IConfigDataProvider;

type
  IConfigDataWriteProvider = interface(IConfigDataProvider)
    ['{2AA51ACD-3056-4328-BDF9-D458E50F5734}']
    procedure DeleteSubItem(const AIdent: string);
    procedure WriteBinaryStream(const AIdent: string; AValue: TStream);
    procedure WriteString(const AIdent: string; const AValue: string);
    procedure WriteInteger(const AIdent: string; AValue: Longint);
    procedure WriteBool(const AIdent: string; AValue: Boolean);
    procedure WriteDate(const AIdent: string; AValue: TDateTime);
    procedure WriteDateTime(const AIdent: string; AValue: TDateTime);
    procedure WriteFloat(const AIdent: string; AValue: Double);
    procedure WriteTime(const AIdent: string; AValue: TDateTime);
  end;

implementation

end.
