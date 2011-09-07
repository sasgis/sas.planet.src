unit i_StringByLanguage;

interface

type
  IStringByLanguage = interface
    ['{5786072E-B7B3-480D-A9DE-3BA943FCC12C}']
    function GetString(ALangIndex: Integer): string;
    function GetDefault: string;
  end;

implementation

end.
