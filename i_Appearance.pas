unit i_Appearance;

interface

uses
  t_Hash;

type
  IAppearance = interface
    ['{E85940D7-9013-4610-A6BB-CEA3C9A9DAB1}']
    function GetHash: THashValue;
    property Hash: THashValue read GetHash;

    function IsEqual(const AValue: IAppearance): Boolean;
  end;

implementation

end.
