unit i_BinaryDataListChangeable;

interface

uses
  i_BinaryDataListStatic,
  i_Changeable;

type
  IBinaryDataListChangeable = interface(IChangeable)
    ['{B22EC19E-0764-4F86-81DB-E1EE351DBF13}']
    function GetStatic: IBinaryDataListStatic;
  end;

implementation

end.
