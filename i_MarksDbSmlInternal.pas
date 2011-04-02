unit i_MarksDbSmlInternal;

interface

type
  IMarkSMLInternal = interface
  ['{2611AAA5-10DA-472B-B3EE-31EA27EDD6CD}']
    function GetId: Integer;
    property Id: Integer read GetId;

    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;
  end;


implementation

end.
