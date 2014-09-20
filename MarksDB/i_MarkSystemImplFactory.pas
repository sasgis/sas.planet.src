unit i_MarkSystemImplFactory;

interface

uses
  i_MarkSystemImpl;

type
  IMarkSystemImplFactory = interface
    ['{6ADF8D8C-670C-4282-9BC7-A3F9250181C6}']
    function GetIsInitializationRequired: Boolean;
    property IsInitializationRequired: Boolean read GetIsInitializationRequired;

    function Build(
      const ABasePath: string;
      const AReadOnly: Boolean = False
    ): IMarkSystemImpl;
  end;
  
implementation

end.
