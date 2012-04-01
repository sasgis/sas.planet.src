unit i_ThreadConfig;

interface

uses
  Classes,
  i_ConfigDataElement;

type
  IThreadConfig = interface(IConfigDataElement)
    ['{34ECD0C7-69E4-4F46-8438-1A4339C07DB8}']
    function GetPriority: TThreadPriority;
    procedure SetPriority(AValue: TThreadPriority);
    property Priority: TThreadPriority read GetPriority write SetPriority;
  end;

implementation

end.
