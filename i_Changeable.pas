unit i_Changeable;

interface

uses
  i_JclNotify;

type
  IChangeable = interface
    ['{7F6DCE71-7EBD-40A0-A3BF-C603A3AEE8BA}']
    function GetBeforeChangeNotifier: IJclNotifier;
    property BeforeChangeNotifier: IJclNotifier read GetBeforeChangeNotifier;

    function GetChangeNotifier: IJclNotifier;
    property ChangeNotifier: IJclNotifier read GetChangeNotifier;

    function GetAfterChangeNotifier: IJclNotifier;
    property AfterChangeNotifier: IJclNotifier read GetAfterChangeNotifier;
  end;

implementation

end.
