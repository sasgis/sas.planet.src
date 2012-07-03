unit i_Changeable;

interface

uses
  i_Notifier;

type
  IChangeable = interface
    ['{7F6DCE71-7EBD-40A0-A3BF-C603A3AEE8BA}']
    function GetBeforeChangeNotifier: INotifier;
    property BeforeChangeNotifier: INotifier read GetBeforeChangeNotifier;

    function GetChangeNotifier: INotifier;
    property ChangeNotifier: INotifier read GetChangeNotifier;

    function GetAfterChangeNotifier: INotifier;
    property AfterChangeNotifier: INotifier read GetAfterChangeNotifier;
  end;

implementation

end.
