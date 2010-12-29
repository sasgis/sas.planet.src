unit i_IJclListenerNotifierLinksList;

interface

uses
  i_JclNotify;

type
  IJclListenerNotifierLinksList = interface
    ['{B197E296-150C-4961-9370-1BF73F0B8BB6}']
    procedure Add(AListener: IJclListener; ANotifier: IJclNotifier);
    procedure ActivateLinks;
    procedure DeactivateLinks;
  end;

implementation

end.
