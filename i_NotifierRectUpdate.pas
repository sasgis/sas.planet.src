unit i_NotifierRectUpdate;

interface

uses
  Types,
  i_Listener,
  i_Point,
  i_Rect;

type
  INotifierRectUpdate = interface
    ['{67415555-955C-4BC7-BC8F-2F9BCDD0F065}']
    procedure AddListenerByRect(
      const AListener: IListener;
      const ATileRect: TRect
    );
    procedure AddListener(
      const AListener: IListener
    );
    procedure Remove(const AListener: IListener);
  end;

  INotifierRectUpdateInternal = interface
    procedure FullUpdateNotify;
    procedure UpdateNotify(const APoint: IPoint); overload;
    procedure UpdateNotify(const APoint: TPoint); overload;
    procedure RectUpdateNotify(const ARect: IRect); overload;
    procedure RectUpdateNotify(const ARect: TRect); overload;
  end;

implementation

end.
