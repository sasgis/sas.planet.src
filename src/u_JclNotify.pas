{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclNotify.pas.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel Bestebroer.                                 }
{ Portions created by Marcel Bestebroer are Copyright Marcel Bestebroer. All rights reserved.      }
{                                                                                                  }
{ Contributors:                                                                                    }
{   -                                                                                              }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains generic JCL notification/listener pattern interfaces and base implementations }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2009-07-02 15:24:19 +0200 (jeu. 02 juil. 2009)                          $ }
{ Revision:      $Rev:: 2841                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit u_JclNotify;

interface

uses
  Classes,
  SysUtils,
  i_JclNotify;

  { The following classes provide a basic notifier/listener implementation. Note that using one of these classes does
    not imply the usage of the related classes; the notifier can be used in conjection with any class implementing
    IJclListener and vice versa. }
type
  TJclBaseListener = class (TInterfacedObject, IJclListener)
  protected
    procedure Notification(msg: IJclNotificationMessage); virtual; stdcall;
  end;

  TJclBaseNotificationMessage = class (TInterfacedObject, IJclNotificationMessage)
  end;

  TJclBaseNotifier = class (TInterfacedObject, IJclNotifier)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FListeners: TInterfaceList;
    FSynchronizer: TMultiReadExclusiveWriteSynchronizer;
  protected
    procedure Add(listener: IJclListener); stdcall;
    procedure Notify(msg: IJclNotificationMessage); stdcall;
    procedure Remove(listener: IJclListener); stdcall;
  end;

implementation

{ TJclBaseNotifier }

constructor TJclBaseNotifier.Create;
begin
  inherited Create;
  FListeners := TInterfaceList.Create;
  FSynchronizer := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TJclBaseNotifier.Destroy;
begin
  FSynchronizer.BeginWrite;
  try
    FreeAndNil(FListeners);
  finally
    FSynchronizer.EndWrite;
    FreeAndNil(FSynchronizer);
  end;
  inherited Destroy;
end;

procedure TJclBaseNotifier.Add(listener: IJclListener);
begin
  FSynchronizer.BeginWrite;
  try
    if FListeners.IndexOf(listener) < 0 then
      FListeners.Add(listener);
  finally
    FSynchronizer.EndWrite;
  end;
end;

procedure TJclBaseNotifier.Notify(msg: IJclNotificationMessage);
var
  idx: Integer;
begin
  FSynchronizer.BeginRead;
  try
    for idx := 0 to FListeners.Count - 1 do
      IJclListener(FListeners[idx]).Notification(msg);
  finally
    FSynchronizer.EndRead;
  end;
end;

procedure TJclBaseNotifier.Remove(listener: IJclListener);
var
  idx: Integer;
begin
  FSynchronizer.BeginWrite;
  try
    idx := FListeners.IndexOf(listener);
    if idx >= 0 then
      FListeners.Delete(idx);
  finally
    FSynchronizer.EndWrite;
  end;
end;

{ TJclBaseListener }

procedure TJclBaseListener.Notification(msg: IJclNotificationMessage);
begin
  // do nothing; descendants should override this method to process incoming notifications
end;

end.
