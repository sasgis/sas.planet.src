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

unit i_JclNotify;

interface

type
  IListener = interface
    ['{26A52ECC-4C22-4B71-BC88-D0EB98AF4ED5}']
    procedure Notification(const msg: IInterface); stdcall;
  end;

  IListenerDisconnectable = interface(IListener)
    ['{7E47F99B-3D00-4743-B6EC-EBEB3257CA08}']
    procedure Disconnect; stdcall;
  end;

  INotifier = interface
    ['{CAAD7814-DD04-497C-91AC-558C2D5BFF81}']
    procedure Add(const listener: IListener); stdcall;
    procedure Remove(const listener: IListener); stdcall;
    procedure Notify(const msg: IInterface); stdcall;
  end;

implementation

end.


