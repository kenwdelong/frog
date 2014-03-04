unit XRefCals;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CalsForm, Grids, FileCtrl, StdCtrls, ComCtrls, ExtCtrls;

type
  TfrmXRefCals = class(TfrmCalibrations)
  private
    { Private declarations }
  public
    { Public declarations }
    procedure rdgDataSourceClick(Sender: TObject); override;
  end;

var
  frmXRefCals: TfrmXRefCals;

// This form collects the calibrations for the reference pulse
// in XFrog.
  
implementation




{$R *.DFM}

end.
