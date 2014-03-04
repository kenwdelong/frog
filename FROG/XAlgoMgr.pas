unit XAlgoMgr;

interface

uses AlgoMgr, Func1D, XFrogAlgo, RunTypeU;

type
  TXFrogAlgoMgr = class(TFrogAlgoMgr)
    private
      mERef: TEField;
      mXFrogAlgo: TXFrogAlgo;
      procedure SyncRefForm;
    public
      property ReferenceField: TEField read mERef write mERef;
      procedure DoXFrog;
      constructor Create;
  end;

implementation

uses XTestCalsForm, XRefCalsForm, Controls, CalsForm, Forms, Dialogs, AlgoCals,
    EFieldCals, SysUtils;

constructor TXFrogAlgoMgr.Create;
begin
  inherited Create;
  mERef := nil;
end;

procedure TXFrogAlgoMgr.DoXFrog;
begin
  if mFrogAlgo <> nil then
    if mFrogAlgo.Running then Exit;

  if frmXTestCals = nil then frmXTestCals := TfrmXTestCals.Create(Application);
  frmXTestCals.ShowModal;
  if (frmXTestCals.ModalResult = mrCancel) then Exit;
  mAllCals := frmXTestCals.AllCals;

  // if there's no ref field, then disable that option
  if frmXRefCals = nil then frmXRefCals := TfrmXRefCals.Create(Application);
  SyncRefForm;
  frmXRefCals.ShowModal;
  if (frmXRefCals.ModalResult = mrCancel) then Exit;

  if mFrogAlgo = nil then
  begin
    mXFrogAlgo := TXFrogAlgo.CreateX(frmXTestCals.AllCals, frmXRefCals.AllCals);
    mFrogAlgo := mXFrogAlgo;
  end;

  if mERef <> nil then mXFrogAlgo.ReferenceField := mERef;

  try
    mFrogAlgo.InitAlgorithm;
  except on e:Exception do
  begin
    ShowMessage('Error initializing algorithm: ' + e.Message);
    Exit;
  end;
  end;
  InitAlgoEvents;

  if not mWindowsOpen then InitWindows;

  InitDisplay;

end;

procedure TXFrogAlgoMgr.SyncRefForm;
begin
  if mERef <> nil then frmXRefCals.RefFieldAvailable := True;

  frmXRefCals.AllCals.InitialFieldCals.N := frmXTestCals.AllCals.InitialFieldCals.N;
  frmXRefCals.AllCals.InitialFieldCals.Domain := frmXTestCals.AllCals.InitialFieldCals.Domain;
  frmXRefCals.AllCals.InitialFieldCals.Units := frmXTestCals.AllCals.InitialFieldCals.Units;

  if frmXTestCals.AllCals.AlgoCals.RunTypeEnum <> rteTheory then
    frmXRefCals.AllCals.InitialFieldCals.Units := unReal;

  frmXRefCals.RdbDomain.ItemIndex := Ord(frmXRefCals.AllCals.InitialFieldCals.Domain);
  frmXRefCals.RdbUnits.ItemIndex := Ord(frmXRefCals.AllCals.InitialFieldCals.Units);
  frmXRefCals.RefreshInitialFieldTab;
end;

end.

{
The XFROG algo is implemented heavily on top of the FROG algo.
XAlgoMgr derives from FrogAlgoMgr
XAlgo derives from FrogAlgo
XRetrieval derives from Retrieval

The main flow is controlled by the FROG classes, and the X versions override hooks to
impl XFROG specific operations.

All the weird unit stuff is done in XFrogAlgo.SynchronizeTimeScales.  Ref fields are read in
by a ReadIn object.  If their N is wrong, it puts it on an N the same size as the test
pulse or exper data.  Then in SynchTimeScales, it is made equal to the test field delT.
This uses the Window function, so that you don't need to worry if it's bigger or smaller.
}
