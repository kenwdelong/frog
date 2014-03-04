unit XFrogAlgo;

interface

uses FrogAlgo, Func1D, XRetrieval, AllCals, RunTypeU;

type
  TXFrogAlgo = class(TFrogAlgo)
    private
      mERef: TEField;
      mRefCals: TAllCals;
      mXRet: TXRetrieval;
      procedure InitReferencePulse;
      procedure SynchronizeReferencePulseScale;
    protected
      procedure InitRetrieval; override;
      procedure InitExperimental; override;
      procedure SynchronizeUnits; override;
    public
      property ReferenceField: TEField read mERef write mERef;
      constructor CreateX(pTestCals, pRefCals: TAllCals);
  end;

implementation

uses AlgoCals, Dialogs, SysUtils, ReadIn, TimeFreq;

//  The XFROG algos use the Ek in the FROG classes as the test field.
// The reference field is handled separately and directly in the XFROG subclasses
constructor TXFrogAlgo.CreateX(pTestCals, pRefCals: TAllCals);
begin
  mRefCals := pRefCals;
  mRefCals.AlgoCals.N := pTestCals.AlgoCals.N;

  inherited Create(pTestCals);
end;

procedure TXFrogAlgo.InitRetrieval;
begin
  // By the time we get here, Ek and mLog are init'd
  InitReferencePulse;
  mXRet := TXRetrieval.Create(mNLOInteraction, FrogI, FrogIk, Ek, mERef);
  mRetrieval := mXRet;
end;

procedure TXFrogAlgo.InitReferencePulse;
begin
  mLog.Add(' ');
  mLog.Add('********* Reference Field **********');
  case mRefCals.AlgoCals.RunTypeEnum of
    rteTheory:
      begin
        mERef.Free;
        mERef := TEField.Create(mRefCals.InitialFieldCals.N);
        mRefCals.InitialFieldCals.GenerateE(mERef);
        mRefCals.InitialFieldCals.InitLog(mLog);
      end;
    rteExperimental:  // For the ref pulse, exper = existing FROG retrieval
      begin
        if mERef = nil then
        begin
          //ShowMessage('No Reference Field found.');
          AbortFrog;
          raise Exception.Create('No Reference Field found.');
        end;
      end;
    rteReadIn:
      begin
        try
          mERef.Free;
          mERef := TEField.Create(mAllCals.AlgoCals.N);
          mRefCals.ReadIn.LoadField(mERef);
          // What the hell is this doing here?  Nuernberger bug.
          //Ek.SetMyUnits(mERef.DelT, mERef.Lam0);
          mRefCals.ReadIn.InitLog(mLog);
        except on e:Exception do
        begin
          ShowMessage(e.Message);
          AbortFrog;
          raise;
        end;
        end;
      end;
  end;

  mERef.Norm;
end;

procedure TXFrogAlgo.InitExperimental;
begin
  mAllCals.Exper.LoadOriginalFrogTrace(FrogI, mNLOInteraction);
  E.SetMyUnits(mAllCals.Exper.ModDelT, mAllCals.Exper.ModLam0/mNLOInteraction.SpectrumMultiplier);
  FrogI.SynchronizeUnits(E);
  FrogIk.SynchronizeUnits(E);
  mAllCals.Exper.InitLog(mLog);
end;

// Called from SynchronizeUnits, below
procedure TXFrogAlgo.SynchronizeReferencePulseScale;
var
  newRef: TEField;
  aReadIn: TReadin;
begin
  // Make sure the test and ref pulses are on the same time scale.
  // Adj Ref field to have same delT as test.  This should work in all cases.
  if mEref.DelT <> Ek.DelT then
  begin
    newRef := TEField.Create(Ek.N);
    newRef.SetMyUnits(Ek.DelT, mEref.Lam0);
    aReadIn := TReadIn.Create;
    aReadIn.SetEFieldDelT(mERef, newRef);
    aReadIn.Free;
    // Don't free the ref field if its the previous best run.  It belongs to the FROG algo mgr.
    if mRefCals.AlgoCals.RunTypeEnum <> rteExperimental then
        mERef.Free;
    mERef := newRef;
    mERef.Norm;  // Calls SetIntensityAndPhase
    mXRet.ReferenceField := mERef;
  end;
end;


// Sets the units on the XFROG trace objects
// This is an override of FrogAlgo.SynchronizeUnits.
// It is called in FrogAlgo.InitAlgorithm
procedure TXFrogAlgo.SynchronizeUnits;
var
  frogLam, refLam, testLam, delT: double;
  tempE: TEField;
  timeFreq: TTimeFreqCalibration;
begin
// E is a property on TFrogAlgo.  It's the original test field, the one
// we are trying to retrieve.  We only have an E on Theory or ReadIn.
  case mAllCals.AlgoCals.RunTypeEnum of
    rteTheory, rteReadIn:
      begin
      	// Determine the Lam0 of the FROG trace, from the lams of the reference and test fields
        frogLam := mNLOInteraction.XFrogLam(mERef.Lam0, E.Lam0);
        timeFreq := TTimeFreqCalibration.CreateFromDelT(E.DelT, frogLam, E.N);
        // Set the FROG traces to that Lam0, and the delT of the test field
        FrogI.SynchronizeUnitsFromTF(timeFreq);
        FrogIk.SynchronizeUnitsFromTF(timeFreq);
        timeFreq.Free;
      end;
    rteExperimental:
      begin
        frogLam := FrogI.Lam0;
        refLam := mERef.Lam0;
        testLam := mNLOInteraction.XTestLam(frogLam, refLam);
        // The test field will be retrieved on whatever the FROG trace spacing is
        delT := FrogI.DelT;
        E.SetMyUnits(delT, testLam);
        Ek.SetMyUnits(delT, testLam);
      end;
  end;

  SynchronizeReferencePulseScale;

  // Create the signal field with a proper reference field.
  // See bug report in TRetrieval
  mXRet.ReInitializeSignalField;
end;

end.
