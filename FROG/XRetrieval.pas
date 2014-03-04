unit XRetrieval;

interface

uses Retrieval, Func1D, SignalField, NLO, FrogTrace, Strategy;

type
  TXRetrieval = class(TRetrieval)
    private
      mERef: TEField;
      procedure MakeSignalField(pEk: TEField; pESig: TSignalField); override;
      function GetRescaleFactor(pXmin: double): double; override;
      procedure SetCurrentStrategy(pCurrStrat: TStrategy); override;
      procedure CenterRetrievedField; override;
    public
      property ReferenceField: TEField read mERef write mERef;
      constructor Create(pNLO: TNLOInteraction; pFrogI, pFrogIk: TFrogTrace; pEk, pERef: TEField);
  end;

implementation

constructor TXRetrieval.Create(pNLO: TNLOInteraction; pFrogI, pFrogIk: TFrogTrace; pEk, pERef: TEField);
begin
  mERef := pERef;  // This needs to be first.
  inherited Create(pNLO, pFrogI, pFrogIk, pEk);
end;

procedure TXRetrieval.MakeSignalField(pEk: TEField; pESig: TSignalField);
begin
  mNLOInteraction.MakeXSignalField(pEk, mERef, pEsig);
end;

procedure TXRetrieval.CenterRetrievedField;
begin
	// Don't center the retrieved field in XFROG
end;

function TXRetrieval.GetRescaleFactor(pXmin: double): double;
begin
  // For XFrog, the signal field is always linear in the test field
  GetRescaleFactor := 0.5;
end;

procedure TXRetrieval.SetCurrentStrategy(pCurrStrat: TStrategy);
begin
  mCurrentStrategy := pCurrStrat;
  mCurrentStrategy.ReferenceField := mERef;
end;


end.
