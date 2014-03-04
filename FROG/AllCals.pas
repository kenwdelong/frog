unit AllCals;

interface

uses
    EFieldCals, AlgoCals, OutputCals, ReadIn, MargCals, Exper, StratTypes, FrogObj;

type
  TAllCals = class(TFrogObject)
    private
      mInitialFieldCals: TEFieldCals;
      mFirstGuessCals: TEFieldCals;
      mOutputCals: TOutputCals;
      mStrategyCals: TStrategyCals;
      mAlgoCals: TAlgoCals;
      mReadIn: TReadIn;
      mExper: TExper;
      mMargCals: TMargCals;
    public
      property InitialFieldCals: TEFieldCals read mInitialFieldCals;
      property FirstGuessCals: TEFieldCals read mFirstGuessCals;
      property OutputCals: TOutputCals read mOutputCals;
      property StrategyCals: TStrategyCals read mStrategyCals write mStrategyCals;
      property AlgoCals: TAlgoCals read mAlgoCals;
      property ReadIn: TReadIn read mReadIn;
      property Exper: TExper read mExper;
      property MargCals: TMargCals read mMargCals;
      constructor Create;
      destructor Destroy; override;
  end;

implementation

constructor TAllCals.Create;
begin
  mInitialFieldCals := TEFieldCals.Create;
  mFirstGuessCals := TEFieldCals.Create;
  mOutputCals := TOutputCals.Create;
  mAlgoCals := TAlgoCals.Create;
  mReadIn := TReadIn.Create;
  mExper := TExper.Create;
  mStrategyCals := TStrategyCals.Create;
  mMargCals := TMargCals.Create;
end;

destructor TAllCals.Destroy;
begin
  mMargCals.Free;
  mMargCals := nil;
  mStrategyCals.Free;
  mStrategyCals := nil;
  mExper.Free;
  mExper := nil;
  mReadIn.Free;
  mReadIn := nil;
  mAlgoCals.Free;
  mAlgoCals := nil;
  mOutputCals.Free;
  mOutputCals := nil;
  mFirstGuessCals.Free;
  mFirstGuessCals := nil;
  mInitialFieldCals.Free;
  mInitialFieldCals := nil;

  inherited Destroy;
end;

end.
