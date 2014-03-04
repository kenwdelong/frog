unit XTestCals;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CalsForm, Grids, FileCtrl, StdCtrls, ComCtrls, ExtCtrls;

type
  TfrmXTestCals = class(TfrmCalibrations)
  private
    { Private declarations }
  public
    { Public declarations }
    procedure rdgDataSourceClick(Sender: TObject); override;
  end;

var
  frmXTestCals: TfrmXTestCals;

implementation

{$R *.DFM}

procedure TfrmXTestCals.rdgDataSourceClick(Sender: TObject);
begin
  case rdgDataSource.ItemIndex of
    Ord(rtTheory):
      begin
      mAlgoCals.RunType := rtTheory;
      tbsInitialField.TabVisible := True;
      tbsExpData.TabVisible := False;
      tbsInputData.TabVisible := False;
      tbsMarginals.TabVisible := False;
      pgcCals.ActivePage := tbsInitialField;
      RefreshInitialFieldTab;
      RefreshFirstGuessTab;
      end;
    Ord(rtExperimental):
      begin
      mAlgoCals.RunType := rtExperimental;
      tbsInitialField.TabVisible := False;
      tbsExpData.TabVisible := True;
      tbsInputData.TabVisible := False;
      tbsMarginals.TabVisible := True;
      pgcCals.ActivePage := tbsExpData;
      RefreshExperTab;
      end;
    Ord(rtReadIn):
      begin
      mAlgoCals.RunType := rtReadIn;
      tbsInitialField.TabVisible := False;
      tbsExpData.TabVisible := False;
      tbsInputData.TabVisible := True;
      tbsMarginals.TabVisible := False;
      pgcCals.ActivePage := tbsInputData;
      RefreshReadInTab;
      end;
  end;
end;

end.
