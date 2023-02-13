unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Spin,
  StdCtrls, ExtCtrls, Menus, uStat, Math, Clipbrd;

type

  TTriangle = record
    angleA, angleB, angleC, sizeA, sizeB, sizeC: double;
  end;
  { TMainForm }

  TMainForm = class(TForm)
    DropO: TComboBox;
    DropR: TComboBox;
    LabelOAngleA: TLabel;
    LabelAngle: TLabel;
    LabelOAngleB: TLabel;
    LabelOAngleC: TLabel;
    LabelSize: TLabel;
    MainMenu1: TMainMenu;
    AppleMenu: TMenuItem;
    AppleAboutMenu: TMenuItem;
    EditMenu: TMenuItem;
    EditCopyMenu: TMenuItem;
    HelpMenu: TMenuItem;
    HelpAboutMenu: TMenuItem;
    Help1x2Item: TMenuItem;
    Help2x2Item: TMenuItem;
    HelpDPrimeItem: TMenuItem;
    HelpRightItem: TMenuItem;
    HelpObliqueItem: TMenuItem;
    RightMemo: TMemo;
    Panel1: TPanel;
    ThetaLabel: TLabel;
    AdjLabel: TLabel;
    OppLabel: TLabel;
    HypLabel: TLabel;
    OMemo: TMemo;
    vOsa: TFloatSpinEdit;
    Image3: TImage;
    LabelOSizeC: TLabel;
    LabelOSizeB: TLabel;
    LabelOSizeA: TLabel;
    LabelOd: TLabel;
    vOaa: TFloatSpinEdit;
    vOab: TFloatSpinEdit;
    vOac: TFloatSpinEdit;
    vOsb: TFloatSpinEdit;
    vOsc: TFloatSpinEdit;
    ThetaEdit: TFloatSpinEdit;
    Image1: TImage;
    Image2: TImage;
    LabelDa: TLabel;
    LabelDf: TLabel;
    LabelDe: TLabel;
    LabelDd: TLabel;
    LabelDc: TLabel;
    LabelDb: TLabel;
    Label2x2a: TLabel;
    Label1x1c: TLabel;
    Label2x2b: TLabel;
    Label2x2c: TLabel;
    Label2x2d: TLabel;
    x1Memo: TMemo;
    x2Memo: TMemo;
    DMemo: TMemo;
    LabelRa: TStaticText;
    sThetaLabel: TStaticText;
    sAdjLabel: TStaticText;
    sOppLabel: TStaticText;
    sHypLabel: TStaticText;
    HitsEdit: TSpinEdit;
    AEdit: TSpinEdit;
    ChanceEdit: TFloatSpinEdit;
    Label1x1a: TLabel;
    Label1x1b: TLabel;
    PageControl1: TPageControl;
    ControlEdit: TSpinEdit;
    Tab1x2: TTabSheet;
    Tab2x2: TTabSheet;
    TabDPrime: TTabSheet;
    TabRight: TTabSheet;
    TabOblique: TTabSheet;
    ExptlEdit: TSpinEdit;
    BEdit: TSpinEdit;
    CEdit: TSpinEdit;
    DEdit: TSpinEdit;
    MissEdit: TSpinEdit;
    crEdit: TSpinEdit;
    faEdit: TSpinEdit;
    HitsEdit2: TSpinEdit;
    MissEdit2: TSpinEdit;
    crEdit2: TSpinEdit;
    faEdit2: TSpinEdit;
    AdjEdit: TFloatSpinEdit;
    OppEdit: TFloatSpinEdit;
    HypEdit: TFloatSpinEdit;
    procedure EditCopyMenuClick(Sender: TObject);
    procedure DPrimeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Help1x2ItemClick(Sender: TObject);
    procedure HelpAboutMenuClick(Sender: TObject);
    procedure Help2x2ItemClick(Sender: TObject);
    procedure HelpDPrimeItemClick(Sender: TObject);
    procedure HelpObliqueItemClick(Sender: TObject);
    procedure HelpRightItemClick(Sender: TObject);
    procedure ObliqueChange(Sender: TObject);
    procedure RightChange(Sender: TObject);
    procedure x2x2Change(Sender: TObject);
    procedure x1x2Change(Sender: TObject);
    procedure ReportD(lnhits, lnmiss, lncr, lnfa: longint;
      var ldprime, lCR, lV: extended; var lOK: boolean);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }
function RadToDeg(lRad: extended): extended;
begin
  Result := lRad * (180 / pi);
end;

function DegToRad(lDeg: extended): extended;
begin
  Result := lDeg * (pi / 180);
end;

function doubleToStr(lR: extended; lDec: integer): string;
var
  lStr: string;
  lRvar: extended;
  lInc, lMult: longint;
begin
  lMult := 1;
  if lDec > 0 then
    for lInc := 1 to lDec do
      lMult := lMult * 10;
  lRvar := (round(lR * lMult)) / lMult;
  Str(lRvar: 0: lDec, lStr);
  doubleToStr := lStr;
end;

function SSScompute(l1, l2, l3: double): double;
var
  lCrad: double;
begin
  lCrad := (-(2 * l2 * l3));
  Result := 90;//-666;
  if lCrad = 0 then
  begin
    //showmessage('xx');
    exit;
  end;
  lCrad := (l1 * l1 - (l2 * l2 + l3 * l3)) / lCrad;
  if lCrad = 0 then
  begin
    //showmessage('aa');
    exit;
  end;
  if lCrad < 0 then
    lCrad := 180 + RadToDeg(ArcTan(sqrt(1 - sqr(lCrad)) / lCrad))
  else
    lCrad := RadToDeg(ArcTan(sqrt(1 - sqr(lCrad)) / lCrad));
  Result := lCrad;
  //ArcCos(x) = ArcTan (sqrt (1-sqr (x)) /x)
end;

function SAA(lSknown, lAknown, lA: double): double;
begin
  Result := (lSknown * sin(degtorad(lA))) / sin(degtorad(lAknown));
end;

procedure TMainForm.x2x2Change(Sender: TObject);
var
  lN, lA, lB, lC, lD, lInc: longint;
  lFish1X, lFish2X: double;
  lMinExp, lChi, lChip, luChi, luChiP: double;
  lFish1Str, lFish2Str, lMinExpStr, lChiStr, lChiPStr, luChiStr, luChiPStr: string[10];
begin
  lA := AEdit.Value;
  lB := BEdit.Value;
  lC := CEdit.Value;
  lD := DEdit.Value;
  lN := lA + lB + lC + lD;
  Chi2x2(lA, lB, lC, lD, lMinExp, lChi, lChip, luChi, luChiP);
  lMinExpStr := doubletostr(lMinExp, 1);
  lChiStr := doubletostr(lChi, 3);
  lChiPStr := doubletostr(lChiP, 3);
  luChiStr := doubletostr(luChi, 3);
  luChiPStr := doubletostr(luChip, 3);
  if lN > kMaxFact then
  begin
    lFish1Str := 'N/A';
    lFish2Str := 'N/A';
  end
  else
  begin

    lFish2X := Fisher2Tail(lA, lB, lC, lD);
    lFish2Str := doubletostr(lFish2X, 5);
    lFish1X := Fisher(lA, lB, lC, lD);
    lFish1Str := doubletostr(lFish1X, 5);
  end;
  x2memo.Clear;
  x2Memo.Lines.Add('N= ' + IntToStr(lN));
  x2Memo.Lines.Add('Minimum Expected = ' + lMinExpStr);
  x2Memo.Lines.Add('Chi DF1 = ' + luChiStr + ' p<' + luChiPStr);
  x2Memo.Lines.Add('Yates'' Chi DF1 = ' + lChiStr + ' p<' + lChiPStr);
  x2Memo.Lines.Add('Fisher 1-tail p<' + lFish1Str);
  x2Memo.Lines.Add('Fisher 2-tail p<' + lFish2Str);
  //x2Memo.Lines.Add('t-test I p<'+doubletostr(TStat2 (lA,lB,lC,lD),6) );
  //x2Memo.Lines.Add('t-test R  p<'+doubletostr(TStatPrecise (lA,lB,lC,lD),6) );
  if (lN + 2) <= kMaxFact then
  begin
    (*if abs(lA-lD) > abs(lB-lC) then
      lFish1X := Fisher (lA+1,lB,lC,lD+1)
    else
      lFish1X := Fisher (lA,lB+1,lC+1,lD); *)
    lFish1X := Lieber1T(lA, lB, lC, lD);
    lFish1Str := doubletostr(lFish1X, 5);
    x2Memo.Lines.Add('Liebermeister 1-tail p<' + lFish1Str);
    lFish1X := FisherMidP(lA, lB, lC, lD);
    lFish1Str := doubletostr(lFish1X, 5);
    x2Memo.Lines.Add('MidP 1-tail p<' + lFish1Str);
  end;
end;

procedure TMainForm.x1x2Change(Sender: TObject);
var
  lN, lControl, lExptl: longint;
  lBiPX, lChiX, lChiPX: double;
  lBiStr, lBiStr2, lChiPStr, lChiStr: string[10];
begin
  {GetInt(Sender);}
  //xRangeCheck(Sender);
  if ControlEdit.Value < ExptlEdit.Value then
  begin
    lControl := round(ControlEdit.Value);
    lExptl := round(ExptlEdit.Value);
  end
  else
  begin
    lExptl := round(ControlEdit.Value);
    lControl := round(ExptlEdit.Value);
  end;
  lN := lControl + lExptl;
  Chi(lN, lExptl, lChiX, lChiPX);
  lChiStr := doubletostr(lChiX, 3);
  lChiPStr := doubletostr(lChiPX, 3);
  x1Memo.Clear;
  if lN < kMaxFact then
  begin
    lBiPX := BiProb(lN, lExptl,{ChanceEdit.value}0.5);
    lBiStr := doubletostr(lBiPX, 3);
    if (2 * lBiPx) > 1 then
      lBiStr2 := '1.000'
    else
      lBiStr2 := doubletostr((2 * lBiPx), 3);
    x1Memo.Lines.Add('N= ' + IntToStr(lN));
    x1Memo.Lines.Add('Chi DF1 = ' + lChiStr + ' p<' + lChiPStr);
    x1Memo.Lines.Add('1-tail Binomial Probability= ' + lBiStr);
    x1Memo.Lines.Add('2-tail Binomial Probability= ' + lBiStr2);
  end
  else
  begin
    x1Memo.Lines.Add('N = ' + IntToStr(lN));
    x1Memo.Lines.Add('Chi DF1 = ' + lChiStr + ' p<' + lChiPStr);
    x1Memo.Lines.Add('Binomial Probability N/A (N>' + IntToStr(kMaxFact) + ')');
  end;
end;

function fSSS(i: TTriangle): TTriangle;
begin
  result := i;
  result.angleA := -1; //error
  if (i.sizeA <= 0) or (i.sizeB <= 0) or (i.sizeC <= 0) then
    exit;
  if (i.sizeA >= (i.sizeB + i.sizeC)) or (i.sizeB >= (i.sizeA + i.sizeC)) or (i.sizeC >= (i.sizeA + i.sizeB)) then
    exit;
  result.angleA := SSScompute(i.sizeA, i.sizeB, i.sizeC);
  result.angleB := SSScompute(i.sizeB, i.sizeA, i.sizeC);
  result.angleC := SSScompute(i.sizeC, i.sizeA, i.sizeB);
end;

function fSAS(i: TTriangle): TTriangle;
var
	x:  TTriangle;
begin
  x := i;
  x.sizeB := sqrt(sqr(i.sizeA) + sqr(i.sizeC) - (2 * i.sizeA * i.sizeC * cos(degtorad(i.angleB))));
  result := fSSS(x);
end;

function fAAS(i: TTriangle): TTriangle;
//size A known, two of the angles known
begin
  result := i;
  if (i.angleA <= 0) then
  	result.angleA := 180 - result.angleB - result.angleC
  else if (i.angleB <= 0) then
  	result.angleB := 180 - result.angleA - result.angleC
  else
  	result.angleC := 180 - result.angleA - result.angleB;

  if ((result.sizeA <= 0)  or (min(min(result.angleA, result.angleB), result.angleC) <= 0)
  	or (max(max(result.angleA, result.angleB), result.angleC) >= 180)) then begin
    result.angleA := -1; //error
  	exit;
  end;
  result.sizeB := SAA(result.sizeA, result.angleA, result.angleB);
  result.sizeC := SAA(result.sizeA, result.angleA, result.angleC);
end;

procedure TMainForm.ObliqueChange(Sender: TObject);
var
  t, r: TTriangle;
begin
    t.sizeA := max(vOsa.Value, 0);
    t.sizeB := max(vOsb.Value, 0);
    t.sizeC := max(vOsc.Value, 0);
	t.angleA := vOaa.Value;
	t.angleB := vOab.Value;
	t.angleC := vOac.Value;
    r := t;
    OMemo.lines.clear();
    case DropO.ItemIndex of
      0: begin
        t.angleA := -1;
        t.angleB := -1;
        t.angleC := -1;
        r := fSSS(t);
      end;
      1: begin
        t.sizeB := -1;
        t.angleA := -1;
        t.angleC := -1;
        r := fSAS(t);
      end;
      2: begin
        t.sizeB := -1;
        t.sizeC := -1;
        t.angleC := -1;
        r := fAAS(t);
      end;
      3: begin
        t.sizeB := -1;
        t.sizeC := -1;
        t.angleA := -1;
        r := fAAS(t);
      end;
    end; //obliquebox itemindex;
    //hide unused inputs
    vOsa.Visible := (t.sizeA >= 0);
    vOsb.Visible := (t.sizeB >= 0);
    vOsc.Visible := (t.sizeC >= 0);
    vOaa.Visible := (t.angleA >= 0);
    vOab.Visible := (t.angleB >= 0);
    vOac.Visible := (t.angleC >= 0);
    //show results
    if (r.angleA < 0) then begin //error
    	OMemo.lines.add('Inputs do not define a valid triangle');
        exit;
    end;
    OMemo.Lines.Add(' a length  ' + doubletostr(r.sizeA, 2) + ' A angle ' + doubletostr(r.angleA, 2));
    OMemo.Lines.Add(' b length  ' + doubletostr(r.sizeB, 2) + ' B angle ' + doubletostr(r.angleB, 2));
    OMemo.Lines.Add(' c length  ' + doubletostr(r.sizeC, 2) + ' C angle ' + doubletostr(r.angleC, 2));
end;

procedure Alpha({lhit}Y, X{lfa}: double; var lA, lB: double);
begin
  lA := -1;
  lB := -1;
  if ((4 * Y) * (1 - x)) <> 0 then
  begin
    lA := 0.5 + ((Y - X) * (1 + Y - X) / ((4 * Y) * (1 - x)));
  end;
  if (Y * (1 - Y) + x * (1 - x)) <> 0 then
  begin
    lB := (Y * (1 - Y) - x * (1 - x)) / (Y * (1 - Y) + x * (1 - x));
  end;
end;

function X(lIn: double): double;
begin
  X := (Power((2 * Pi), -0.5)) * Exp(-0.5 * (sqr(z(lIn))));
end;

procedure DVariance(lNhitmiss, lNcrfa: longint; lHitrate, lFArate: double;
  var lv, lSE, lCI, lCCI: double);
{variance, Standard Error, Confidence interval, C Confidence interval}
var
  lvHM, lvCF: double;
begin
  lvHM := lnHitMiss;
  lvCF := lNcrfa;
  lvHM := (lHitrate * (1 - lHitrate)) / (lvHM * (sqr(X(lHitrate))));
  lvCF := (lFArate * (1 - lFArate)) / (lvCF * (sqr(X(lFArate))));
  lv := lvCF + lvHM;
  lSE := power(lv, 0.5);
  lCI := 1.96 * lSE;
  lCCI := 1.96 * (power(0.25 * lv, 0.5));
end;

procedure TMainForm.ReportD(lnhits, lnmiss, lncr, lnfa: longint;
  var ldprime, lCR, lV: double; var lOK: boolean);
var
  lN: longint;
  lhitrate, lfarate, lbeta, lA, lB, lSE, lCI, lCCI: double;
  lAdjStr{,lAStr,lBStr}: string[10];
  lerror, ladj: boolean;
begin
  lOK := False;
  lN := lnHits + lnMiss + lnCr + lnFA;
  DPrime(lnhits, lnmiss, lncr, lnfa,
    lerror, ladj,
    lhitrate, lfarate, ldprime, lbeta, lcr);
  if not lError then
  begin
    Alpha(lhitrate, lfarate, lA, lB);
    Dvariance(lnhits + lnmiss, lncr + lnfa, lHitRate, lFARate, lv, lSE, lCI, lCCI);
    if ladj then lAdjStr := '  Adjusted'
    else
      lAdjStr := '';
    DMemo.Lines.Add(' N  ' + IntToStr(lN) + ' beta ' + doubletostr(lbeta, 2) + lAdjStr);
    DMemo.Lines.Add(' D'' ' + doubletostr(ldprime, 2) + ' (95% ' +
      doubletostr(lDprime - abs(lCI), 2) + '..' + doubletostr(lDprime + abs(lCI), 2) + ')');
    DMemo.Lines.Add(' C ' + doubletostr(lcr, 2){+ xxx} +
      ' (95% ' + doubletostr(lCR - abs(lCCI), 2) + '..' + doubletostr(lCR + abs(lCCI), 2) + ')');
    DMemo.Lines.Add(' Hit rate ' + IntToStr(round(100 * lhitrate)) +
      '%  FA rate ' + IntToStr(round(100 * lfarate)) + '%');
    DMemo.Lines.Add(' A'' ' + doubletostr(lA, 2) + '  B ' + doubletostr(lB, 2));
    lOK := True;
  end
  else
    DMemo.Lines.Add(' Cannot compute D'' ');

end;

procedure TMainForm.DPrimeChange(Sender: TObject);
var
  lD1, lC1, lV1, lD2, lC2, lV2: double;
  lOK1, lOK2: boolean;

  procedure Confidence(lLabel: string; lVal, lCI: double);
  var {lStr: string[10];}
    lCImax, lCIMin, lInf, lV: double;
  begin
    lInf := MaxInt / 0.01;
    lV := (abs(lVal)) / (abs(lCI / 1.96));
    lCImax := abs(lVal) + abs(lCI);
    lCImin := abs(lVal) - abs(lCI);
    if lV <> 0 then
      lV := betai(0.5 * lInf, 0.5, lInf / (lInf + sqr(lv)))
    else
      lV := 1.0;
    DMemo.Lines.Add(' ' + lLabel + doubletostr(lV, 3) + ' (95% ' +
      doubletostr(lCImin, 2) + '..' + doubletostr(lCImax, 2) + ')');
  end;

begin
  DMemo.Clear;
  DMemo.Lines.Add('Group 1 Values:');
  ReportD(HitsEdit.Value, MissEdit.Value, crEdit.Value, faEdit.Value, lD1, lC1, lv1, lOK1);
  DMemo.Lines.Add('Group 2 Values:');
  ReportD(HitsEdit2.Value, MissEdit2.Value, crEdit2.Value, faEdit2.Value,
    lD2, lC2, lv2, lOK2);
  if lOK1 and lOK2 then
  begin
    DMemo.Lines.Add('Group 1 vs Group 2:');
    Confidence('D'' p<', lD1 - lD2, 1.96 * (power(lV1 + lV2, 0.5)));
    Confidence('C  p<', lC1 - lC2, 1.96 * (power((0.25 * lV1) + (0.25 * lV2), 0.5)));
  end;
end;

procedure TMainForm.EditCopyMenuClick(Sender: TObject);
begin
  if (PageControl1.ActivePage = Tab1x2) then
  	ClipBoard.AsText := x1Memo.Text
  else if (PageControl1.ActivePage = Tab2x2) then
  	ClipBoard.AsText := x2Memo.Text
  else if (PageControl1.ActivePage = TabDPrime) then
  	ClipBoard.AsText := DMemo.Text
  else if (PageControl1.ActivePage = TabRight) then
  	ClipBoard.AsText := RightMemo.Text
  else if (PageControl1.ActivePage = TabOblique) then
  	ClipBoard.AsText := OMemo.Text;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  {$IFDEF Darwin}
  HelpAboutMenu.Visible := false; //use apple menu
  EditCopyMenu.ShortCut  := ShortCut(Word('C'), [ssModifier]);
  {$ELSE}
  AppleMenu.Visible := False;
  {$ENDIF}
  ControlEdit.MaxValue := kMaxFact;
  ExptlEdit.MaxValue := kMaxFact;
  AEdit.MaxValue := kMaxFact;
  BEdit.MaxValue := kMaxFact;
  CEdit.MaxValue := kMaxFact;
  DEdit.MaxValue := kMaxFact;
  x1x2Change(nil);
  x2x2Change(nil);
  DPrimeChange(nil);
  RightChange(nil);
  ObliqueChange(nil);
end;

procedure TMainForm.Help1x2ItemClick(Sender: TObject);
begin
  ShowMessage('Binomial probability computes the precise odds for 1x2'+
     ' design and is preferable to the estimate generated by'+
     ' the Chi-square test.  Null hypothesis: 50% probability of condition 1.');
end;

procedure TMainForm.HelpAboutMenuClick(Sender: TObject);
begin
  ShowMessage('Statilite version 1.10 by Chris Rorden');
end;

procedure TMainForm.Help2x2ItemClick(Sender: TObject);
begin
   ShowMessage('The Fisher Exact test computes the precise probability'+
     ' for a 2x2 design and is preferable to the estimate of the Chi-square test.');
end;

procedure TMainForm.HelpDPrimeItemClick(Sender: TObject);
begin
  ShowMessage('D'' is a measure of detectability, where D=0 is chance'+
     ' performance. Beta indicates criterion.');
end;

procedure TMainForm.HelpObliqueItemClick(Sender: TObject);
begin
  ShowMessage('Trigonometric functions for sine, cosine, and tangent (SOHCAHTOA).');
end;

procedure TMainForm.HelpRightItemClick(Sender: TObject);
begin
  ShowMessage('Solve using the Pythagorean theorem.');
end;

procedure TMainForm.RightChange(Sender: TObject);
var
  lStr: string[10];
  gOpp, gTheta, gAdj, gThetaRad, gHyp: double;
  bTheta: boolean = True;
  bAdj: boolean = True;
  bHyp: boolean = True;
  bOpp: boolean = True;

begin
  case DropR.ItemIndex of
    1: begin {adjacent}
      gOpp := OppEdit.Value;
      gTheta := ThetaEdit.Value;
      gThetaRad := gTheta * pi / 180;
      if gThetaRad = 0 then
        gAdj := 0
      else
        gAdj := gOpp * cos(gThetaRad) / sin(gThetaRad);
      lStr := doubletostr(((round(gAdj * 100)) / 100), 2);
      AdjLabel.Caption := lStr;
      bAdj := False;
      gHyp := sqrt((gAdj * gAdj) + (gOpp * gOpp));
      HypLabel.Caption := doubletostr(((round(gHyp * 100)) / 100), 2);
      bHyp := False;
    end;
    2: begin {opposite}
      gAdj := AdjEdit.Value;
      gTheta := ThetaEdit.Value;
      gThetaRad := gTheta * pi / 180;
      if gThetaRad = 0 then
        gOpp := 0
      else
        gOpp := gAdj * sin(gThetaRad) / cos(gThetaRad);
      lStr := doubletostr(((round(gOpp * 100)) / 100), 2);
      OppLabel.Caption := lStr;
      bOpp := False;
      gHyp := sqrt((gAdj * gAdj) + (gOpp * gOpp));
      HypLabel.Caption := doubletostr(((round(gHyp * 100)) / 100), 2);
      bHyp := False;
    end;
    3: {compute adjacent+opposite} begin
      gHyp := HypEdit.Value;
      gTheta := ThetaEdit.Value;
      gThetaRad := gTheta * pi / 180;
      gOpp := gHyp * sin(gThetaRad);
      gAdj := gHyp * cos(gThetaRad);
      lStr := doubletostr(((round(gAdj * 100)) / 100), 2);
      AdjLabel.Caption := lStr;
      bAdj := False;
      lStr := doubletostr(((round(gOpp * 100)) / 100), 2);
      OppLabel.Caption := lStr;
      bOpp := False;

    end;
    else
      {0 = compute theta} begin
      gAdj := AdjEdit.Value;
      gOpp := OppEdit.Value;
      if gAdj = 0 then
        gThetaRad := 0
      else
        gThetaRad := arctan(gOpp / gAdj);
      gTheta := 180 * (gThetaRad / pi);
      lStr := doubletostr(((round(gTheta * 100)) / 100), 2);
      ThetaLabel.Caption := lStr;
      bTheta := False;
      gHyp := sqrt((gAdj * gAdj) + (gOpp * gOpp));
      HypLabel.Caption := doubletostr(((round(gHyp * 100)) / 100), 2);
      bHyp := False;
    end;
  end; {case}
  ThetaEdit.Visible := bTheta;
  AdjEdit.Visible := bAdj;
  HypEdit.Visible := bHyp;
  OppEdit.Visible := bOpp;
  RightMemo.Clear;
  RightMemo.Lines.Add('Theta (deg)  ' + doubletostr(gTheta, 2));
  RightMemo.Lines.Add('Adjacent  ' + doubletostr(gAdj, 2));
  RightMemo.Lines.Add('Opposite  ' + doubletostr(gOpp, 2));
  RightMemo.Lines.Add('Hypotenuse  ' + doubletostr(gHyp, 2));

end;

end.
