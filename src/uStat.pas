Unit uStat;
interface
uses Dialogs;

const
	ITMAX = 300;
	EPS = 3.0e-7;
	kMaxFact = 170;//Lazarus on Windows overflows 1700;  {<= 1754}
var
	gFactRAready : boolean = false;

type
	FactRA = array[0..kMaxFact] of double;
	DataType = Array [1..1] of integer;
	IntPtr= ^DataType;
var
   gFactRA : FactRA;
{prototypes}
function NChooseR (pN, pR: word): double;

function FisherMidP (A,B,C,D: integer): double;
function Liebermeister (Ain,Bin,Cin,Din: integer): double;

function Lieber1T (A,B,C,D: integer): double;
function betai (a, b, x: double): double; {see numerical recipes for Pascal page 188}
procedure AlertMsg (pWarningStr: String);
procedure DPrime (pnhits, pnmisses, pncr, pnfa: longint;
          var error, adjustment: boolean;
          var pHitRate,pFARate, pdprime, pbeta, pcr: double);
procedure Chi2x2 (A, B, C, D: integer; var pMinExp, pChi, p, puChi, pup: double); {FisherExactTest, use instead of chi}
function Fisher (A,B,C,D: integer): double; { use instead of chi2x2}
function Fisher2Tail (A,B,C,D: integer): double;
procedure Chi(pN, pNHits: integer; var pChi, p: double); {pn > 1, pExpected = 0.5}
function BinomialProb(pN, pNHits: integer): double; {pn > 1}
function BiProb(pN, pNHits: integer; lChance: double): double; {pn > 1}
function z (p: double): double;
function Power(B, E:double): double;

implementation

procedure AlertMsg (pWarningStr: String);
begin
     MessageDLG(pWarningStr, mtWarning,[mbOK],0);
end;
function betacf (a, b, x: double): double;
		label
			99;
		const
			itmax = 100;
			eps = 3.0e-7;
		var
			tem, qap, qam, qab, em, d, bz, bpp, bp, bm, az, app, am, aold, ap: double;
			m: integer;
	begin
		am := 1.0;
		bm := 1.0;
		az := 1.0;
		qab := a + b;
		qap := a + 1.0;
		qam := a - 1.0;
		bz := 1.0 - qab * x / qap;
		for m := 1 to itmax do
			begin
				em := m;
				tem := em + em;
				d := em * (b - m) * x / ((qam + tem) * (a + tem));
				ap := az + d * am;
				bp := bz + d * bm;
				d := -(a + em) * (qab + em) * x / ((a + tem) * (qap + tem));
				app := ap + d * az;
				bpp := bp + d * bz;
				aold := az;
				am := ap / bpp;
				bm := bp / bpp;
				az := app / bpp;
				bz := 1.0; {what the hell is this for?}
				if abs(az - aold) < eps * abs(az) then
					goto 99
			end;
                MessageDlg('Error in BetaCF function.', mtWarning, [mbOk],0);
99:
		betacf := az;
	end; {betacf}

	function gammln (xx: double): double;  {Numerical Recipes for Pascal, p 177}
		const
			stp = 2.50662827465;
		var
			x, tmp, ser: double;
	begin
		x := xx - 1.0;
		tmp := x + 5.5;
		tmp := (x + 0.5) * ln(tmp) - tmp;
		ser := 1.0 + 76.18009173 / (x + 1.0) - 86.50532033 /
         (x + 2.0) + 24.01409822 / (x + 3.0) - 1.231739516 / (x + 4.0) + 0.120858003e-2 / (x + 5.0) - 0.536382e-5 / (x + 6.0);
{zzz}		gammln := tmp + ln(stp * ser)
	end; {procedure gammln}

	function betai (a, b, x: double): double; {see numerical recipes for Pascal page 188}
		var
			bt: double;
	begin
                betai := 0;
		if (a=0) or (b=0) or (x < 0.0) or (x > 1.0) then begin
                   MessageDlg('Error in BetaI function: p<0 or p>1.', mtWarning, [mbOk],0);
                   exit;
                end;
  		if (x = 0.0) or (x > 1.0) then
			bt := 0.0
		else
			bt := exp(gammln(a + b) - gammln(a) - gammln(b) + a * ln(x) + b * ln(1.0 - x));
 		if x < (a + 1.0) / (a + b + 2.0) then
			betai := bt * betacf(a, b, x) / a
		else
			betai := 1.0 - bt * betacf(b, a, 1.0 - x) / b
  	end; {betai}
function z (p: double): double;
var
	y: double;
begin
	y := sqrt(-2 * ln(p));
	z := -y + ((((0.0000453642210148 * y + 0.0204231210245) * y + 0.342242088547) * y
+ 1) * y + 0.322232431088) / ((((0.0038560700634 * y + 0.10353775285) * y + 0.531103462366) * y
+ 0.588581570495) * y + 0.099348462606);
end;
procedure DPrime (pnhits, pnmisses, pncr, pnfa: longint;
var error, adjustment: boolean;
var pHitRate,pFARate, pdprime, pbeta, pcr: double);
var
	nhits, nmisses, nfa, ncr, ncatchT, ntargetT,
	{lHitRate, lFARate,}zh, zf: double;
begin
	error := false;
	nhits := pnhits;
	nmisses := pnmisses;
	nfa := pnfa;
	ncr := pncr;
	adjustment := false;
	ntargetT := nHits + nMisses;
	ncatchT := nfa + ncr;
	if (nhits <= 0) or (nmisses < 0) or (nfa < 0) or (ncr <= 0) then
		error := true {cannot compute dprime for this data}
	else begin
		if nmisses = 0 then begin
			nmisses := 1 / (2 * ntargetT);
			nhits := nhits - 1 / (2 * ntargetT);
			adjustment := true;
		end;
		if nfa = 0 then begin
			nfa := 1 / (2 * ncatcht);
			ncr := ncr - 1 / (2 * ncatcht);
			adjustment := true;
		end;
		{pHitPct := round(100*(nhits / ntargetT));
		pFAPct := round(100*(nfa / ncatcht));}
                pHitRate := nhits / ntargetT;
                pFARate := nfa / ncatcht;
		zh := z(pHitRate);
		zf := z(pFARate);
		pdprime := zh - zf;
		pcr := -0.5 * (zh + zf);
		pbeta := exp(-0.5 * (zh * zh - zf * zf));
	end;
end;

procedure InitFact;
var lX: word;
begin
	gFactRA[0]:= 1;
	gFactRA[1] := 1;
	for lx := 2 to kMaxFact do
         gFactRA[lx] := lx * gFactRA[lx-1];
	gFactRAready := true;
end;
 procedure gser(var gamser, a,x, gln: double);
var n: integer;
	sum, del, ap: double;
begin
	gln := gammln(a);
	if x <= 0.0 then begin
		if x < 0.0 then AlertMsg('x less then 0 in routine GSER');
		gamser:= 0.0;
	end else begin
		ap := a;
		sum := 1.0/a;
		del := sum;
		for n := 1 to ITMAX do begin
			ap := ap + 1;
			del := del * (x/ap);
			sum := sum + del;
			if (abs(del) < abs((sum)*EPS) )then begin
				gamser := sum * exp(-x+a*ln(x)-gln);
				exit;
			end;
		end;
		Alertmsg('GSER error: ITMAX too small for requested a-value');
	end;
end;

 procedure gcf(var gammcf: double; a,x, gln: double);
var n: integer;
	gold,fac,b1,b0,a0,g,ana,anf,an,a1: double;
begin
	fac := 1.0;
	b1 := 1.0;
	b0 := 0.0;
	a0 := 1.0;
    gold := 0.0;
	gln := gammln(a);
	a1 := x;
	for n := 1 to ITMAX do begin
		an :=(n);
		ana := an - a;
		a0 := (a1 + a0*ana)*fac;
		b0 := (b1 + b0*ana)*fac;
		anf := an * fac;
		a1 := x*a0+anf*a1;
		b1 := x*b0+anf*b1;
		if a1 <> 0 then begin
			fac := 1.0/a1;
			g := b1*fac;
			if (abs((g-gold)/g)<EPS) then begin
				gammcf := exp(-x+a*ln(x)-gln)*g;
				exit;
			end;
			gold := g;
        end;
	end;
	Alertmsg('GCF error: ITMAX too small for requested a-value');
end;

function gammq( a,x: double): double;
	var gamser, gammcf, gln: double;
begin
        gammq := 0;
	if (x < 0) or (a <= 0.0) then alertmsg('Invalid arguments in routine GAMMQ')
	else begin
		if (x < (a+1.0)) then begin
			gser(gamser,a,x,gln);
			gammq := 1.0 - gamser;
		end else begin
			gcf(gammcf,a,x,gln);
			gammq := gammcf;
		end;
	end;
end;
procedure Chi(pN, pNHits: integer; var pChi, p: double); {pn > 1}
var lExpected: double;
begin
         pChi := 0;
         p := 0;
     if pN > 0 then begin
        lExpected := pN / 2;
        pChi := (sqr(pNHits-lExpected))/lExpected+(sqr((pN-pNHits)-lExpected))/lExpected;
        p := gammq(0.5 {half df},0.5*pChi);
        if p > 1 then p := 1;
     end else begin
         pChi := 0;
         p := 0;
     end;
end;
function NChooseR (pN, pR: word): double;
{var Ldouble: double; }
begin
     if not gFactRAready then InitFact;
     NChooseR := 0;
     if pN <= kMaxFact then
        NChooseR := (gFactRA[pN] )/(gFactRA[pR]*gFactRA[pN-pR]);
{     NChooseR := (subtractorial(pN,(pN-pR)) / (factorial(pR)) ); }
end;
function Power(B, E:double): double;
begin
    Power := 0;
    if B > 0 then
       Power := Exp(E * Ln(B) );
end;
function BinomialProb(pN, pNHits: integer): double; {pn > 1}
{probability of pHits or more in pN attempts}
{pNHits could be between 0 and pN}
{single tail test, double value for 2 tails}
var
   lProb : double;
   lHitCnt, lnHits: integer;
begin
     if pnHits <= (pN div 2) then
        lnHits := pN - pnHits
     else lnHits := pnHits;
     LProb := 0;
     for lHitCnt := pN downto lNHits do
         LProb := lProb + NChooseR(pN, lHitCnt)*(power(0.5,lHitCnt) )*(Power(0.5,(pN-lHitCnt) ) );
     if lProb > 1 then lProb := 1;
     BinomialProb := lProb;
end;

function FisherX (A,B,C,D: integer): double; {FisherExactTest, use instead of chi}
{FisherX computes odds for this specific config only, not more extreme cases}
{alternate to Chi Square, see Siegel & Castellan, Nonparametric Statistics}
{use instead of Chi when n <= 20}
{A= X hits, B= control hits, C = X misses, D = control misses}
var
   N: word;
begin
	 N := A+B+C+D;
	 if (N <= kMaxFact) and (A>=0) and (B>=0) and (C>=0) and (D>=0) and (N > 0) then begin
		FisherX := (
			(gFactRA[A+B]/gFactRA[A])*
			(gFactRA[B+D]/gFactRA[B])*
			(gFactRA[A+C]/gFactRA[C])*
			(gFactRA[C+D]/gFactRA[D])
			)/ gFactRA[N];
	 end else FisherX := 0;
end;
function KingFisher (lSmal,lCross1,lCross2,lSmalDiag: integer): double;
var
   lProb1, lProb2: double;
   lA,lB,lC,lD,lCnt: integer;

begin
	 lA :=lSmal;
	 lB:=lCross1;
	 lC:=lCross2;
	 lD:=lSmalDiag;
	 lProb1:=0;
	 for lCnt := lA downto 0 do begin
	     lProb1 := lProb1 + FisherX(lA,lB,lC,lD);
		 dec(lA);
		 dec(lD);
		 inc(lB);
		 inc(lC);
	 end;
	 lA :=lSmal;
	 lB:=lCross1;
	 lC:=lCross2;
	 lD:=lSmalDiag;
     lProb2:=0;
     while (lB >= 0) and (lC >= 0) do begin
	     lProb2 := lProb2 + FisherX(lA,lB,lC,lD);
		 inc(lA);
		 inc(lD);
		 dec(lB);
		 dec(lC);
     end;
     if lProb1 < lProb2 then
        kingfisher := lProb1
     else
         kingfisher := lProb2;
end;
function Fisher (A,B,C,D: integer): double;
{A= X hits, B= control hits, C = X misses, D = control misses}
begin
     if (A+B+C+D)<1 then Fisher := 1 else begin
     if not gFactRAready then InitFact;
	 if (A<=B) and (A<=C) and (A<=D) then {lA smallest}
        Fisher :=KingFisher(A,B,C,D)
	 else if (B<=C) and (B<=D) then {lB smallest}
          Fisher :=KingFisher(B,A,D,C)
	 else if (C<=D) then {lC smallest}
          Fisher :=KingFisher(C,D,A,B)
     else {d smallest}
          Fisher :=KingFisher(D,C,B,A);
     end;
end;

function Fisher2Tail (A,B,C,D: integer): double;
{1 Jan 1996
 The Fisher Exact Two Tail test requires finding the total sum of ALL
 outcomes that are less likely than the observed outcome.  To do so, we
 find the expected outcome, find out how far off the expected the observed
 outcome is (Diff) and then create the next most unlikely table in the other
 direction (A2,B2,C2,D2) and find its Fisher Exact (One Tail)}

var
  N : integer;
  Expected,Diff : double;
  A2,B2,C2,D2 : integer;

begin
  A2 := 0;
  N := A+B+C+D;
  if N < 1 then Fisher2Tail := 1 else begin
  Expected := 1.0*(A+B)*(A+C)/N; {1.0 needed to cast into double type, instead of using integers for intermediate results}
  Diff := A-Expected;
  if A<Expected then begin
    if (Trunc(Expected-Diff) - (Expected-Diff))<>0.0 then
      A2 := Trunc(Expected-Diff)+1 else A2 := Trunc(Expected-Diff); {Ceil}
  end else if A>Expected then begin
    if Expected-Diff>0 then A2:=Trunc(Expected-Diff) else begin
      if (Trunc(Expected-Diff) - (Expected-Diff))<>0.0 then
        A2:=Trunc(Expected-Diff)-1 else A2:=Trunc(Expected-Diff);
    end; {if Expected-Diff<=0}
  end else if A=Expected then A2:=A-1; {if A=Expected, A2=A-1 or A2=A+1 will work}
  B2 := (A+B)-A2;
  C2 := (A+C)-A2;
  D2 := (B+D)-B2;
  if (A2+B2+C2+D2)<>N then begin
     Fisher2Tail := 0;
     AlertMsg('Error with Fisher 2-tail')
  end else
      Fisher2Tail := Fisher(A,B,C,D) + Fisher(A2,B2,C2,D2);
  end;
end; {Fisher2Tail}
procedure Chi2x2 (A, B, C, D: integer; var pMinExp, pChi, p, puChi, pup: double); {FisherExactTest, use instead of chi}
 {alternate to Chi Square, see Siegel & Castellan, Nonparametric Statistics}
 {use instead of Chi when n <= 20}
 {A= X hits, B= control hits, C = X misses, D = control misses}
 var
    lA, lB, lC, lD, lN: double; {AEXp, BExp, CExp, Dexp, }
    lSameOdds: boolean;
 begin
      lA := A; {convert to double}
      lB := B;
      lC := C;
      lD := D;
      ln := lA + lB + lC + lD;
      if lN > 0 then begin {avoid divide by 0}
         pMinExp := ((lA + lB) * (lA + lC)) / lN;
         if (((lA + lB) * (lB + lD)) / lN) < pMinExp then
            pMinExp := ((lA + lB) * (lB + lD)) / lN;
         if (((lC + lD) * (lA + lC)) / lN) < pMinExp then
            pMinExp := ((lC + lD) * (lA + lC)) / lN;
         if (((lC + lD) * (lB + lD)) / lN) < pMinExp then
            pMinExp := ((lC + lD) * (lB + lD)) / lN;
      end else
          pMinExp := 0;
      lSameOdds := false;
      if (lC > 0) and (lD > 0) then begin
         if (lA / lC) = (lB / lD) then
            lSameOdds := true;
      end;
      if (lC = 0) and (lD = 0) then
         lSameOdds := true;
      if ((lA+lC) = 0) or ((lB+lD) = 0) then
         lSameOdds := true;
      if (lSameOdds = true) then begin
         pChi := 0;   {same odds}
         p := 1.0;
         puChi := 0;
         pup := 1.0;
      end else begin
          puChi := ((sqr((lA * lD) - (lB * lC))) * lN) / ((la + lb) * (lc + ld) * (lb + ld) * (la + lc));
          pup := gammq(0.5, 0.5 * puChi); {half df}
          pChi := ((sqr(abs((lA * lD) - (lB * lC)) - (0.5 * lN))) * lN) / ((la + lb) * (lc + ld) * (lb + ld) * (la + lc));
          p := gammq(0.5, 0.5 * pChi);
      end;
 end;
function BiProb(pN, pNHits: integer; lChance: double): double; {pn > 1}
{probability of pHits or more in pN attempts}
{pNHits could be between 0 and pN}
{single tail test, double value for 2 tails}
var
   lProb : double;
   lHitCnt, lnHits: integer;
begin
     lnHits := pnHits;
          if not gFactRAready then InitFact;
     LProb := 0;
     for lHitCnt := pN downto lNHits do
         lProb := lProb +  ((gFactRA[pN]/(gFactRA[lHitCnt]*gFactRA[pN-lHitCnt]) )*power(lChance,lHitCnt)*power((1-lChance),(pn-lHitCnt)) );
//         LProb := lProb + NChooseR(pN, lHitCnt)*(power(lChance,lHitCnt) )*(Power(1-lChance,(pN-lHitCnt) ) );
     if lProb > 1 then lProb := 1;
     BiProb := lProb;
end;

function Lieber (lSmal,lCross1,lCross2,lSmalDiag: integer): double;
var
   lA,lB,lC,lD,lCnt: integer;
begin
	 lA :=lSmal;
	 lB:=lCross1+1;
	 lC:=lCross2+1;
	 lD:=lSmalDiag;
	 result :=0;
	 for lCnt := lA downto 0 do begin
		 result := result + FisherX(lA,lB,lC,lD);
		 dec(lA);
		 dec(lD);
		 inc(lB);
		 inc(lC);
	 end;
	 //TabbedNotebookDlg.caption := doubletostr(result,6) ;
	 // TabbedNotebookDlg.caption := doubletostr(result,6) ;
	 if result <= 0.5 then
		exit;

	 lA :=lSmal+1;
	 lB:=lCross1;
	 lC:=lCross2;
	 lD:=lSmalDiag+1;
	 result:=0;
	 while (lB >= 0) and (lC >= 0) do begin
		 result := result + FisherX(lA,lB,lC,lD);
		 inc(lA);
		 inc(lD);
		 dec(lB);
		 dec(lC);
	 end;
end;

function Lieber1T (A,B,C,D: integer): double;
{A= X hits, B= control hits, C = X misses, D = control misses}
begin
	 result := 1;
	 if (A+B+C+D)<1 then
		exit;
	 if not gFactRAready then InitFact;
	 if (A<=B) and (A<=C) and (A<=D) then {lA smallest}
		result :=Lieber(A,B,C,D)
	 else if (B<=C) and (B<=D) then {lB smallest}
		  result :=Lieber(B,A,D,C)
	 else if (C<=D) then {lC smallest}
		  result :=Lieber(C,D,A,B)
	 else {d smallest}
		  result :=Lieber(D,C,B,A);
end;

function Liebermeister (Ain,Bin,Cin,Din: integer): double;
var
	A,B,C,D: integer;
{A= X hits, B= control hits, C = X misses, D = control misses}
begin
	 A := Ain;
	 B := Bin;
	 C := Cin;
	 D := Din;
	 if (A+B+C+D)<1 then begin
		result := 1;
		exit;
	 end;
	 //easy way to calculate Lieberman - make more extreme, then calculate Fisher
	 if abs(A-D) > abs(B-C) then begin
		inc(A);
		inc(D);
	 end else begin
		inc(B);
		inc(C);
	 end;
	 if not gFactRAready then InitFact;
	 if (A<=B) and (A<=C) and (A<=D) then {lA smallest}
		result :=KingFisher(A,B,C,D)
	 else if (B<=C) and (B<=D) then {lB smallest}
		  result :=KingFisher(B,A,D,C)
	 else if (C<=D) then {lC smallest}
		  result :=KingFisher(C,D,A,B)
	 else {d smallest}
		  result :=KingFisher(D,C,B,A);
	 if ((A+C)>0) and ((B+D)>0) then begin
		if (A/(A+C)) < (B/(B+D)) then
			result := -result;
	 end;
end;

function KingFisherMidP (lSmal,lCross1,lCross2,lSmalDiag: integer): double;
var
	l1st :boolean;
   lA,lB,lC,lD,lCnt: integer;
begin
	 lA :=lSmal;
	 lB:=lCross1;
	 lC:=lCross2;
	 lD:=lSmalDiag;
	 //result:=0;
	 l1st := true;
	 for lCnt := lA downto 0 do begin
		 if l1st then
			result := 0.5* FisherX(lA,lB,lC,lD)
		 else
			result := result + FisherX(lA,lB,lC,lD);
		 l1st := false;
		 dec(lA);
		 dec(lD);
		 inc(lB);
		 inc(lC);
	 end;
	 if result <= 0.5 then
		exit;
	 lA :=lSmal;
	 lB:=lCross1;
	 lC:=lCross2;
	 lD:=lSmalDiag;
	 //result:=0;
	 l1st := true;
	 while (lB >= 0) and (lC >= 0) do begin
		 if l1st then
			result := 0.5* FisherX(lA,lB,lC,lD)
		 else
			result := result + FisherX(lA,lB,lC,lD);
		 l1st := false;
		 inc(lA);
		 inc(lD);
		 dec(lB);
		 dec(lC);
	 end;
	  //TabbedNotebookDlg.caption := inttostr(lSmal)+'x'+inttostr(lCross1)+'x'+inttostr(lCross2)+'x'+inttostr(lSmalDiag)+'='+doubletostr(lProb1,6) +'  '+doubletostr(lProb2,6);
end;

function FisherMidP (A,B,C,D: integer): double;
{A= X hits, B= control hits, C = X misses, D = control misses}
begin
	 if (A+B+C+D)<1 then result := 1 else begin
	 if not gFactRAready then InitFact;
	 if (A<=B) and (A<=C) and (A<=D) then {lA smallest}
		result :=KingFisherMidP(A,B,C,D)
	 else if (B<=C) and (B<=D) then {lB smallest}
		  result :=KingFisherMidP(B,A,D,C)
	 else if (C<=D) then {lC smallest}
		  result :=KingFisherMidP(C,D,A,B)
	 else {d smallest}
		  result :=KingFisherMidP(D,C,B,A);
	 end;
end;



END.

