* Encoding: UTF-8.
* Study 1.

** Validation of groups.

T-TEST GROUPS=vviq.group(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=brt.priming
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

SORT CASES  BY vviq.group.
SPLIT FILE LAYERED BY vviq.group.

T-TEST
  /TESTVAL=0.5
  /MISSING=ANALYSIS
  /VARIABLES=brt.priming
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

SPLIT FILE OFF.

CORRELATIONS
  /VARIABLES=vviq.score brt.priming
  /PRINT=TWOTAIL NOSIG FULL
  /MISSING=PAIRWISE.


** Self-reported sympathy.

GLM pet.verbal pet.visual BY vviq.group
  /WSFACTOR=condition 2 Polynomial 
  /METHOD=SSTYPE(3)
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=condition 
  /DESIGN=vviq.group.

T-TEST GROUPS=vviq.group(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=pet.verbal pet.visual
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

CORRELATIONS
  /VARIABLES=vviq.score pet.verbal pet.visual
  /PRINT=TWOTAIL NOSIG FULL
  /MISSING=PAIRWISE.

T-TEST GROUPS=vviq.group(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=tas.score tas.identify tas.describe tas.external
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

CROSSTABS
  /TABLES=vviq.group BY tas.group
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ 
  /CELLS=COUNT
  /COUNT ROUND CELL.


** The role of mental imagery in alexithymia: self-reported sympathy (mediation models only available in PROCESS).


** Physiological response.

GLM T1.visual.physio T2.visual.physio T3.visual.physio T4.visual.physio T5.visual.physio T6.visual.physio T7.visual.physio T8.visual.physio T9.visual.physio 
    T10.visual.physio T11.visual.physio T12.visual.physio T1.verbal.physio T2.verbal.physio T3.verbal.physio T4.verbal.physio T5.verbal.physio T6.verbal.physio 
    T7.verbal.physio T8.verbal.physio T9.verbal.physio T10.verbal.physio T11.verbal.physio T12.verbal.physio BY vviq.group
  /WSFACTOR=modality 2 Polynomial trial 12 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(trial*vviq.group*modality) TYPE=LINE ERRORBAR=SE(1) MEANREFERENCE=NO YAXIS=AUTO
  /PRINT=DESCRIPTIVE ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=modality trial modality*trial
  /DESIGN=vviq.group.

T-TEST GROUPS=vviq.group(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=pet.verbal.physio pet.visual.physio
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

SORT CASES  BY vviq.group.
SPLIT FILE LAYERED BY vviq.group.

CORRELATIONS
  /VARIABLES=pet.visual WITH pet.visual.physio
    /CI CILEVEL(95)
  /PRINT=TWOTAIL NOSIG FULL
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=pet.verbal WITH pet.verbal.physio
    /CI CILEVEL(95)
  /PRINT=TWOTAIL NOSIG FULL
  /MISSING=PAIRWISE.

SPLIT FILE OFF.


** The role of mental imagery in alexithymia: physiological response (mediation models only available in PROCESS).


* Study 2.

* Emotion recognition task.

T-TEST GROUPS=vviq.group(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=ert.total.score ert.total.rt
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

T-TEST GROUPS=vviq.group(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=ert.neutral.score ert.anger.score ert.happy.score ert.surprise.score   ert.sad.score ert.fear.score  ert.disgust.score
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

T-TEST GROUPS=vviq.group(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=ert.neutral.RT ert.anger.RT ert.happy.RT ert.surprise.RT   ert.sad.RT ert.fear.RT  ert.disgust.RT
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).


* The role of alexithymia in the emotion recognition in others.

CORRELATIONS
  /VARIABLES=tas.score ert.total.score ert.total.rt
  /PRINT=TWOTAIL NOSIG FULL
  /MISSING=PAIRWISE.
