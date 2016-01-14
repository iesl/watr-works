# Annotation examples

## Sample method sections

### Example

> 2. Experimental

##### Entity + operation + single-step
> __LiNi0.4Mn0.4Co0.2O2__ was __synthesised__ using solid-state synthesis route at __high temperature__ by __mixing__ __metal acetate salts__ (__Aldrich__) as the starting __materials__.

##### Coreference labels (includes lines from steps below)
    e1     : __metal acetate salts__ (
    e1     : __Aldrich__) as the starting      (1)
    e1     : __materials__.
    e1     : __acetates__ of
    e1     : __samples__ were


##### Step component labels
    output    : __LiNi0.4Mn0.4Co0.2O2__ was
              : __synthesised__ using solid-state synthesis route at
    condition : __high temperature__ by
    action    : __mixing__
    input     : __metal acetate salts__ (
    input     : __Aldrich__) as the starting      (1)
    input     : __materials__.

##### Entity + operation + single-step
> __Stoichiometric__ amount of __acetates__ of __lithium nickel__, __manganese__, and __cobalt__ were __mixed__ and __ground__ by using a __pestle__ and __mortar__.

##### Step component labels
    input-quantity : __Stoichiometric__ amount of
                   : __acetates__ of
    input          : __lithium nickel__,
    input          : __manganese__, and
    input          : __cobalt__ were
    action         : __mixed__ and
    action         : __ground__ by using a
    apparatus      : __pestle__ and
    apparatus      : __mortar__.


##### Entity + operation + single-step
> After that, the __samples__ were __heated__ at __400 °C__ for __12 h__ with a heating rate of __2 °C/min__ to __remove__ __acetates__.

##### Step component labels
                   : After that, the
    input          : __samples__ were
    action         : __heated__ at
    condition      : __400 °C__ for
    condition      : __12 h__ with a heating rate of
    condition      : __2 °C/min__ to
                   : __remove__                              (2)
                   : __acetates__.                           (2)


> Then, samples were further heated at temperatures between 800 and 950 °C for 12 h in oxygen and in air with intermittent grinding.


> Samples were analysed using a Bruker D2 Phaser benchtop X-ray diffractometer equipped with LYNXEYE 1Ddetector with Cu–Kα radiation at room temperature.
> The samples were analysed using a step size of 0.02° and dwell time of 0.2 s per step.

##### Step component labels
    input     : Samples were
    action    : analysed using a
    apparatus : Bruker D2 Phaser benchtop
    apparatus : X-ray diffractometer equipped with
    apparatus : LYNXEYE 1D detector with
    condition : Cu–Kα radiation at
    condition : room temperature.

              : The
    input     : samples were
    action    : analysed using a
    condition : step size of             (3)
              : 0.02° and
    condition : dwell time of            (3)
              : 0.2 s per
              : step.


> Rietveld refinements of XRD data were performed using GSAS/ EXPGUI [13,14] software to determine the structural parameters and the amount of cation disorder.
> The refinement strategy that imposed to validate the refinement model was performed according to the literatures [15–17].
>
> The electrodes were electrochemically characterised by using a 2016 coin cell (20 mm outside diameter and 1.6 mm thickness).
> The cells were fabricated in the argon-filled glove box with lithium foil as the anode, and 1 M LiPF6 in ethylene carbonate (EC) + dimethyl carbonate (DMC) (with a mixing ratio of 1:1 V/V) as the electrolyte.
> The cells were tested using Cyclic Voltammetry Macpile II (Biologic, France) to determine the oxidation/reduction of the samples at a sweep rate of 0.058 mV s−1 voltage range of 2.5–4.4 V.
> Bitrode battery tester (Model SCN-12-4-5/18, USA) was used to perform galvanostatic cycling in order to assess the performance of the cells.
> The cells were tested by charging and discharging in the voltage range of 2.5–4.5 V (vs. Li).
> These tests were carried out at room temperature at a constant current density of 15 mA g−1.


### Example

> 2. Experimental
>
> 2.1. Chemicals and materials
>
> Standards of the __PUHs__ (__monuron__, __isoproturon__, __diuron__ and __buturon__) were purchased from __Aladdin-reagent__ (Shanghai, China).
> The chemical structures of the __herbicides__ are shown in Fig. 1.
> Chromatography-grade __acetonitrile__ was __purchased__ from __Huaxin Chemical Reagent Company__ (Baoding, China).
> __Methanol__ was obtained from __Kermel Chemical Reagents Company__ (Tianjin, China).
> Sodium chloride (NaCl) was from Tianjin Fuchen Chemical Reagent Factory (Tianjin, China).
> Acetone, hydrochloric acid (HCl), sodium hydroxide (NaOH), cobalt nitrate hexahydrate (Co(NO3)26H2O), 2-methylimidazole, ethanol and all other reagents were purchased from Beijing Chemical Reagents Company (Beijing, China).
> The __water__ used throughout the work was __purified__ by a __SZ-93 automatic double-distiller__ purchased from Yarong Biochemistry Instrumental Factory (Shanghai, China).
> The __grape__ and bitter __gourd__ samples were bought from the local market (Baoding, China).


> A mixture stock solution containing monuron, isoproturon, diuron and buturon each at 20.0 mg L1 was prepared in methanol.

##### Step component labels
                : A mixture stock
    input       : solution containing
                : monuron,
                : isoproturon,
                : diuron and
                : buturon each at
    in-quantity : 20.0 mg
                : L1 was
    action      : prepared in
    condition   : methanol.            (4)

> A series of standard solutions were prepared by mixing an appropriate amount of the stock solution with methanol in a 10-mL volumetric flask.
> All the standard solutions were stored at 4 C and protected from light.



### Example

> CHEMICAL SYNTHESIS
>
> The exploration of the system Tl-Ba-Cl-O was performed,
> starting from mixtures of TlCl, Tl<sub>2</sub>O<sub>3</sub>, BaO, and BaO<sub>2</sub>
> pressed into the form of bars and heated in evacuated silica tubes at temperatures ranging from 530°C to 660°C.
>
> The new phase Ba<sub>3</sub>Tl<sub>2</sub>O<sub>5</sub>C1<sub>2</sub> was synthesized
> for the nominal composition Ba<sub>2.8</sub>Tl<sub>2.2</sub>O<sub>4.5</sub>C1<sub>2</sub> ,
> heated slowly up to 6OO”C, maintained at that temperature for 6 hours, and then slowly cooled down to room temperature.
>
> Under these conditions, the phase appears as practically pure from its powder XRD pattern, which evidences only traces of BaCO,.



### Isolated examples for annotation types

#### Entities
* preparation
* BaV13O18
* Tl-Ba-Cl-O
* mixtures
* TlCl
* bars
* samples

#### Operations

##### Actions
  * prepared
  * pressed
  * heated
  * synthesised
  * decomposition
  * reactions

##### Conditions
* 530°C
* wet               'wet process'
* solid-state
* in air
* in oxygen


##### Apparatus
* arc furnace
* silica tubes
* X-ray diffractometer
* LYNXEYE 1D detector

##### Settings
+ step size
+ dwell time



## Results/analysis

### examples
+ that they form a __single phase__ with __orthorhombic structure__


## Quantities
    * duration
    * rate
    * temperature
    * purity     e.g., ReO3 (99.9%)



### Notes
+ labeling phrases like 'the sample' is tricky - it can refer to an ever-changing substance over the course of several steps
+ Organization show up frequently, perhaps should be labeled someday.


### Questions
1. Aldritch? branded formulation of some sort?
2. The output is specified by the thing removed. We don't know how to describe that yet.
3. Should this be labeled as "setting", applicable only to an apparatus?
4. This appears to be a condition, not an entity.
