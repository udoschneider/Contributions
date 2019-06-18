"Filed out from Dolphin Smalltalk 7"!

Object subclass: #OFCCanvas
	instanceVariableNames: 'data'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCCanvas guid: (GUID fromString: '{ce229b69-e9ad-4fc1-bc89-c7009a942d37}')!
OFCCanvas comment: ''!
!OFCCanvas categoriesForClass!Unclassified! !
!OFCCanvas methodsFor!

arrayAt: aByteSymbol 
	^data at: aByteSymbol ifAbsentPut: [OrderedCollection new]!

at: key put: value 
	^ data 
		at: key
		put: value!

background
	^ self newBrush: OFCBackground!

background: aBlock 
	^ self background
		with: aBlock;
		yourself!

backgroundColor: aColor 
	"shortcut for
		ofcCanvas background color: aColor
		
	This was done to have access to a seperate background object in case
	more than just a simple color will be supported in the future."
	self background color: aColor!

barChart
	^ self newBrush: OFCBarChart!

barChart: aBlock 
	^ self barChart
		with: aBlock;
		yourself!

cylinderChart
	^ self newBrush: OFCCylinderChart!

cylinderChart: aBlock 
	^ self cylinderChart
		with: aBlock;
		yourself!

data
	^ data!

dictionaryAt: aByteSymbol 
	^data at: aByteSymbol ifAbsentPut: [Dictionary new]!

dottedLineChart
	^ self newBrush: OFCDottedLineChart!

dottedLineChart: aBlock 
	^ self dottedLineChart
		with: aBlock;
		yourself!

glassBarChart
	^ self newBrush: OFCGlassBarChart!

glassBarChart: aBlock 
	^ self glassBarChart
		with: aBlock;
		yourself!

glassRoundBarChart
	^ self newBrush: OFCRoundGlassBarChart!

glassRoundBarChart: aBlock 
	^ self glassRoundBarChart
		with: aBlock;
		yourself!

hollowAreaChart
	^ self newBrush: OFCHollowAreaChart!

hollowAreaChart: aBlock 
	^ self hollowAreaChart
		with: aBlock;
		yourself!

hollowLineChart
	^ self newBrush: OFCHollowLineChart!

hollowLineChart: aBlock 
	^ self hollowLineChart
		with: aBlock;
		yourself!

horizontalBarChart
	^ self newBrush: OFCHorizontalBarChart!

horizontalBarChart: aBlock 
	^ self horizontalBarChart
		with: aBlock;
		yourself!

initialize
	super initialize.
	data := Dictionary new!

jsonSaveOn: aJSONFiler 
	^data jsonSaveOn: aJSONFiler!

lineAreaChart
	^ self newBrush: OFCLineAreaChart!

lineAreaChart: aBlock 
	^ self lineAreaChart
		with: aBlock;
		yourself!

lineChart
	^ self newBrush: OFCLineChart!

lineChart: aBlock 
	^ self lineChart
		with: aBlock;
		yourself!

lineScatterChart
	^ self newBrush: OFCLineScatterChart!

lineScatterChart: aBlock 
	^ self lineScatterChart
		with: aBlock;
		yourself!

newBrush: anOFCObjectClass 
	^ anOFCObjectClass new
		setOFCCanvas: self;
		yourself!

outlinedBarChart
	^ self newBrush: OFCOutlinedBarChart!

outlinedBarChart: aBlock 
	^ self outlinedBarChart
		with: aBlock;
		yourself!

pieChart
	^ self newBrush: OFCPieChart!

pieChart: aBlock 
	^ self pieChart
		with: aBlock;
		yourself!

radarAxis
	^ self newBrush: OFCRadarAxis!

radarAxis: aBlock 
	^ self radarAxis
		with: aBlock;
		yourself!

render: aBlock 
	"Render the chart definition aBlock on self"

	aBlock value: self.
	^self jsonString asString!

scatterChart
	^ self newBrush: OFCScatterChart!

scatterChart: aBlock 
	^ self scatterChart
		with: aBlock;
		yourself!

shape
	^ self newBrush: OFCShape !

shape: aBlock 
	^ self shape
		with: aBlock;
		yourself!

sketchBarChart
	^ self newBrush: OFCSketchBarChart!

sketchBarChart: aBlock 
	^ self sketchBarChart
		with: aBlock;
		yourself!

stackedBarChart
	^ self newBrush: OFCStackedBarChart!

stackedBarChart: aBlock 
	^ self stackedBarChart
		with: aBlock;
		yourself!

threeDBarChart
	^ self newBrush: OFC3dBarChart!

threeDBarChart: aBlock 
	^ self threeDBarChart
		with: aBlock;
		yourself!

title
	^ self newBrush: OFCTitle!

title: aBlock 
	^ self title
		with: aBlock;
		yourself!

tooltipStyle
	^ self newBrush: OFCTooltipStyle!

tooltipStyle: aBlock 
	^ self tooltipStyle
		with: aBlock;
		yourself!

xAxis
	^ self newBrush: OFCXAxis!

xAxis: aBlock 
	^ self xAxis
		with: aBlock;
		yourself!

xLegend
	^ self newBrush: OFCXLegend!

xLegend: aBlock 
	^ self xLegend
		with: aBlock;
		yourself!

yAxis
	^ self newBrush: OFCYAxis!

yAxis: aBlock 
	^ self yAxis
		with: aBlock;
		yourself!

yAxisRight
	^ self newBrush: OFCYAxisRight!

yAxisRight: aBlock 
	^ self yAxisRight
		with: aBlock;
		yourself!

yLegend
	^ self newBrush: OFCYLegend!

yLegend: aBlock 
	^ self yLegend
		with: aBlock;
		yourself! !
!OFCCanvas categoriesFor: #arrayAt:!accessing-helpers!public! !
!OFCCanvas categoriesFor: #at:put:!accessing!public! !
!OFCCanvas categoriesFor: #background!elements!public! !
!OFCCanvas categoriesFor: #background:!elements!public! !
!OFCCanvas categoriesFor: #backgroundColor:!elements!public! !
!OFCCanvas categoriesFor: #barChart!charts-bar!public! !
!OFCCanvas categoriesFor: #barChart:!charts-bar!public! !
!OFCCanvas categoriesFor: #cylinderChart!charts-bar!public! !
!OFCCanvas categoriesFor: #cylinderChart:!charts-bar!public! !
!OFCCanvas categoriesFor: #data!accessing!public! !
!OFCCanvas categoriesFor: #dictionaryAt:!accessing-helpers!public! !
!OFCCanvas categoriesFor: #dottedLineChart!charts-line!public! !
!OFCCanvas categoriesFor: #dottedLineChart:!charts-line!public! !
!OFCCanvas categoriesFor: #glassBarChart!charts-bar!public! !
!OFCCanvas categoriesFor: #glassBarChart:!charts-bar!public! !
!OFCCanvas categoriesFor: #glassRoundBarChart!charts-bar!public! !
!OFCCanvas categoriesFor: #glassRoundBarChart:!charts-bar!public! !
!OFCCanvas categoriesFor: #hollowAreaChart!charts-area!public! !
!OFCCanvas categoriesFor: #hollowAreaChart:!charts-area!public! !
!OFCCanvas categoriesFor: #hollowLineChart!charts-line!public! !
!OFCCanvas categoriesFor: #hollowLineChart:!charts-line!public! !
!OFCCanvas categoriesFor: #horizontalBarChart!charts-bar!public! !
!OFCCanvas categoriesFor: #horizontalBarChart:!charts-bar!public! !
!OFCCanvas categoriesFor: #initialize!initialization!public! !
!OFCCanvas categoriesFor: #jsonSaveOn:!JSON-writing!public! !
!OFCCanvas categoriesFor: #lineAreaChart!charts-area!public! !
!OFCCanvas categoriesFor: #lineAreaChart:!charts-area!public! !
!OFCCanvas categoriesFor: #lineChart!charts-line!public! !
!OFCCanvas categoriesFor: #lineChart:!charts-line!public! !
!OFCCanvas categoriesFor: #lineScatterChart!charts-scatter!public! !
!OFCCanvas categoriesFor: #lineScatterChart:!charts-scatter!public! !
!OFCCanvas categoriesFor: #newBrush:!helpers!public! !
!OFCCanvas categoriesFor: #outlinedBarChart!charts-bar!public! !
!OFCCanvas categoriesFor: #outlinedBarChart:!charts-bar!public! !
!OFCCanvas categoriesFor: #pieChart!charts-pie!public! !
!OFCCanvas categoriesFor: #pieChart:!charts-pie!public! !
!OFCCanvas categoriesFor: #radarAxis!elements!public! !
!OFCCanvas categoriesFor: #radarAxis:!elements!public! !
!OFCCanvas categoriesFor: #render:!public! !
!OFCCanvas categoriesFor: #scatterChart!charts-scatter!public! !
!OFCCanvas categoriesFor: #scatterChart:!charts-scatter!public! !
!OFCCanvas categoriesFor: #shape!charts-shape!public! !
!OFCCanvas categoriesFor: #shape:!charts-shape!public! !
!OFCCanvas categoriesFor: #sketchBarChart!charts-bar!public! !
!OFCCanvas categoriesFor: #sketchBarChart:!charts-bar!public! !
!OFCCanvas categoriesFor: #stackedBarChart!charts-bar!public! !
!OFCCanvas categoriesFor: #stackedBarChart:!charts-bar!public! !
!OFCCanvas categoriesFor: #threeDBarChart!charts-bar!public! !
!OFCCanvas categoriesFor: #threeDBarChart:!charts-bar!public! !
!OFCCanvas categoriesFor: #title!elements!public! !
!OFCCanvas categoriesFor: #title:!elements!public! !
!OFCCanvas categoriesFor: #tooltipStyle!elements!public! !
!OFCCanvas categoriesFor: #tooltipStyle:!elements!public! !
!OFCCanvas categoriesFor: #xAxis!elements!public! !
!OFCCanvas categoriesFor: #xAxis:!elements!public! !
!OFCCanvas categoriesFor: #xLegend!elements!public! !
!OFCCanvas categoriesFor: #xLegend:!elements!public! !
!OFCCanvas categoriesFor: #yAxis!elements!public! !
!OFCCanvas categoriesFor: #yAxis:!elements!public! !
!OFCCanvas categoriesFor: #yAxisRight!elements!public! !
!OFCCanvas categoriesFor: #yAxisRight:!elements!public! !
!OFCCanvas categoriesFor: #yLegend!elements!public! !
!OFCCanvas categoriesFor: #yLegend:!elements!public! !

!OFCCanvas class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\OFCCanvas.ico'!

new
^super new initialize!

render: aBlock 
	^ self new render: aBlock! !
!OFCCanvas class categoriesFor: #icon!public! !
!OFCCanvas class categoriesFor: #new!public! !
!OFCCanvas class categoriesFor: #render:!instance creation!public! !

