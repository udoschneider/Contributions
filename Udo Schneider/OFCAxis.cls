"Filed out from Dolphin Smalltalk 7"!

OFCChartDecoration subclass: #OFCAxis
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCAxis guid: (GUID fromString: '{20a51df8-7903-44a6-abef-e68bd79b5cba}')!
OFCAxis comment: ''!
!OFCAxis categoriesForClass!Unclassified! !
!OFCAxis methodsFor!

color: aColorOrHTMLHexString 
	self 
		propertiesAt: #colour
		put: aColorOrHTMLHexString!

gridColor: aColorOrHTMLHexString 
	self 
		propertiesAt: #'grid-colour'
		put: aColorOrHTMLHexString!

labels
	^ OFCAxisLabels new
		setOFCCanvas: ofcCanvas
			axis: self;
		yourself!

labels: aBlock 
	^ self labels
		with: aBlock;
		yourself!

max: aNumber 
	self 
		propertiesAt: #max
		put: aNumber!

min: aNumber 
	self 
		propertiesAt: #min
		put: aNumber!

steps: aNumber 
	self 
		propertiesAt: #steps
		put: aNumber! !
!OFCAxis categoriesFor: #color:!accessing!public! !
!OFCAxis categoriesFor: #gridColor:!accessing!public! !
!OFCAxis categoriesFor: #labels!accessing!public! !
!OFCAxis categoriesFor: #labels:!accessing!public! !
!OFCAxis categoriesFor: #max:!accessing!public! !
!OFCAxis categoriesFor: #min:!accessing!public! !
!OFCAxis categoriesFor: #steps:!accessing!public! !

!OFCAxis class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\OFCAxis.ico'! !
!OFCAxis class categoriesFor: #icon!public! !

