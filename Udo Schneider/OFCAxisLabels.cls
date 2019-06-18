"Filed out from Dolphin Smalltalk 7"!

OFCBrush subclass: #OFCAxisLabels
	instanceVariableNames: 'axis'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCAxisLabels guid: (GUID fromString: '{34895370-1296-4677-8863-cb33b9c8b5da}')!
OFCAxisLabels comment: ''!
!OFCAxisLabels categoriesForClass!Unclassified! !
!OFCAxisLabels methodsFor!

color: aColorOrHTMLHexString 
	self 
		propertiesAt: #colour
		put: aColorOrHTMLHexString!

properties
	properties isNil
		ifTrue: [properties := axis properties
						at: #labels
						ifAbsentPut: [Dictionary new]].
	^ properties!

rotate: aNumber 
	self propertiesAt: #rotate put: aNumber!

setOFCCanvas: anOFCCanvas axis: anOFCAxis 
	self setOFCCanvas: anOFCCanvas.
	axis := anOFCAxis!

size: anInteger
	self propertiesAt: #size put: anInteger!

steps: anInteger
	self propertiesAt: #steps put: anInteger!

values: anArray 
	self propertiesAt: #labels put: anArray! !
!OFCAxisLabels categoriesFor: #color:!accessing!public! !
!OFCAxisLabels categoriesFor: #properties!accessing!public! !
!OFCAxisLabels categoriesFor: #rotate:!accessing!public! !
!OFCAxisLabels categoriesFor: #setOFCCanvas:axis:!initialize-release!public! !
!OFCAxisLabels categoriesFor: #size:!accessing!public! !
!OFCAxisLabels categoriesFor: #steps:!accessing!public! !
!OFCAxisLabels categoriesFor: #values:!accessing!public! !

!OFCAxisLabels class methodsFor!

icon
^String icon! !
!OFCAxisLabels class categoriesFor: #icon!public! !

