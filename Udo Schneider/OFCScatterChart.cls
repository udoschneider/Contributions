"Filed out from Dolphin Smalltalk 7"!

OFCNamedChart subclass: #OFCScatterChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCScatterChart guid: (GUID fromString: '{3a972261-32b1-4456-91b7-d75f51339f96}')!
OFCScatterChart comment: ''!
!OFCScatterChart categoriesForClass!Unclassified! !
!OFCScatterChart methodsFor!

dotHaloSize: aNumber 
	self 
		propertiesAt: #'halo-size'
		put: aNumber!

dotSize: aNumber 
	self 
		propertiesAt: #'dot-size'
		put: aNumber!

elementType
	^ #'scatter'!

lineColor: aColorOrHTMLHexString 
	self 
		propertiesAt: #colour
		put: aColorOrHTMLHexString!

lineWidth: aNumber 
	self 
		propertiesAt: #width
		put: aNumber!

values: anArrayOfPoints 
	self 
		propertiesAt: #values
		put: anArrayOfPoints! !
!OFCScatterChart categoriesFor: #dotHaloSize:!accessing!public! !
!OFCScatterChart categoriesFor: #dotSize:!accessing!public! !
!OFCScatterChart categoriesFor: #elementType!constants!public! !
!OFCScatterChart categoriesFor: #lineColor:!accessing!public! !
!OFCScatterChart categoriesFor: #lineWidth:!accessing!public! !
!OFCScatterChart categoriesFor: #values:!accessing!public! !

!OFCScatterChart class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\OFCScatterChart.ico'! !
!OFCScatterChart class categoriesFor: #icon!public! !

