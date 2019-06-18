"Filed out from Dolphin Smalltalk 7"!

OFCNamedChart subclass: #OFCBarChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCBarChart guid: (GUID fromString: '{7f0ff844-fada-43ef-a42e-beaf73fbb778}')!
OFCBarChart comment: ''!
!OFCBarChart categoriesForClass!Unclassified! !
!OFCBarChart methodsFor!

elementType
	^ #bar!

fillAlpha: aNumber 
	self 
		propertiesAt: #alpha
		put: aNumber!

fillColor: aColorOrHTMLHexString 
	self 
		propertiesAt: #colour
		put: aColorOrHTMLHexString! !
!OFCBarChart categoriesFor: #elementType!constants!public! !
!OFCBarChart categoriesFor: #fillAlpha:!accessing!public! !
!OFCBarChart categoriesFor: #fillColor:!accessing!public! !

!OFCBarChart class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\OFCBarChart.ico'! !
!OFCBarChart class categoriesFor: #icon!public! !

