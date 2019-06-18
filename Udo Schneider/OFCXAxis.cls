"Filed out from Dolphin Smalltalk 7"!

OFCEuclidianAxis subclass: #OFCXAxis
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCXAxis guid: (GUID fromString: '{b2356117-638d-4112-a602-0cb63e7bad2e}')!
OFCXAxis comment: ''!
!OFCXAxis categoriesForClass!Unclassified! !
!OFCXAxis methodsFor!

elementType
	^ #'x_axis'!

threeD: anInteger 
	self propertiesAt: #'3d' put: anInteger!

tickSize: anInteger 
	self propertiesAt: #'tick-height' put: anInteger! !
!OFCXAxis categoriesFor: #elementType!constants!public! !
!OFCXAxis categoriesFor: #threeD:!accessing!public! !
!OFCXAxis categoriesFor: #tickSize:!accessing!public! !

