"Filed out from Dolphin Smalltalk 7"!

OFCAxis subclass: #OFCEuclidianAxis
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCEuclidianAxis guid: (GUID fromString: '{86a36acb-128b-4415-8d89-7b7643568fec}')!
OFCEuclidianAxis comment: ''!
!OFCEuclidianAxis categoriesForClass!Unclassified! !
!OFCEuclidianAxis methodsFor!

lineWidth: aNumber 
	self 
		propertiesAt: #stroke
		put: aNumber!

offset: aBoolean  
	self propertiesAt: #offset put: (aBoolean ifTrue: [1] ifFalse: [0]) !

tickSize: anInteger 
	self subclassResponsibility! !
!OFCEuclidianAxis categoriesFor: #lineWidth:!accessing!public! !
!OFCEuclidianAxis categoriesFor: #offset:!accessing!public! !
!OFCEuclidianAxis categoriesFor: #tickSize:!accessing!public! !

