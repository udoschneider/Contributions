"Filed out from Dolphin Smalltalk 7"!

OFCEuclidianAxis subclass: #OFCYAxis
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCYAxis guid: (GUID fromString: '{0ed40ad5-f481-4a96-bba9-192a8ad1c842}')!
OFCYAxis comment: ''!
!OFCYAxis categoriesForClass!Unclassified! !
!OFCYAxis methodsFor!

elementType
	^ #'y_axis'!

labels
	self shouldNotImplement!

labels: aBlock 
	^ self labels
		with: aBlock;
		yourself!

labelValues: aCollectionOfStrings 
	self 
		propertiesAt: #labels
		put: aCollectionOfStrings!

tickSize: anInteger 
	self propertiesAt: #'tick-length' put: anInteger! !
!OFCYAxis categoriesFor: #elementType!constants!public! !
!OFCYAxis categoriesFor: #labels!accessing!public! !
!OFCYAxis categoriesFor: #labels:!accessing!public! !
!OFCYAxis categoriesFor: #labelValues:!accessing!public! !
!OFCYAxis categoriesFor: #tickSize:!accessing!public! !

