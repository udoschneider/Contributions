"Filed out from Dolphin Smalltalk 7"!

OFCAxisLabels subclass: #OFCSpokeLabels
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCSpokeLabels guid: (GUID fromString: '{ac20d609-283a-413a-89fa-7ca8e9fe71a9}')!
OFCSpokeLabels comment: ''!
!OFCSpokeLabels categoriesForClass!Unclassified! !
!OFCSpokeLabels methodsFor!

properties
	properties isNil
		ifTrue: [properties := axis properties
						at: #'spoke-labels'
						ifAbsentPut: [Dictionary new]].
	^ properties! !
!OFCSpokeLabels categoriesFor: #properties!accessing!public! !

