"Filed out from Dolphin Smalltalk 7"!

OFCOutlinedBarChart subclass: #OFCSketchBarChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCSketchBarChart guid: (GUID fromString: '{bb588311-603e-4ba8-bc4d-1eafe150d0a7}')!
OFCSketchBarChart comment: ''!
!OFCSketchBarChart categoriesForClass!Unclassified! !
!OFCSketchBarChart methodsFor!

elementType
	^ #'bar_sketch'!

funFactor: anInteger 
	self 
		propertiesAt: #offset
		put: anInteger! !
!OFCSketchBarChart categoriesFor: #elementType!constants!public! !
!OFCSketchBarChart categoriesFor: #funFactor:!accessing!public! !

