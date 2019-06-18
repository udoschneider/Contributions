"Filed out from Dolphin Smalltalk 7"!

OFCAxis subclass: #OFCRadarAxis
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCRadarAxis guid: (GUID fromString: '{0372d842-2039-4d09-b4d0-f38fd52351ba}')!
OFCRadarAxis comment: ''!
!OFCRadarAxis categoriesForClass!Unclassified! !
!OFCRadarAxis methodsFor!

elementType
	^ #'radar_axis'!

spokeLabels
	^ OFCSpokeLabels new
		setOFCCanvas: ofcCanvas
			axis: self;
		yourself!

spokeLabels: aBlock 
	^ self spokeLabels
		with: aBlock;
		yourself! !
!OFCRadarAxis categoriesFor: #elementType!constants!public! !
!OFCRadarAxis categoriesFor: #spokeLabels!accessing!public! !
!OFCRadarAxis categoriesFor: #spokeLabels:!accessing!public! !

