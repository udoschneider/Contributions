"Filed out from Dolphin Smalltalk 7"!

OFCObject subclass: #OFCBrush
	instanceVariableNames: 'ofcCanvas'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCBrush guid: (GUID fromString: '{3bd16cf5-0551-4d09-937a-2180eeddd1ee}')!
OFCBrush comment: ''!
!OFCBrush categoriesForClass!Unclassified! !
!OFCBrush methodsFor!

setOFCCanvas: anOFCCanvas 
	ofcCanvas := anOFCCanvas! !
!OFCBrush categoriesFor: #setOFCCanvas:!initialize-release!public! !

!OFCBrush class methodsFor!

icon
^Icon fromId: 'BRUSH.ICO'! !
!OFCBrush class categoriesFor: #icon!public! !

