"Filed out from Dolphin Smalltalk 7"!

OFCObject subclass: #OFCLegendLabel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCLegendLabel guid: (GUID fromString: '{f077972f-7bff-430b-afe6-78ea4d91ad70}')!
OFCLegendLabel comment: ''!
!OFCLegendLabel categoriesForClass!Unclassified! !
!OFCLegendLabel methodsFor!

beInvisible
	^ self visible: false!

beVisible
	^ self visible: true!

color: aColorOrHTMLHexString 
	self 
		propertiesAt: #colour
		put: aColorOrHTMLHexString!

isVisible: aBoolean 
	self 
		propertiesAt: #visible
		put: aBoolean!

jsonSaveOn: aStream 
	self properties jsonSaveOn: aStream!

properties
	properties isNil ifTrue: [properties := Dictionary new].
	^properties!

rotate: aNumber 
	self 
		propertiesAt: #rotate
		put: aNumber!

size: aNumber 
	self 
		propertiesAt: #size
		put: aNumber!

text: aString 
	self 
		propertiesAt: #text
		put: aString! !
!OFCLegendLabel categoriesFor: #beInvisible!accessing!public! !
!OFCLegendLabel categoriesFor: #beVisible!accessing!public! !
!OFCLegendLabel categoriesFor: #color:!accessing!public! !
!OFCLegendLabel categoriesFor: #isVisible:!accessing!public! !
!OFCLegendLabel categoriesFor: #jsonSaveOn:!JSON-writing!public! !
!OFCLegendLabel categoriesFor: #properties!accessing!public! !
!OFCLegendLabel categoriesFor: #rotate:!accessing!public! !
!OFCLegendLabel categoriesFor: #size:!accessing!public! !
!OFCLegendLabel categoriesFor: #text:!accessing!public! !

!OFCLegendLabel class methodsFor!

icon
^String icon! !
!OFCLegendLabel class categoriesFor: #icon!public! !

