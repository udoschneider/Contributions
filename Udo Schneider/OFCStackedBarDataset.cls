"Filed out from Dolphin Smalltalk 7"!

OFCObject subclass: #OFCStackedBarDataset
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCStackedBarDataset guid: (GUID fromString: '{b5dd638b-33e6-4892-a0fe-267cb97bd613}')!
OFCStackedBarDataset comment: ''!
!OFCStackedBarDataset categoriesForClass!Unclassified! !
!OFCStackedBarDataset methodsFor!

color
	^ self propertiesAtOrNil: #colour!

color: aColorOrHTMLHexString 
	self 
		propertiesAt: #colour
		put: aColorOrHTMLHexString!

key

	^ Dictionary new
		at: #colour
			put: self color;
		at: #'font-size'
			put: self size;
		at: #text
			put: self text;
		yourself!

properties
	properties isNil ifTrue: [properties := Dictionary new].
	^properties!

size
	^ self propertiesAtOrNil: #'font-size'!

size: aNumber 
	self 
		propertiesAt: #'font-size'
		put: aNumber!

text
	^ self propertiesAtOrNil: #text!

text: aString
	self propertiesAt: #text put: aString!

values
	^ self propertiesAtOrNil: #values!

values: anArray 
	self 
		propertiesAt: #values
		put: anArray! !
!OFCStackedBarDataset categoriesFor: #color!accessing!public! !
!OFCStackedBarDataset categoriesFor: #color:!accessing!public! !
!OFCStackedBarDataset categoriesFor: #key!accessing!public! !
!OFCStackedBarDataset categoriesFor: #properties!accessing!public! !
!OFCStackedBarDataset categoriesFor: #size!accessing!public! !
!OFCStackedBarDataset categoriesFor: #size:!accessing!public! !
!OFCStackedBarDataset categoriesFor: #text!accessing!public! !
!OFCStackedBarDataset categoriesFor: #text:!accessing!public! !
!OFCStackedBarDataset categoriesFor: #values!accessing!public! !
!OFCStackedBarDataset categoriesFor: #values:!accessing!public! !

!OFCStackedBarDataset class methodsFor!

icon ^ValueModel icon! !
!OFCStackedBarDataset class categoriesFor: #icon!public! !

