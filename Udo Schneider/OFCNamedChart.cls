"Filed out from Dolphin Smalltalk 7"!

OFCChart subclass: #OFCNamedChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCNamedChart guid: (GUID fromString: '{d1ae6709-fb81-4220-90a9-36ea42275625}')!
OFCNamedChart comment: ''!
!OFCNamedChart categoriesForClass!Unclassified! !
!OFCNamedChart methodsFor!

name: aString
	self propertiesAt: #text put: aString!

nameSize: aNumber 
	self 
		propertiesAt: #'font-size'
		put: aNumber! !
!OFCNamedChart categoriesFor: #name:!accessing!public! !
!OFCNamedChart categoriesFor: #nameSize:!accessing!public! !

