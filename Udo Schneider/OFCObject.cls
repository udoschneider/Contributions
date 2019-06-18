"Filed out from Dolphin Smalltalk 7"!

Object subclass: #OFCObject
	instanceVariableNames: 'properties'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCObject guid: (GUID fromString: '{a65eb67c-18bb-4105-acc7-015d372e9046}')!
OFCObject comment: ''!
!OFCObject categoriesForClass!Unclassified! !
!OFCObject methodsFor!

properties
	self subclassResponsibility!

propertiesAt: key put: value 
	^ self properties 
		at: key
		put: value!

propertiesAtOrNil: key 
	^ self properties 
		at: key
		ifAbsent: [ nil ]!

with: aBlock 
	aBlock value: self! !
!OFCObject categoriesFor: #properties!accessing!public! !
!OFCObject categoriesFor: #propertiesAt:put:!accessing!public! !
!OFCObject categoriesFor: #propertiesAtOrNil:!accessing!public! !
!OFCObject categoriesFor: #with:!public! !

!OFCObject class methodsFor!

new
^super new initialize! !
!OFCObject class categoriesFor: #new!public! !

