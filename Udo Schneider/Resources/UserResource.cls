"Filed out from Dolphin Smalltalk 7"!

BinaryResource subclass: #UserResource
	instanceVariableNames: 'type'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
UserResource guid: (GUID fromString: '{8a7cba4a-8635-416b-8c09-a5d98aad658a}')!
UserResource comment: ''!
!UserResource categoriesForClass!Unclassified! !
!UserResource methodsFor!

resourceType
	^self type!

setIdentifier: aResourceId locale: aLocale bytes: aByteArray 
^self setType: self class resourceType identifier: aResourceId locale: aLocale bytes: aByteArray !

setType: aResourceType identifier: aResourceId locale: aLocale bytes: aByteArray 
	type := aResourceType.
	identifier := aResourceId.
	locale := aLocale.
	bytes := aByteArray!

type
	^type!

type: anObject 

	type := anObject! !
!UserResource categoriesFor: #resourceType!public! !
!UserResource categoriesFor: #setIdentifier:locale:bytes:!private! !
!UserResource categoriesFor: #setType:identifier:locale:bytes:!private! !
!UserResource categoriesFor: #type!accessing!public! !
!UserResource categoriesFor: #type:!*-in class package!accessing!public! !

