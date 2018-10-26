"Filed out from Dolphin Smalltalk 7"!

Object subclass: #WindowsResourceIdentifier
	instanceVariableNames: 'type id locale'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WindowsResourceIdentifier guid: (GUID fromString: '{d77a89e0-57ef-4c1b-b9b7-104fe015a8da}')!
WindowsResourceIdentifier comment: ''!
!WindowsResourceIdentifier categoriesForClass!Unclassified! !
!WindowsResourceIdentifier methodsFor!

id
	^id!

id: anObject
	id := anObject!

locale
	^locale!

locale: anObject
	locale := anObject!

setType: aResourceType
		id: aResourceId
		locale: aLocale
		type := aResourceType.
		id := aResourceId.
		locale := aLocale!

type
	^type!

type: anObject
	type := anObject! !
!WindowsResourceIdentifier categoriesFor: #id!accessing!public! !
!WindowsResourceIdentifier categoriesFor: #id:!accessing!private! !
!WindowsResourceIdentifier categoriesFor: #locale!accessing!public! !
!WindowsResourceIdentifier categoriesFor: #locale:!accessing!private! !
!WindowsResourceIdentifier categoriesFor: #setType:id:locale:!public! !
!WindowsResourceIdentifier categoriesFor: #type!accessing!public! !
!WindowsResourceIdentifier categoriesFor: #type:!accessing!private! !

!WindowsResourceIdentifier class methodsFor!

new
"Use #type:id:locale: instead"
	self shouldNotImplement!

type: aResourceType id: aResourceId locale: aLocale 
	^super new setType: aResourceType id: aResourceId locale: aLocale ! !
!WindowsResourceIdentifier class categoriesFor: #new!public! !
!WindowsResourceIdentifier class categoriesFor: #type:id:locale:!public! !

