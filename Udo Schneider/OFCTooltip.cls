"Filed out from Dolphin Smalltalk 7"!

OFCObject subclass: #OFCTooltip
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCTooltip guid: (GUID fromString: '{cc8e7d70-9776-4184-aa16-9f8d6fabd5e6}')!
OFCTooltip comment: ''!
!OFCTooltip categoriesForClass!Unclassified! !
!OFCTooltip methodsFor!

body
	^ self propertiesAtOrNil: #body!

body: aString
	self propertiesAt: #body put: aString!

bodyLines
	| input |
	input := self body readStream.
	^ Array
		streamContents: [:output | [input atEnd]
				whileFalse: [output
						nextPut: (input upTo: Character cr).
					input peek = Character lf
						ifTrue: [input next]]]!

jsonSaveOn: aStream 
	| tooltipStream |
	tooltipStream := ReadWriteStream on: String new.
	self title notNil 
		ifTrue: [(tooltipStream
				nextPutAll: self title).
				tooltipStream nextPut: Character lf]
		ifFalse: [tooltipStream nextPut: Character lf].
	self body notNil 
		ifTrue: 
			[self bodyLines do: [:each | tooltipStream nextPutAll: each]
				separatedBy: [tooltipStream nextPut: Character lf]]
		ifFalse: [tooltipStream nextPutAll: ''].
	tooltipStream contents jsonSaveOn: aStream!

properties
	properties isNil ifTrue: [properties := Dictionary new].
	^properties!

text: aString 
	^ self body: aString!

title
	^ self propertiesAtOrNil: #title!

title: aString
	self propertiesAt: #title put: aString!

title: title body: body 
	self title: title.
	self body: body! !
!OFCTooltip categoriesFor: #body!accessing!public! !
!OFCTooltip categoriesFor: #body:!accessing!public! !
!OFCTooltip categoriesFor: #bodyLines!JSON-writing!public! !
!OFCTooltip categoriesFor: #jsonSaveOn:!JSON-writing!public! !
!OFCTooltip categoriesFor: #properties!accessing!public! !
!OFCTooltip categoriesFor: #text:!accessing!public! !
!OFCTooltip categoriesFor: #title!accessing!public! !
!OFCTooltip categoriesFor: #title:!accessing!public! !
!OFCTooltip categoriesFor: #title:body:!accessing!public! !

!OFCTooltip class methodsFor!

icon
^Tooltip icon! !
!OFCTooltip class categoriesFor: #icon!public! !

