"Filed out from Dolphin Smalltalk 7"!

Object subclass: #NSPropertyListSerialization
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'PropertyListConstants'
	classInstanceVariableNames: ''!
NSPropertyListSerialization guid: (GUID fromString: '{cfb55ee4-bc84-449e-bb81-cae69f0b1bf5}')!
NSPropertyListSerialization comment: ''!
!NSPropertyListSerialization categoriesForClass!Unclassified! !
!NSPropertyListSerialization class methodsFor!

errorNotAPropertyList
	self error: 'This is not an plist!!'!

errorWrongVersion
	self error: 'Unknown plist version'!

propertyListFromData: aString format: aFormat 


	aFormat = NSPropertyListXMLFormat_v1_0 
		ifTrue: [^NSXMLPropertyListSerialization propertyListFromData: aString].
	aFormat = NSPropertyListBinaryFormat_v1_0 
		ifTrue: [^NSBinaryPropertyListSerialization propertyListFromData: aString].
	self error: 'Unknown plist format'! !
!NSPropertyListSerialization class categoriesFor: #errorNotAPropertyList!public! !
!NSPropertyListSerialization class categoriesFor: #errorWrongVersion!public! !
!NSPropertyListSerialization class categoriesFor: #propertyListFromData:format:!public! !

