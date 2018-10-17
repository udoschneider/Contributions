"Filed out from Dolphin Smalltalk 7"!

NSPropertyListSerialization subclass: #NSXMLPropertyListSerialization
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
NSXMLPropertyListSerialization guid: (GUID fromString: '{94a9d591-fe44-43e5-b926-0742ff909538}')!
NSXMLPropertyListSerialization comment: ''!
!NSXMLPropertyListSerialization categoriesForClass!Unclassified! !
!NSXMLPropertyListSerialization methodsFor!

parse: anXMLElement 
	anXMLElement tagName = 'true' ifTrue: [^true].
	anXMLElement tagName = 'false' ifTrue: [^false].
	anXMLElement tagName = 'integer' ifTrue: [^self parseNumber: anXMLElement].
	anXMLElement tagName = 'real' ifTrue: [^self parseNumber: anXMLElement].
	anXMLElement tagName = 'data' ifTrue: [^self parseData: anXMLElement].
	anXMLElement tagName = 'date' ifTrue: [^TimeStamp fromRfc3339String: anXMLElement text].
	anXMLElement tagName = 'string' ifTrue: [^anXMLElement text].
	anXMLElement tagName = 'array' ifTrue: [^self parseArray: anXMLElement].
	anXMLElement tagName = 'dict' ifTrue: [^self parseDictionary: anXMLElement].
	self error: 'Unknown property found (' , anXMLElement tagName , ')'!

parseArray: anXMLElement 
	^anXMLElement childNodes collect: [:eachElement | self parse: eachElement]!

parseData: anXMLElement 
	| stream |
	stream := ReadWriteStream on: ByteArray new.
	Base64Codec decodeFrom: (ReadStream on: anXMLElement text) onto: stream.
	^stream contents!

parseDictionary: anXMLElement 
	| dic |
	dic := Dictionary new.
	1 to: anXMLElement childNodes size
		by: 2
		do: 
			[:index | 
			| key value |
			key := (anXMLElement childNodes at: index) text.
			value := self parse: (anXMLElement childNodes at: index + 1).
			dic at: key put: value].
	^dic!

parseNumber: anXMLElement 
	^Number fromString: anXMLElement text! !
!NSXMLPropertyListSerialization categoriesFor: #parse:!public! !
!NSXMLPropertyListSerialization categoriesFor: #parseArray:!public! !
!NSXMLPropertyListSerialization categoriesFor: #parseData:!public! !
!NSXMLPropertyListSerialization categoriesFor: #parseDictionary:!public! !
!NSXMLPropertyListSerialization categoriesFor: #parseNumber:!public! !

!NSXMLPropertyListSerialization class methodsFor!

errorXMLParsing
	self error: 'Error parsing XML plist'!

propertyListFromData: aByteArray 
	| doc root |
	doc := (IXMLDOMDocument new)
				resolveExternals: false;
				validateOnParse: false;
				yourself.
	(doc loadXML: aByteArray asString) ifFalse: [self errorXMLParsing].
	root := doc documentElement.
	root tagName ~= 'plist' ifTrue: [self errorNotAPropertyList].
	(root getAttribute: 'version') ~= '1.0' ifTrue: [self errorWrongVersion].
	^self new parse: root firstChild! !
!NSXMLPropertyListSerialization class categoriesFor: #errorXMLParsing!public! !
!NSXMLPropertyListSerialization class categoriesFor: #propertyListFromData:!public! !

