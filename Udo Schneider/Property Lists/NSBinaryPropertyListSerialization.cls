"Filed out from Dolphin Smalltalk 7"!

NSPropertyListSerialization subclass: #NSBinaryPropertyListSerialization
	instanceVariableNames: 'data header trailer offsetTable'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
NSBinaryPropertyListSerialization guid: (GUID fromString: '{5ed2e2a4-ab9c-4dd8-a3fd-cd1591a44493}')!
NSBinaryPropertyListSerialization comment: ''!
!NSBinaryPropertyListSerialization categoriesForClass!Unclassified! !
!NSBinaryPropertyListSerialization methodsFor!

objectIsOversized: objectType 
	^(objectType bitAnd: 16rF) = 15!

parse
	|topObject |
	topObject := trailer topObject.
	^self parse: topObject!

parse: anObjectIndex 
	| objectOffset objectType |
	objectOffset := offsetTable at: anObjectIndex + 1.
	objectType := data byteAtOffset: objectOffset.
	objectType = kCFBinaryPlistMarkerNull ifTrue: [^nil].
	objectType = kCFBinaryPlistMarkerFalse ifTrue: [^false].
	objectType = kCFBinaryPlistMarkerTrue ifTrue: [^true].
	(objectType bitAnd: 16rF0) = kCFBinaryPlistMarkerInt 
		ifTrue: [^self parseIntegerAt: objectOffset].
	(objectType bitAnd: 16rF0) = kCFBinaryPlistMarkerReal 
		ifTrue: [^self parseRealAt: objectOffset].
	objectType = kCFBinaryPlistMarkerDate ifTrue: [^self parseDateAt: objectOffset].
	(objectType bitAnd: 16rF0) = kCFBinaryPlistMarkerData 
		ifTrue: [^self parseDataAt: objectOffset].
	(objectType bitAnd: 16rF0) = kCFBinaryPlistMarkerASCIIString 
		ifTrue: [^self parseStringAt: objectOffset].
	(objectType bitAnd: 16rF0) = kCFBinaryPlistMarkerUnicode16String 
		ifTrue: [^self parseUnicodeStringAt: objectOffset].
	(objectType bitAnd: 16rF0) = kCFBinaryPlistMarkerArray 
		ifTrue: [^self parseArrayAt: objectOffset].

	(objectType bitAnd: 16rF0) = kCFBinaryPlistMarkerDict 
		ifTrue: [^self parseDictionaryAt: objectOffset].




	self error: 'Unknown data type'!

parseArrayAt: objectOffset 
	| objectType length offset array |
	objectType := data byteAtOffset: objectOffset.
	(self objectIsOversized: objectType) 
		ifFalse: 
			[offset := 1.
			length := self parseLengthAt: objectOffset]
		ifTrue: 
			[length := self parseIntegerAt: objectOffset + 1.
			offset := (self parsePower2LengthAt: objectOffset + 1) + 2].
	array := data 
				arraySized: trailer objectRefSize
				atOffset: objectOffset + offset
				length: length.


	^array asArray collect: [:each | self parse: each]!

parseDataAt: objectOffset 
	| objectType length offset |
	objectType := data byteAtOffset: objectOffset.
	(self objectIsOversized: objectType) 
		ifFalse: 
			[offset := 1.
			length := self parseLengthAt: objectOffset]
		ifTrue: 
			[length := self parseIntegerAt: objectOffset + 1.
			offset := (self parsePower2LengthAt: objectOffset + 1) + 2].
	^data copyFromOffset: objectOffset + offset length: length!

parseDateAt: objectOffset 
	^TimeStamp fromCoreFoundationDouble: (data bigEndianDoubleAtOffset: objectOffset+1)!

parseDictionaryAt: objectOffset 
	| objectType length offset array dictionary |
	objectType := data byteAtOffset: objectOffset.
	(self objectIsOversized: objectType) 
		ifFalse: 
			[offset := 1.
			length := self parseLengthAt: objectOffset]
		ifTrue: 
			[length := self parseIntegerAt: objectOffset + 1.
			offset := (self parsePower2LengthAt: objectOffset + 1) + 2].
	
	array := data 
				arraySized: trailer objectRefSize
				atOffset: objectOffset + offset
				length: length * 2.
	dictionary := Dictionary new.
	1 to: length
		do: 
			[:index | 
			dictionary at: (self parse: (array at: index))
				put: (self parse: (array at: index + length))].
	^dictionary!

parseIntegerAt: objectOffset 
	| length |
	length := self parsePower2LengthAt: objectOffset.
	length = 1 ifTrue: [^data byteAtOffset: objectOffset +1].
	length = 2 ifTrue: [^data bigEndianWordAtOffset: objectOffset+1].
	length = 4 ifTrue: [^data bigEndianDwordAtOffset: objectOffset+1].
	length = 8 ifTrue: [^data bigEndianQwordAtOffset: objectOffset+1].
	self error: 'Unknown Integer format'!

parseLengthAt: objectOffset 
	^(data byteAtOffset: objectOffset) bitAnd: 16r0F!

parsePower2LengthAt: objectOffset 
	^2 raisedTo: (self parseLengthAt: objectOffset )!

parseRealAt: objectOffset 
	| length |
	length := self parsePower2LengthAt: objectOffset.
	length = 4 ifTrue: [^data bigEndianFloatAtOffset: objectOffset +1].
	length = 8 ifTrue: [^data bigEndianDoubleAtOffset: objectOffset+1 ].
	self error: 'Unknown Float format'!

parseStringAt: objectOffset 	^(self parseDataAt: objectOffset ) asString!

parseUnicodeStringAt: objectOffset
	^(self parseStringAt: objectOffset) asUtf16String!

setData: aByteArray header: aCFBinaryPlistHeader trailer: aCFBinaryPlistTrailer 
	| array |
	data := aByteArray.
	header := aCFBinaryPlistHeader.
	trailer := aCFBinaryPlistTrailer.
	offsetTable := data 
				arraySized: trailer offsetIntSize
				atOffset: trailer offsetTableOffset
				length: trailer numObjects

	! !
!NSBinaryPropertyListSerialization categoriesFor: #objectIsOversized:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parse!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parse:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parseArrayAt:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parseDataAt:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parseDateAt:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parseDictionaryAt:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parseIntegerAt:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parseLengthAt:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parsePower2LengthAt:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parseRealAt:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parseStringAt:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parseUnicodeStringAt:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #setData:header:trailer:!public! !

!NSBinaryPropertyListSerialization class methodsFor!

propertyListFromData: aByteArray 
	| header trailer |
	header := CFBinaryPlistHeader fromBytes: (aByteArray first: CFBinaryPlistHeader byteSize).
	trailer := CFBinaryPlistTrailer 
				fromBytes: (aByteArray last: CFBinaryPlistTrailer byteSize).
	header magic ~= 'bplist' ifTrue: [self errorNotAPropertyList].
	header version ~= '00' ifTrue: [self errorWrongVersion].
	^(self new)
		setData: aByteArray
			header: header
			trailer: trailer;
		parse! !
!NSBinaryPropertyListSerialization class categoriesFor: #propertyListFromData:!public! !

