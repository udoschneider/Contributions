"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #JsonTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JsonTestCase guid: (GUID fromString: '{d9f1cdc7-dab5-42e8-b941-e382b36b3c56}')!
JsonTestCase comment: ''!
!JsonTestCase categoriesForClass!Unclassified! !
!JsonTestCase methodsFor!

json: aString equals: aValue 
	| jsonValue |
	jsonValue := Object fromJsonString: aString.
	self assert: jsonValue = aValue!

render: anObject equals: aString 
	| smalltalkString |
	smalltalkString := anObject jsonString.
	self assert: smalltalkString = aString!

testReadArray
self json: '[]' equals: #().
self json: ' [ ] ' equals: #().
self json: ' [ [ [ ] [ ] ] [ ] ]' equals: #( #( #() #() ) #() ).
self json: '[ 123 "Hello" true false null]' equals: (Array with: 123 with: 'Hello' with: true with: false with: nil)!

testReadAtomicValues

self json: 'true' equals: true.
self json: ' true ' equals: true.
self json: 'false' equals: false.
self json: ' false ' equals: false.
self json: 'null' equals: nil.
self json: ' null ' equals: nil!

testReadNumber
	self json: '123' equals: 123.
	self json: '-123' equals: -123.
	self json: '1.23' equals: 1.23.
	self json: '-1.23' equals: -1.23.
	self json: '123e456' equals: 123e456.
	self json: '-123e-456' equals: -123e-456!

testReadObject

self json: '{}' equals: Dictionary new.
self json: ' { } ' equals: Dictionary new.
self json: '{ "a" : "b" , true : false } ' equals: (Dictionary new at: 'a' put: 'b'; at: true put: false; yourself)!

testReadString
	self json: '"Hello World"' equals: 'Hello World'.
	self json: '"Quote \""' equals: 'Quote "'.
	self json: '"Reverse Solidus \\"' equals: 'Reverse Solidus \'.
	self json: '"Solidus \/"' equals: 'Solidus /'.
	self json: '"Backspace \b"' equals: 'Backspace ' , Character backspace asString.
	
	self json: '"Formfeed \f"' equals: 'Formfeed ' , Character ff asString.
	self json: '"Newline \n"' equals: 'Newline ' , Character nl asString.
	self json: '"Carriage Return \r"' equals: 'Carriage Return ' , Character cr asString.
	self json: '"Tab \t"' equals: 'Tab ' , Character tab asString.!

testWriteArray
	self render: (Array 
				with: 123
				with: 'Hello'
				with: true
				with: false
				with: nil)
		equals: '[123,"Hello",true,false,null]'!

testWriteAtomicValues
	self render: true equals: 'true'.
	self render: false equals: 'false'.
	self render: nil equals: 'null'!

testWriteNumber
	self render: 123 equals: '123'.
	self render: -123 equals: '-123'.
	self render: 1.23 equals: '1.23'.
	self render: -1.23 equals: '-1.23'.
!

testWriteObject
self render: ((Dictionary new)
				at: 'a' put: 'b';
				at: true put: false;
				yourself) equals: '{"a":"b",true:false}'!

testWriteString
	| stream |
	stream := ReadWriteStream on: String new.
	stream
		nextPutAll: 'Hello';
		nextPut: $";
		nextPut: $\;
		nextPut: $/;
		nextPut: Character backspace;
		nextPut: Character ff;
		nextPut: Character nl;
		nextPut: Character cr;
		nextPut: Character tab;
		nextPutAll: 'World'.
	self render: stream contents equals: '"Hello\"\\/\b\f\n\r\tWorld"'! !
!JsonTestCase categoriesFor: #json:equals:!public! !
!JsonTestCase categoriesFor: #render:equals:!public! !
!JsonTestCase categoriesFor: #testReadArray!public! !
!JsonTestCase categoriesFor: #testReadAtomicValues!public! !
!JsonTestCase categoriesFor: #testReadNumber!public! !
!JsonTestCase categoriesFor: #testReadObject!public! !
!JsonTestCase categoriesFor: #testReadString!public! !
!JsonTestCase categoriesFor: #testWriteArray!public! !
!JsonTestCase categoriesFor: #testWriteAtomicValues!public! !
!JsonTestCase categoriesFor: #testWriteNumber!public! !
!JsonTestCase categoriesFor: #testWriteObject!public! !
!JsonTestCase categoriesFor: #testWriteString!public! !

