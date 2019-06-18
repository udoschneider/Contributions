"Filed out from Dolphin Smalltalk 7"!

Object subclass: #XMLUtil
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
XMLUtil guid: (GUID fromString: '{c234f5e7-c472-42ba-b139-1fa529ccafb6}')!
XMLUtil comment: 'This class provides utility methods for working with XML.'!
!XMLUtil categoriesForClass!Unclassified! !
!XMLUtil class methodsFor!

defaultEncoding	"answer the default character encoding"		^ 'UTF-8'!

escape: aString 	"answer a version of aString where special characters in XML text are escaped"		| result |	result := aString.		"The next line was suggested by Bert Freudenberg.	It is essential that ampersands are processed first	so other entity references don't have their & escaped."	#($& '&amp;' $< '&lt;' $> '&gt;' $' '&apos;' $" '&quot;' )		pairsDo: [:char :entity | result := result copyReplaceAll: (char asString) with: entity].			^result!

flag: aSymbol 
	!

isComment: aString 	"answer whether aString is a valid XML comment"		(aString = nil) ifTrue: [ IllegalArgumentException signal: '"nil" is an invalid comment' ].	^(aString includesSubString: '--') not!

isName: aString 	"answer whether aString is a valid XML name"		aString ifNil: [^ false].	(aString asLowercase beginsWith: 'xml') ifTrue: [^ false].	^aString matchesRegex: self latinNameRegex!

isURI: aString 
	"answer whether aString is a URI"

	| alpha authority digit fragment hierPart pathAbempty query scheme uri |
	self flag: #todo.	"This isn't working yet."
	"^URI fromString: aString"

	"See http://gbiv.com/protocols/uri/rfc/rfc3986.html#generic-syntax."
	alpha := ''.
	authority := ''.
	digit := ''.
	fragment := ''.
	pathAbempty := ''.
	hierPart := '//' , authority , pathAbempty.
	query := ''.
	scheme := alpha , '(' , alpha , '|' , digit , '|\+|-|\.)*'.
	uri := scheme , ':' , hierPart , '(' , query , ')?(#' , fragment , ')?'.
	^true!

isVersion: aSymbol	"answer whether aString is a valid XML version"	^#(#'1.0' #'1.1' #'1.2') includes: aSymbol!

latinNameRegex	"answer the regular expression for latin XML names"	"Why can't '\.' be used in place of '.' in the regex below!!"		^'^[A-Za-z:=][A-Za-z0-9\-:=.]*$'!

verifyComment: aString	"verify that aString is a valid XML comment and signals if not"	(XMLUtil isComment: aString) ifFalse: [		IllegalArgumentException signal: '"', aString, ' is an invalid XML comment'	]!

verifyName: aString	"verify that aString is a valid XML comment and signals if not"	(XMLUtil isName: aString) ifFalse: [		IllegalArgumentException signal: '"', aString, '" is an invalid XML name'	]!

verifyURI: aString	"verify that aString is a valid URI and signals if not"	(XMLUtil isURI: aString) ifFalse: [		IllegalArgumentException signal: '"', aString, ' is an invalid URI'	]!

verifyVersion: aSymbol	"verify that aSymbol is a valid XML version and signals if not"	(XMLUtil isVersion: aSymbol) ifFalse: [		IllegalArgumentException signal: '"', aSymbol asString, ' is an invalid XML version'	]!

xmlSchemaInstanceNS	"answer the namespace URI for XML Schema instances"	^'http://www.w3.org/1999/XMLSchema-instance'! !
!XMLUtil class categoriesFor: #defaultEncoding!constants!public! !
!XMLUtil class categoriesFor: #escape:!public!transforming! !
!XMLUtil class categoriesFor: #flag:!public! !
!XMLUtil class categoriesFor: #isComment:!public!testing! !
!XMLUtil class categoriesFor: #isName:!public!testing! !
!XMLUtil class categoriesFor: #isURI:!public!testing! !
!XMLUtil class categoriesFor: #isVersion:!public!testing! !
!XMLUtil class categoriesFor: #latinNameRegex!private! !
!XMLUtil class categoriesFor: #verifyComment:!public!testing! !
!XMLUtil class categoriesFor: #verifyName:!public!testing! !
!XMLUtil class categoriesFor: #verifyURI:!public!testing! !
!XMLUtil class categoriesFor: #verifyVersion:!public!testing! !
!XMLUtil class categoriesFor: #xmlSchemaInstanceNS!constants!public! !

