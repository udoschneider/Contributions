﻿"Filed out from Dolphin Smalltalk 7"!

Object subclass: #WAX
	instanceVariableNames: 'attrOnNewLine checkMe closeStream defaultNSOnCurrentElement doctypePublicId doctypeSystemId dtdSpecified encoding entityDefs escape hasContent hasIndentedContent inCommentedStart indent lineSeparator namespaceURIToSchemaPathMap outputStarted parentStack pendingPrefixes prefixesStack spaceInEmptyElements state stream xsltSpecified'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WAX guid: (GUID fromString: '{430e050e-df44-4e24-b953-6e8eb22d14d8}')!
WAX comment: 'This is the main class of the Writing API for XML (WAX) implementation.'!
!WAX categoriesForClass!Unclassified! !
!WAX methodsFor!

allSpaces: aString

attr: aName value: aValue 

badState: aMethodName

blankLine

cdata: aString 

cdata: aString newline: aBoolean

child: anElementName text: aString

close

comment: aString 

comment: aString newline: aBoolean

commentedStart: anElementName 

defaultNamespace: aURI

defaultNamespace: aURI schemaPath: aPath

dtd: systemId

dtd: publicId systemId: systemId

end

end: verbose

entityDef: aName value: aString

externalEntityDef: aName filePath: aFilePath

indent

indentChars: aStringOrChar 
	"Sets the indentation characters to use.

	" causes elements to be output on separate lines, but not indented.

	| string |
	string := aStringOrChar asString.
	checkMe 
		ifTrue: 
			[| valid |
			valid := string isEmptyOrNil or: [(aStringOrChar = Character tab) or: [self allSpaces: string]].
			(valid not or: [string notNil and: [string size > 4]]) 
				ifTrue: [IllegalArgumentException signal: 'invalid indent value']].
	indent := string!

indentSize: anInteger

initialize

isInScopePrefix: aPrefix

isSpaceInEmptyElements

lineSeparator

lineSeparator: aLineSeparator

noIndentsOrLineSeparators

pi: target data: data

prefix: aPrefix attr: aName value: aValue 

prefix: aPrefix attr: aName value: aValue newline: aBoolean 
	"add an attribute to the current element"

	| hasPrefix qName value |
	state = #InStartTag ifFalse: [self badState: 'attr'].
	hasPrefix := aPrefix ~= nil and: [aPrefix size > 0].
	checkMe 
		ifTrue: 
			[hasPrefix 
				ifTrue: 
					[XMLUtil verifyName: aPrefix.
					pendingPrefixes add: aPrefix].
			XMLUtil verifyName: aName].
	qName := hasPrefix ifFalse: [aName] ifTrue: [aPrefix , ':' , aName]  .
	aBoolean ifTrue: [self writeIndent] ifFalse: [self write: ' '].
	value := escape ifTrue: [XMLUtil escape: aValue] ifFalse: [aValue].
	self write: qName , '="' , value , '"'!

prefix: aPrefix child: anElementName text: aString

prefix: aPrefix commentedStart: anElementName 

prefix: aPrefix namespace: aURI

prefix: aPrefix namespace: aURI schemaPath: aPath

prefix: aPrefix ns: aURI

prefix: aPrefix ns: aURI schemaPath: aPath

prefix: aPrefix start: anElementName 

prefix: aPrefix unescapedAttr: aName value: aValue

prefix: aPrefix unescapedAttr: aName value: aValue newline: aBoolean

processingInstruction: target data: data

spaceInEmptyElements: aBoolean

start: anElementName 

stream

stream: aStream

terminateStart

text: aString 

text: aString newline: newline

trustMe

trustMe: aBoolean

unescapedAttr: aName value: aValue

unescapedText: text

unescapedText: text newline: aBoolean

verifyPrefixes

willIndent

write: aString 

writeDocType: rootElementName 

writeIndent

writeSchemaLocations
	"Private - write the namespace declaration for the XMLSchema-instance namespace

	| nsStream |
	
	namespaceURIToSchemaPathMap ifEmpty: [^self].

	"Write the attributes needed to associate XML Schemas with this XML."
	nsStream := WriteStream on: String new.
	namespaceURIToSchemaPathMap keysAndValuesDo: 
			[:uri :path | 
			"If not the first pair output ..."
			nsStream isEmpty 
				ifFalse: 
					[self willIndent 
						ifTrue: 
							[nsStream nextPutAll: lineSeparator.
							parentStack size + 1 timesRepeat: [nsStream nextPutAll: indent]]
						ifFalse: [nsStream nextPut: Character space]].
			nsStream nextPutAll: uri , ' ' , path].
	self
		prefix: 'xsi' namespace: XMLUtil xmlSchemaInstanceNS;
		prefix: 'xsi'
			attr: 'schemaLocation'
			value: nsStream contents
			newline: self willIndent.
	attrOnNewLine := true.	"for the next attribute"
	namespaceURIToSchemaPathMap := Dictionary new!

writeXMLDeclaration: aVersion 

xslt: filePath
!WAX categoriesFor: #allSpaces:!private! !
!WAX categoriesFor: #attr:value:!public!writing! !
!WAX categoriesFor: #badState:!error/handling!public! !
!WAX categoriesFor: #blankLine!public!writing! !
!WAX categoriesFor: #cdata:!public!writing! !
!WAX categoriesFor: #cdata:newline:!public!writing! !
!WAX categoriesFor: #child:text:!public!writing! !
!WAX categoriesFor: #close!public!writing! !
!WAX categoriesFor: #comment:!public!writing! !
!WAX categoriesFor: #comment:newline:!public!writing! !
!WAX categoriesFor: #commentedStart:!public!writing! !
!WAX categoriesFor: #defaultNamespace:!public!writing! !
!WAX categoriesFor: #defaultNamespace:schemaPath:!public!writing! !
!WAX categoriesFor: #dtd:!public!writing! !
!WAX categoriesFor: #dtd:systemId:!public!writing! !
!WAX categoriesFor: #end!public!writing! !
!WAX categoriesFor: #end:!public!writing! !
!WAX categoriesFor: #entityDef:value:!public!writing! !
!WAX categoriesFor: #externalEntityDef:filePath:!public!writing! !
!WAX categoriesFor: #indent!configuring!public! !
!WAX categoriesFor: #indentChars:!configuring!public! !
!WAX categoriesFor: #indentSize:!configuring!public! !
!WAX categoriesFor: #initialize!initialize/release!public! !
!WAX categoriesFor: #isInScopePrefix:!private! !
!WAX categoriesFor: #isSpaceInEmptyElements!private! !
!WAX categoriesFor: #lineSeparator!accessing!public! !
!WAX categoriesFor: #lineSeparator:!configuring!public! !
!WAX categoriesFor: #noIndentsOrLineSeparators!configuring!public! !
!WAX categoriesFor: #pi:data:!public!writing! !
!WAX categoriesFor: #prefix:attr:value:!public!writing! !
!WAX categoriesFor: #prefix:attr:value:newline:!public!writing! !
!WAX categoriesFor: #prefix:child:text:!public!writing! !
!WAX categoriesFor: #prefix:commentedStart:!public!writing! !
!WAX categoriesFor: #prefix:namespace:!public!writing! !
!WAX categoriesFor: #prefix:namespace:schemaPath:!public!writing! !
!WAX categoriesFor: #prefix:ns:!public!writing! !
!WAX categoriesFor: #prefix:ns:schemaPath:!public!writing! !
!WAX categoriesFor: #prefix:start:!public!writing! !
!WAX categoriesFor: #prefix:unescapedAttr:value:!public!writing! !
!WAX categoriesFor: #prefix:unescapedAttr:value:newline:!public!writing! !
!WAX categoriesFor: #processingInstruction:data:!public!writing! !
!WAX categoriesFor: #spaceInEmptyElements:!private! !
!WAX categoriesFor: #start:!public!writing! !
!WAX categoriesFor: #stream!accessing!public! !
!WAX categoriesFor: #stream:!accessing!public! !
!WAX categoriesFor: #terminateStart!private! !
!WAX categoriesFor: #text:!public!writing! !
!WAX categoriesFor: #text:newline:!public!writing! !
!WAX categoriesFor: #trustMe!configuring!public! !
!WAX categoriesFor: #trustMe:!configuring!public! !
!WAX categoriesFor: #unescapedAttr:value:!public!writing! !
!WAX categoriesFor: #unescapedText:!public!writing! !
!WAX categoriesFor: #unescapedText:newline:!public!writing! !
!WAX categoriesFor: #verifyPrefixes!private! !
!WAX categoriesFor: #willIndent!private! !
!WAX categoriesFor: #write:!private! !
!WAX categoriesFor: #writeDocType:!private! !
!WAX categoriesFor: #writeIndent!private! !
!WAX categoriesFor: #writeSchemaLocations!private! !
!WAX categoriesFor: #writeXMLDeclaration:!private! !
!WAX categoriesFor: #xslt:!public!writing! !

!WAX class methodsFor!

macLineSeparator
	^String with: Character nl!

new
^super new initialize!

stream: aStream

stream: aStream version: aSymbol

unixLineSeparator
^String with: Character nl!

windowsLineSeparator
	^String with: Character cr with: Character nl! !
!WAX class categoriesFor: #macLineSeparator!accessing!public! !
!WAX class categoriesFor: #new!public! !
!WAX class categoriesFor: #stream:!instance creation!public! !
!WAX class categoriesFor: #stream:version:!instance creation!public! !
!WAX class categoriesFor: #unixLineSeparator!accessing!public! !
!WAX class categoriesFor: #windowsLineSeparator!accessing!public! !
