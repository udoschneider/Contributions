"Filed out from Dolphin Smalltalk 7"!

DWORDField subclass: #BigEndianDWORDField
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BigEndianDWORDField guid: (GUID fromString: '{8ac8c978-6639-409c-8557-cb3fdb5c9ece}')!
BigEndianDWORDField comment: ''!
!BigEndianDWORDField categoriesForClass!Unclassified! !
!BigEndianDWORDField methodsFor!

accessorStem
	"Private - Answer the receiver's 'accessorStem'."

	^'bigEndianDword'!

fieldClass
	^BigEndianDWORD!

readFrom: anExternal 
	"Private - Instantiate an object of the the type the receiver represents
	at the receiver's offset in anExternal"

	^anExternal bigEndianDwordAtOffset: offset!

writeInto: anExternal value: anObject 
	"Private - Write anObject into anExternal at the receiver's offset, and in a form
	appropriate for the receiver's External type"

	^anExternal bigEndianDwordAtOffset: offset put: anObject! !
!BigEndianDWORDField categoriesFor: #accessorStem!automatic generation!private! !
!BigEndianDWORDField categoriesFor: #fieldClass!constants!private! !
!BigEndianDWORDField categoriesFor: #readFrom:!indirect accessing!private! !
!BigEndianDWORDField categoriesFor: #writeInto:value:!indirect accessing!private! !

