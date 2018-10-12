"Filed out from Dolphin Smalltalk 7"!

WORDField subclass: #BigEndianWORDField
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BigEndianWORDField guid: (GUID fromString: '{50fe92b2-5199-40d4-adf3-e51ff951c5ff}')!
BigEndianWORDField comment: ''!
!BigEndianWORDField categoriesForClass!Unclassified! !
!BigEndianWORDField methodsFor!

accessorStem
	"Private - Answer the receiver's 'accessorStem'."

	^'bigEndianWord'!

fieldClass
	^BigEndianWORD!

readFrom: anExternal 
	"Private - Instantiate an object of the the type the receiver represents
	at the receiver's offset in anExternal"

	^anExternal bigEndianWordAtOffset: offset!

writeInto: anExternal value: anObject 
	"Private - Write anObject into anExternal at the receiver's offset, and in a form
	appropriate for the receiver's External type"

	^anExternal bigEndianWordAtOffset: offset put: anObject! !
!BigEndianWORDField categoriesFor: #accessorStem!automatic generation!private! !
!BigEndianWORDField categoriesFor: #fieldClass!constants!private! !
!BigEndianWORDField categoriesFor: #readFrom:!indirect accessing!private! !
!BigEndianWORDField categoriesFor: #writeInto:value:!indirect accessing!private! !

