"Filed out from Dolphin Smalltalk 7"!

QWORDField subclass: #BigEndianQWORDField
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BigEndianQWORDField guid: (GUID fromString: '{6999153f-edcf-409f-8a89-09897811614c}')!
BigEndianQWORDField comment: ''!
!BigEndianQWORDField categoriesForClass!Unclassified! !
!BigEndianQWORDField methodsFor!

accessorStem
	"Private - Answer the receiver's 'accessorStem'."

	^'bigEndianQword'!

fieldClass
	^BigEndianQWORD!

readFrom: anExternal 
	"Private - Instantiate an object of the the type the receiver represents
	at the receiver's offset in anExternal"

	^anExternal bigEndianQwordAtOffset: offset!

writeInto: anExternal value: anObject 
	"Private - Write anObject into anExternal at the receiver's offset, and in a form
	appropriate for the receiver's External type"

	^anExternal bigEndianQwordAtOffset: offset put: anObject! !
!BigEndianQWORDField categoriesFor: #accessorStem!automatic generation!private! !
!BigEndianQWORDField categoriesFor: #fieldClass!constants!private! !
!BigEndianQWORDField categoriesFor: #readFrom:!indirect accessing!private! !
!BigEndianQWORDField categoriesFor: #writeInto:value:!indirect accessing!private! !

