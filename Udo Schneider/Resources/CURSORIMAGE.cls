"Filed out from Dolphin Smalltalk 7"!

ExternalStructure subclass: #CURSORIMAGE
	instanceVariableNames: ''
	classVariableNames: '_OffsetOf_icAND _OffsetOf_icColors _OffsetOf_icHeader _OffsetOf_icXOR _OffsetOf_xHotSpot _OffsetOf_yHotSpot'
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
CURSORIMAGE guid: (GUID fromString: '{32d499b3-520d-4226-9dc9-ee5e11c5cf5c}')!
CURSORIMAGE addClassConstant: '_OffsetOf_icAND' value: 16r34!
CURSORIMAGE addClassConstant: '_OffsetOf_icColors' value: 16r2C!
CURSORIMAGE addClassConstant: '_OffsetOf_icHeader' value: 16r4!
CURSORIMAGE addClassConstant: '_OffsetOf_icXOR' value: 16r30!
CURSORIMAGE addClassConstant: '_OffsetOf_xHotSpot' value: 16r0!
CURSORIMAGE addClassConstant: '_OffsetOf_yHotSpot' value: 16r2!
CURSORIMAGE comment: ''!
!CURSORIMAGE categoriesForClass!Unclassified! !
!CURSORIMAGE methodsFor!

asIcon
	| handle |
	#USToDo.
	"Switch to CreateIcon() here. This will allow more flexible mask settings where mask,dib and colors are references and not part of the bytes"
	handle := UserLibrary default 
				createIconFromResource: bytes
				dwResSize: bytes size
				fIcon: true
				dwVer: 16r00030000.
	^Icon fromOwnedHandle: handle!

icAND
	| start size |
	start := self icHeader byteSize + self icColors  byteSize + self icXOR byteSize.
	size := (self widthInBytes: self icHeader biWidth) * (self icHeader biHeight / 2).
	^bytes copyFrom: start + 1 to: start + size!

icANDDIB
	| infoHeader info hBitmap |
	infoHeader := self icHeader copy.
	infoHeader
		biHeight: infoHeader biHeight / 2;
		biBitCount: 1.
	info := BITMAPINFO colorDepth: 1.
	info bmiHeader: infoHeader.
	info bmiColors at: 1 put: (RGBQUAD fromColor: Color white). #USToDo. "Which color is white? 0 or 1?"
	hBitmap := GDILibrary default 
				createDIBitmap: UserLibrary default getDC
				lpbmih: infoHeader
				fdwInit: CBM_INIT
				lpbInit: self icAND asParameter
				lpbmi: info
				fuUsage: DIB_RGB_COLORS.
	^DIBSection fromOwnedHandle: hBitmap!

icColors
	"Answer the <StructureArray> value of the receiver's 'icColors' field."

	^StructureArray
		fromAddress: bytes yourAddress + _OffsetOf_icColors
		length: self numColors
		elementClass: RGBQUAD!

icColors: aStructureArrayOfRGBQUAD
	"Set the receiver's 'icColors' field to the value of the argument, aStructureArrayOfRGBQUAD"

	| size |
	size := aStructureArrayOfRGBQUAD byteSize.
	aStructureArrayOfRGBQUAD
		replaceBytesOf: bytes
		from: ##(_OffsetOf_icColors + 1)
		to: _OffsetOf_icColors + size
		startingAt: 1!

icHeader
	"Answer the <BITMAPINFOHEADER> value of the receiver's 'icHeader' field."

	^BITMAPINFOHEADER fromAddress: bytes yourAddress + _OffsetOf_icHeader!

icHeader: aBITMAPINFOHEADER
	"Set the receiver's 'icHeader' field to the value of the argument, aBITMAPINFOHEADER"

	aBITMAPINFOHEADER
		replaceBytesOf: bytes
		from: ##(_OffsetOf_icHeader + 1)
		to: ##(_OffsetOf_icHeader + BITMAPINFOHEADER basicByteSize)
		startingAt: 1!

icXOR
	| start size |
	start := self icHeader byteSize + self icColors byteSize.
	size := (self widthInBytes: self icHeader biWidth * self icHeader biBitCount) 
				* (self icHeader biHeight / 2).
	"size := self icHeader biSizeImage."
	^bytes copyFrom: start + 1 to: start + size!

icXORDIB
	| infoHeader info hBitmap |
	infoHeader := self icHeader copy.
	infoHeader biHeight: infoHeader biHeight / 2.
	info := BITMAPINFO colorDepth: infoHeader biBitCount.
	info
		bmiHeader: infoHeader.
		self numColors >0 ifTrue: [
		info bmiColors: self icColors].
	hBitmap := GDILibrary default 
				createDIBitmap: UserLibrary default getDC
				lpbmih: infoHeader
				fdwInit: CBM_INIT
				lpbInit: self icXOR asParameter
				lpbmi: info
				fuUsage: DIB_RGB_COLORS.
	^DIBSection fromOwnedHandle: hBitmap!

lengthAndMask
	| ich |
	ich := self icHeader.
	^ich biHeight * (self widthInBytes: ich biWidth)!

lengthXorMask
	| ich |
	ich := self icHeader.
	
	^ich biHeight * (self widthInBytes: ich biWidth * ich biPlanes * ich biBitCount)!

numColors
	^self icHeader biBitCount < 16 ifTrue: [2 raisedTo: self icHeader biBitCount] ifFalse: [0]!

widthInBytes: anInteger 
	"How wide, in bytes, would this many bits be, DWORD aligned?
	
	#define WIDTHBYTES(bits)      ((((bits) + 31)>>5)<<2)"
^anInteger + 31 >> 5 <<2
	!

xHotSpot
	"Answer the <Integer> value of the receiver's 'xHotSpot' field."

	^bytes wordAtOffset: _OffsetOf_xHotSpot!

xHotSpot: anInteger
	"Set the receiver's 'xHotSpot' field to the value of the argument, anInteger"

	bytes wordAtOffset: _OffsetOf_xHotSpot put: anInteger!

yHotSpot
	"Answer the <Integer> value of the receiver's 'yHotSpot' field."

	^bytes wordAtOffset: _OffsetOf_yHotSpot!

yHotSpot: anInteger
	"Set the receiver's 'yHotSpot' field to the value of the argument, anInteger"

	bytes wordAtOffset: _OffsetOf_yHotSpot put: anInteger! !
!CURSORIMAGE categoriesFor: #asIcon!*-in class package!public! !
!CURSORIMAGE categoriesFor: #icAND!*-in class package!public! !
!CURSORIMAGE categoriesFor: #icANDDIB!*-in class package!public! !
!CURSORIMAGE categoriesFor: #icColors!**compiled accessors**!public! !
!CURSORIMAGE categoriesFor: #icColors:!**compiled accessors**!public! !
!CURSORIMAGE categoriesFor: #icHeader!**compiled accessors**!public! !
!CURSORIMAGE categoriesFor: #icHeader:!**compiled accessors**!public! !
!CURSORIMAGE categoriesFor: #icXOR!*-in class package!public! !
!CURSORIMAGE categoriesFor: #icXORDIB!*-in class package!public! !
!CURSORIMAGE categoriesFor: #lengthAndMask!private! !
!CURSORIMAGE categoriesFor: #lengthXorMask!private! !
!CURSORIMAGE categoriesFor: #numColors!*-in class package!private! !
!CURSORIMAGE categoriesFor: #widthInBytes:!private! !
!CURSORIMAGE categoriesFor: #xHotSpot!**compiled accessors**!public! !
!CURSORIMAGE categoriesFor: #xHotSpot:!**compiled accessors**!public! !
!CURSORIMAGE categoriesFor: #yHotSpot!**compiled accessors**!public! !
!CURSORIMAGE categoriesFor: #yHotSpot:!**compiled accessors**!public! !

!CURSORIMAGE class methodsFor!

defineFields
	"Define the fields of the CURSORIMAGE structure.
		CURSORIMAGE compileDefinition

	typdef struct
	{
		DWORD xHotSpot;
		DWORD yHotSpot;
		BITMAPINFOHEADER   icHeader;      // DIB header
		RGBQUAD         icColors[1];   // Color table
		BYTE            icXOR[1];      // DIB bits for XOR mask
		BYTE            icAND[1];      // DIB bits for AND mask
	} CURSORIMAGE, *LPCURSORIMAGE;"

	self
		defineField: #xHotSpot type: WORDField new;
		defineField: #yHotSpot type: WORDField new;
		defineField: #icHeader type: (StructureField type: BITMAPINFOHEADER);
		defineField: #icColors
			type: (StructureArrayField type: RGBQUAD
					arrayLength: (ConformantArrayFieldLength lengthField: #numColors));
		defineField: #icXOR type: (FillerField byteSize: 4);
		defineField: #icAND type: (FillerField byteSize: 4)!

getFieldNames
	^#(#xHotSpot #yHotSpot #icHeader #icColors)!

xor: xorDIB and: andDIB 
	| header xorBytes andBytes iconBytes colorBytes |
	self assert: [xorDIB extent = andDIB extent].
	self assert: [andDIB depth = 1].
	header := xorDIB getBITMAPINFOHEADER copy.
	header biHeight: header biHeight * 2.
	xorBytes := ByteArray fromAddress: xorDIB imageBits
				length: xorDIB getInfo bmHeight * xorDIB getInfo bmWidthBytes.
	colorBytes := xorDIB getColorTable ifNil: [#[]] ifNotNil: [:value | value bytes].
	andBytes := ByteArray fromAddress: andDIB imageBits
				length: andDIB getInfo bmHeight * andDIB getInfo bmWidthBytes.
	iconBytes := header bytes , colorBytes  , xorBytes , andBytes.
	^(self new: iconBytes size) bytes: iconBytes! !
!CURSORIMAGE class categoriesFor: #defineFields!*-in class package!constants!public! !
!CURSORIMAGE class categoriesFor: #getFieldNames!**compiled accessors**!constants!private! !
!CURSORIMAGE class categoriesFor: #xor:and:!*-in class package!public! !

